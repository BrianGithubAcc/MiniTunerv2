(*open Z3*)
open Yojson.Basic.Util

(*open Yojson.Basic*)
open Math_parse.Exprtype
open Expr_data
open Taylor_parse.Taylortype
let parse_expr line =
  let line_trimmed = String.trim line in
  (* Debug: print the line we're about to parse *)
  (*Printf.eprintf "Attempting to parse line: %S\n" line_trimmed;*)
  let lexbuf = Lexing.from_string line_trimmed in
  match Taylor_parse.Parser.expr_option Taylor_parse.Lexer.read lexbuf with
  | Some e -> e
  | None ->
      Printf.eprintf "Parser returned None for line: %S\n" line_trimmed;
      failwith "Empty expression"

let float_to_z3_string f =
  let f = float_of_string f in
  Printf.sprintf "%.30f" f
(* 30 decimal digits *)
(* 30 decimal digits, no scientific notation *)

let q_of_numeric_string value =
  let value = String.trim value in
  try Q.of_string value
  with Invalid_argument _ ->
    let exponent_index =
      match String.index_opt value 'e' with
      | Some index -> Some index
      | None -> String.index_opt value 'E'
    in
    let mantissa, exponent =
      match exponent_index with
      | Some index ->
          ( String.sub value 0 index,
            int_of_string
              (String.sub value (index + 1) (String.length value - index - 1)) )
      | None -> (value, 0)
    in
    let whole, fraction =
      match String.split_on_char '.' mantissa with
      | [ whole; fraction ] -> (whole, fraction)
      | [ whole ] -> (whole, "")
      | _ -> invalid_arg ("Invalid numeric coefficient: " ^ value)
    in
    let numerator = Z.of_string (whole ^ fraction) in
    let denominator = Z.pow (Z.of_int 10) (String.length fraction) in
    let base = Q.make numerator denominator in
    if exponent >= 0 then
      Q.mul base (Q.of_bigint (Z.pow (Z.of_int 10) exponent))
    else Q.div base (Q.of_bigint (Z.pow (Z.of_int 10) (-exponent)))

let z3_real ctx value =
  Z3.Arithmetic.Real.mk_numeral_s ctx
    (Q.to_string (q_of_numeric_string value))

(* FPTaylor rewrites decimal constants in expression keys as reduced exact
   fractions.  The mathematical parser stores them as OCaml floats, so using
   [string_of_float] here produced incompatible keys such as [1.] instead of
   [1], and [0.5] instead of [(1/2)].  Convert the shortest decimal rendering
   back to an exact rational and use FPTaylor's key syntax. *)
let fptaylor_float_key decimal =
  let decimal =
    if String.length decimal > 0 && decimal.[String.length decimal - 1] = '.'
    then String.sub decimal 0 (String.length decimal - 1)
    else decimal
  in
  let rational = q_of_numeric_string decimal in
  if Z.equal (Q.den rational) Z.one then Z.to_string (Q.num rational)
  else
    Printf.sprintf "(%s / %s)" (Z.to_string (Q.num rational))
      (Z.to_string (Q.den rational))

(* Render an AST back into MiniTuner input syntax without reusing FPTaylor's
   internal inverse key notation.  In particular, rendering [y / x] as
   [y * 1/(x)] and parsing it again changes the rounded tree to [(y * 1) / x],
   which adds a spurious multiplication-by-one node to the local domain model.
   This source rendering preserves the original tree for nested FPTaylor
   domain checks. *)
let rec math_expr_to_source = function
  | Int value -> string_of_int value
  | Float value -> value
  | Var name -> name
  | Neg expression -> "-(" ^ math_expr_to_source expression ^ ")"
  | Add (left, right) ->
      "(" ^ math_expr_to_source left ^ " + " ^ math_expr_to_source right ^ ")"
  | Sub (left, right) ->
      "(" ^ math_expr_to_source left ^ " - " ^ math_expr_to_source right ^ ")"
  | Mul (left, right) ->
      "(" ^ math_expr_to_source left ^ " * " ^ math_expr_to_source right ^ ")"
  | Div (left, right) ->
      "(" ^ math_expr_to_source left ^ " / " ^ math_expr_to_source right ^ ")"
  | Pow (left, right) ->
      "(" ^ math_expr_to_source left ^ " ^ " ^ math_expr_to_source right ^ ")"
  | Func (name, argument) ->
      name ^ "(" ^ math_expr_to_source argument ^ ")"
  | Func2 (name, left, right) ->
      name ^ "(" ^ math_expr_to_source left ^ ", "
      ^ math_expr_to_source right ^ ")"

(* OpTuner matches FPTaylor forms back to FPCore nodes with structural AST
   equality. FPTaylor canonicalizes decimal literals (for example [1.0] to
   [1]), while the FPCore AST retains the original token. Consequently a
   decimal literal prevents its containing node and ancestors from receiving a
   matched error form. Compatibility mode reproduces that observable encoding;
   native MiniTuner continues to use normalized expression keys. *)
let rec optuner_form_matches = function
  | Int _ | Var _ -> true
  | Float _ -> false
  | Neg expression | Func (_, expression) ->
      optuner_form_matches expression
  | Add (left, right)
  | Sub (left, right)
  | Mul (left, right)
  | Div (left, right)
  | Pow (left, right)
  | Func2 (_, left, right) ->
      optuner_form_matches left && optuner_form_matches right

let filter_field_member spec_data math_func =
  List.filter
    (fun obj ->
      match obj |> member "operation" |> to_string_option with
      | Some op -> op = math_func
      | None -> false)
    spec_data

let is_tunable_function = function
  | "sin" | "cos" | "tan" | "exp" | "log" | "log1p" | "expm1" | "sqrt" -> true
  | _ -> false

let fixed_operation_cost cost_data = function
  | "atan" -> cost_data |> member "atan_fixed" |> to_float_option |> Option.value ~default:0.
  | "abs" | "fabs" -> cost_data |> member "abs_fixed" |> to_float_option |> Option.value ~default:0.
  | "pow" -> cost_data |> member "pow_fixed" |> to_float_option |> Option.value ~default:0.
  | _ -> 0.

let fixed_operation_cost_key = function
  | "atan" -> Some "atan_fixed"
  | "abs" | "fabs" -> Some "abs_fixed"
  | "pow" -> Some "pow_fixed"
  | _ -> None

let get_interval_from_string interval_string =
  if not (Config.is_quiet ()) then (
    print_endline "Interval to stirng";
    print_endline interval_string
  );
  match
    interval_string |> String.split_on_char ','
    |> List.map (fun x ->
        print_string x;
        float_of_string (String.trim x))
  with
  | [ f1; f2 ] -> (f1, f2)
  | _ -> failwith "Expected closed domain,not found"

(* Returns the first fraction it finds, recursively *)
let rec extract_fraction expr =
  match expr with
  | Q q -> Some q
  (* Handle zero-subtraction first *)
  | Sub (IntZ z, e) when Z.equal z Z.zero -> extract_fraction e
  (* Divisions *)
  | Div (numer, denom) -> (
      match (numer, denom) with
      | IntZ n, IntZ d -> Some (Q.div (Q.of_bigint n) (Q.of_bigint d))
      | Q q, IntZ _ | IntZ _, Q q | Q q, Q _ -> Some q
      | _ -> (
          match extract_fraction numer with
          | Some q -> Some q
          | None -> extract_fraction denom))
  (* Generic binary operations *)
  | Add (a, b) | Sub (a, b) | Mul (a, b) | Pow (a, b) -> (
      match extract_fraction a with
      | Some q -> Some q
      | None -> extract_fraction b)
  (* Unary operations *)
  | Func (_, e) | Rnd (_, _, _, _, _, e) | Neg e -> extract_fraction e
  | Func2 (_, a, b) -> (
      match extract_fraction a with Some _ as result -> result | None -> extract_fraction b)
  | IntZ _ -> None
  | _ -> None

let string_of_q q =
  let num = Q.num q in
  let den = Q.den q in
  Printf.sprintf "%s/%s" (Z.to_string num) (Z.to_string den)

let remove_some q_opt =
  match q_opt with Some q -> string_of_q q | None -> failwith "Couldn't"

let guidance_points = ref 4

type z3_engine = Pareto | Staircase
type search_engine = Auto | Dp | Z3

let z3_engine = ref Pareto
let search_engine = ref Auto
let z3_cost_low : Q.t option ref = ref None
let z3_cost_high : Q.t option ref = ref None

let set_guidance_points points = guidance_points := max 0 points
let set_z3_engine = function
  | "pareto" -> z3_engine := Pareto
  | "staircase" -> z3_engine := Staircase
  | name -> invalid_arg ("Unknown Z3 engine: " ^ name)

let z3_engine_name () =
  match !z3_engine with Pareto -> "pareto" | Staircase -> "staircase"

let set_search_engine = function
  | "auto" -> search_engine := Auto
  | "dp" -> search_engine := Dp
  | "z3" -> search_engine := Z3
  | name -> invalid_arg ("Unknown search engine: " ^ name)

let effective_search_engine () =
  match !search_engine with
  | Auto -> Dp
  | engine -> engine

let search_engine_name () =
  match effective_search_engine () with Dp -> "dp" | Z3 -> "z3" | Auto -> assert false

let set_z3_cost_band low high =
  z3_cost_low := Option.map q_of_numeric_string low;
  z3_cost_high := Option.map q_of_numeric_string high

type implementation_candidate = {
  index : int;
  cname : string;
  cost : float;
  cost_q : Q.t;
  delta : string;
  epsilon : string;
  delta_q : Q.t;
  epsilon_q : Q.t;
  local_error_q : Q.t;
  min_gap : Q.t;
}

type compatibility_site = {
  key : string;
  stable_site : string;
  node_id : string;
  mutable candidates : implementation_candidate list;
}

let candidate_dominates ~scalar_safe ~domain_proven a b =
  Q.compare a.cost_q b.cost_q <= 0
  &&
  (if scalar_safe then Q.compare a.local_error_q b.local_error_q <= 0
   else
     Q.compare a.delta_q b.delta_q <= 0
     && Q.compare a.epsilon_q b.epsilon_q <= 0)
  && (domain_proven || Q.compare a.min_gap b.min_gap >= 0)
  &&
  (Q.compare a.cost_q b.cost_q < 0
  ||
  (if scalar_safe then Q.compare a.local_error_q b.local_error_q < 0
   else
     Q.compare a.delta_q b.delta_q < 0
     || Q.compare a.epsilon_q b.epsilon_q < 0)
  || (not domain_proven && Q.compare a.min_gap b.min_gap > 0))

let pareto_reduce_candidates ~scalar_safe ~domain_proven candidates =
  let unique_candidates =
    List.fold_left
      (fun unique candidate ->
        if
          List.exists
            (fun other ->
              other.cname = candidate.cname
              && Q.equal other.cost_q candidate.cost_q
              && Q.equal other.delta_q candidate.delta_q
              && Q.equal other.epsilon_q candidate.epsilon_q
              && Q.equal other.min_gap candidate.min_gap)
            unique
        then unique
        else candidate :: unique)
      [] candidates
    |> List.rev
  in
  List.filter
    (fun candidate ->
      not
        (List.exists
           (fun other ->
             other.index <> candidate.index
             &&
             candidate_dominates ~scalar_safe ~domain_proven other candidate)
           unique_candidates))
    unique_candidates

let candidate_local_error (model : Expr_data.expr_data) delta epsilon =
  let delta_coefficient =
    match (model.delta, model.max_delta) with
    | Some _, Some value -> q_of_numeric_string value
    | _, _ -> Q.zero
  in
  let epsilon_coefficient =
    match (model.epsilon, model.max_epsilon) with
    | Some _, Some value -> q_of_numeric_string value
    | _, _ -> Q.zero
  in
  Q.add (Q.mul delta delta_coefficient) (Q.mul epsilon epsilon_coefficient)

let function_ancestor_flags ast =
  let rec walk has_function_ancestor = function
    | Int _ | Float _ | Var _ -> []
    | Neg expression -> walk has_function_ancestor expression
    | Add (left, right)
    | Sub (left, right)
    | Mul (left, right)
    | Div (left, right)
    | Pow (left, right) ->
        walk has_function_ancestor left @ walk has_function_ancestor right
    | Func (name, argument) ->
        if is_tunable_function name then
          has_function_ancestor :: walk true argument
        else walk has_function_ancestor argument
    | Func2 (_, left, right) ->
        walk has_function_ancestor left @ walk has_function_ancestor right
  in
  Array.of_list (walk false ast)

let debug_print s = if not (Config.is_quiet ()) then print_endline s

let load_exact_costs () =
  let table = Hashtbl.create 512 in
  let path = Config.project_path "implementations/all_costs.json" in
  let channel = open_in path in
  let length = in_channel_length channel in
  let text = really_input_string channel length in
  close_in channel;
  let pattern =
    Str.regexp
      "\"\\([^\"]+\\)\"[ \t\r\n]*:[ \t\r\n]*\\([-+0-9.eE]+\\)"
  in
  let rec scan position =
    try
      ignore (Str.search_forward pattern text position);
      Hashtbl.replace table (Str.matched_group 1 text)
        (q_of_numeric_string (Str.matched_group 2 text));
      scan (Str.match_end ())
    with Not_found -> ()
  in
  scan 0;
  table

(* A Boolean sequential counter avoids coercing every choice to Real merely to
   state that exactly one candidate is selected.  The weighted epsilon, delta,
   and cost expressions remain exact rationals. *)
let exactly_one ctx prefix choices =
  match choices with
  | [] -> [ Z3.Boolean.mk_false ctx ]
  | [ choice ] -> [ choice ]
  | _ when List.length choices <= 16 ->
      let rec pairwise = function
        | [] -> []
        | choice :: rest ->
            List.map
              (fun other ->
                Z3.Boolean.mk_not ctx
                  (Z3.Boolean.mk_and ctx [ choice; other ]))
              rest
            @ pairwise rest
      in
      Z3.Boolean.mk_or ctx choices :: pairwise choices
  | _ ->
      let count = List.length choices in
      let choice_array = Array.of_list choices in
      let counters =
        Array.init (count - 1) (fun index ->
            Z3.Boolean.mk_const_s ctx
              (Printf.sprintf "%s;onehot;%d" prefix index))
      in
      let constraints = ref [ Z3.Boolean.mk_or ctx choices ] in
      constraints :=
        Z3.Boolean.mk_implies ctx choice_array.(0) counters.(0)
        :: !constraints;
      for index = 1 to count - 2 do
        constraints :=
          Z3.Boolean.mk_implies ctx choice_array.(index) counters.(index)
          :: Z3.Boolean.mk_implies ctx counters.(index - 1) counters.(index)
          :: Z3.Boolean.mk_not ctx
               (Z3.Boolean.mk_and ctx
                  [ choice_array.(index); counters.(index - 1) ])
          :: !constraints
      done;
      constraints :=
        Z3.Boolean.mk_not ctx
          (Z3.Boolean.mk_and ctx
             [ choice_array.(count - 1); counters.(count - 2) ])
        :: !constraints;
      !constraints

let custom_error_add (model : Expr_data.expr_data) ctx delta epsilon =
  let delta_term =
    match (model.delta, model.max_delta) with
    | Some _, Some m ->
        Z3.Arithmetic.mk_mul ctx
          [ delta; z3_real ctx m ]
    | _, _ -> Z3.Arithmetic.Real.mk_numeral_i ctx 0
  in
  let epsilon_term =
    match (model.epsilon, model.max_epsilon) with
    | Some _, Some m ->
        Z3.Arithmetic.mk_mul ctx
          [ epsilon; z3_real ctx m ]
    | _, _ -> Z3.Arithmetic.Real.mk_numeral_i ctx 0
  in

  Z3.Arithmetic.mk_add ctx [ delta_term; epsilon_term ]

let basic_error_add (model : Expr_data.expr_data) ctx =
  let machine_epsilon = "1/9007199254740992" in
  let delta_term =
    match (model.delta, model.max_delta) with
    | Some _, Some m ->
        Z3.Arithmetic.mk_mul ctx
          [
            Z3.Arithmetic.Real.mk_numeral_s ctx machine_epsilon;
              (*(remove_some (extract_fraction (parse_expr d)));*)
              z3_real ctx m;
          ]
    | _, _ -> Z3.Arithmetic.Real.mk_numeral_i ctx 0
  in

  let epsilon_term =
    match (model.epsilon, model.max_epsilon) with
    | Some e, Some m ->
        debug_print "Real epsilon";
        debug_print e;
        Z3.Arithmetic.mk_mul ctx
          [
            Z3.Arithmetic.Real.mk_numeral_s ctx machine_epsilon;
            z3_real ctx m;
          ]
    | _, _ -> Z3.Arithmetic.Real.mk_numeral_i ctx 0
  in
  debug_print "In basic erro";
  debug_print "delta";
  debug_print (Z3.Expr.to_string delta_term);
  debug_print "Epsilon";
  debug_print (Z3.Expr.to_string epsilon_term);

  Z3.Arithmetic.mk_add ctx [ delta_term; epsilon_term ]

let basic_error_q (model : Expr_data.expr_data) =
  let machine_epsilon = Q.make Z.one (Z.shift_left Z.one 53) in
  let coefficient field bound =
    match (field, bound) with
    | Some _, Some value -> q_of_numeric_string value
    | _ -> Q.zero
  in
  Q.mul machine_epsilon
    (Q.add
       (coefficient model.delta model.max_delta)
       (coefficient model.epsilon model.max_epsilon))

let next_id = ref 0

let fresh_id () =
  let id = !next_id in
  incr next_id;
  id


  (*I need to keep a track of the node we enter at and add once to anohter global counter
  so when i get to a bit I can e.g exp i can just do v1;exp if duplicate flag*)
let match_domain_error error_model_max ctx ast_root root_id varible_share_state =
  let next_id_domain = ref root_id in

  let decres_id () =
    let id = !next_id_domain in
    decr next_id_domain;
    id in

  let z3_0 = Z3.Arithmetic.Real.mk_numeral_s ctx "0" in
  let _ = Z3.Arithmetic.Real.mk_numeral_s ctx "1" in
  let rec z3_domain_error = function
  | Int i -> let _=decres_id ()in (string_of_int i ,z3_0)
  | Float f -> let _=decres_id ()in(fptaylor_float_key f,z3_0)
  | Var x -> let _=decres_id() in let model = Hashtbl.find error_model_max x in
        (*(x, basic_error_add model ctx, z3_0)*)

        (*(x, basic_error_add model ctx, z3_0)*)
        print_string "In x";
        print_endline (Z3.Expr.to_string (basic_error_add model ctx));
        (x,basic_error_add model ctx)
  |Neg e1 -> let _=decres_id ()in let s1,d_err1 =z3_domain_error e1 in
  ("-(" ^ s1 ^ ")",d_err1)
  |Add(e1,e2)->let _=decres_id() in let s1, d_err1 = z3_domain_error e1 in
        let s2, d_err2 = z3_domain_error e2 in
        let key = "(" ^ s1 ^ " + " ^ s2 ^ ")" in

        let this_err = basic_error_add (Hashtbl.find error_model_max key) ctx in
        ( key,Z3.Arithmetic.mk_add ctx [ d_err1; d_err2; this_err ])
  |Sub(e1,e2)->let _=decres_id() in let s1, d_err1 = z3_domain_error e1 in
        let s2, d_err2 = z3_domain_error e2 in
        let key = "(" ^ s1 ^ " - " ^ s2 ^ ")" in

        let this_err = basic_error_add (Hashtbl.find error_model_max key) ctx in
        ( key,Z3.Arithmetic.mk_add ctx [ d_err1; d_err2; this_err ])
  |Mul(e1,e2)-> let _=decres_id ()in let s1, d_err1 = z3_domain_error e1 in
        let s2, d_err2 = z3_domain_error e2 in
        let key = "(" ^ s1 ^ " * " ^ s2 ^ ")" in

        let this_err = basic_error_add (Hashtbl.find error_model_max key) ctx in
        ( key,Z3.Arithmetic.mk_add ctx [ d_err1; d_err2; this_err ])
  |Div (e1, e2)->let _=decres_id ()in let s1, d_err1 = z3_domain_error e1 in
        let s2, d_err2 = z3_domain_error e2 in
        let key = "(" ^ s1 ^ " * 1/(" ^ s2 ^ "))" in

        let this_err = basic_error_add (Hashtbl.find error_model_max key) ctx in
        ( key,Z3.Arithmetic.mk_add ctx [ d_err1; d_err2; this_err ])
  |Pow(e1,e2)->let _=decres_id() in let s1, d_err1 = z3_domain_error e1 in
        let s2, d_err2 = z3_domain_error e2 in
        let key = s1 ^ "^" ^ s2 in


        let this_err = basic_error_add (Hashtbl.find error_model_max key) ctx in
        ( key,Z3.Arithmetic.mk_add ctx [ d_err1; d_err2; this_err ])
  |Func(f,e)->let id=decres_id ()in let s1, d_err1 = z3_domain_error e in
        let normalized_f = if f = "fabs" then "abs" else f in
        let key = normalized_f ^ "(" ^ s1 ^")" in
        (match varible_share_state.(id) with
        (*Forgot the condition*)
        | true ->print_string ("THIS IS TRUE for " ^ key);
          let delta=Z3.Arithmetic.Real.mk_const_s ctx ("v"^ (string_of_int id) ^ ";delta") in
        let epsilon =Z3.Arithmetic.Real.mk_const_s ctx ("v"^(string_of_int id)^ ";epsilon") in
          let this_err = custom_error_add (Hashtbl.find error_model_max key) ctx delta epsilon  in
        ( key,Z3.Arithmetic.mk_add ctx [ d_err1; this_err ])
        | false ->let this_err = basic_error_add (Hashtbl.find error_model_max key) ctx in
        ( key,Z3.Arithmetic.mk_add ctx [ d_err1; this_err ]))
  | Func2 (f, e1, e2) ->
      let _ = decres_id () in
      let s1, d_err1 = z3_domain_error e1 in
      let s2, d_err2 = z3_domain_error e2 in
      let key = f ^ "(" ^ s1 ^ ", " ^ s2 ^ ")" in
      let this_err = basic_error_add (Hashtbl.find error_model_max key) ctx in
      (key, Z3.Arithmetic.mk_add ctx [ d_err1; d_err2; this_err ])

        

in
 let _,domain_error=z3_domain_error ast_root in
 domain_error


 (*List of key should be possible to do the match_domain in O(n)*)
let dfs_ast_z3 ?fast_analysis ast dom_expr data_spec cost_data error_table
  allow_duplicate candidate_cost_bound ctx opt num_var=
  let module SetStr = Set.Make (String) in
  let dup_set = ref SetStr.empty in
  let site_id = ref 0 in
  let candidate_count_before = ref 0 in
  let candidate_count_after = ref 0 in
  let function_has_ancestor = function_ancestor_flags ast in
  let exact_costs = load_exact_costs () in
  let operation_counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let shared_choices :
      (string, Z3.Expr.expr * Z3.Expr.expr) Hashtbl.t =
    Hashtbl.create 16
  in
  let compatibility_sites = ref [] in
  let compatibility_sites_by_key : (string, compatibility_site) Hashtbl.t =
    Hashtbl.create 16
  in
  let compatibility_base_error = ref Q.zero in
  let compatibility_base_cost = ref Q.zero in
  let dp_safe = ref true in
  let add_compatibility_basic_error model =
    compatibility_base_error :=
      Q.add !compatibility_base_error (basic_error_q model)
  in
  let add_compatibility_fixed_cost operation =
    match fixed_operation_cost_key operation with
    | Some key ->
        let exact =
          Hashtbl.find_opt exact_costs key |> Option.value ~default:Q.zero
        in
        let selected =
          if Config.is_optuner_compatible () then exact
          else q_of_numeric_string (string_of_float (Q.to_float exact))
        in
        compatibility_base_cost := Q.add !compatibility_base_cost selected
    | None -> ()
  in
  let stable_operation_name operation =
    let index = Hashtbl.find_opt operation_counts operation |> Option.value ~default:0 in
    Hashtbl.replace operation_counts operation (index + 1);
    Printf.sprintf "%s_%02d" operation index
  in

  let error_varible_state = Array.make num_var false in

  let z3_0 = Z3.Arithmetic.Real.mk_numeral_s ctx "0" in
  let z3_1 = Z3.Arithmetic.Real.mk_numeral_s ctx "1" in
  let isRealDomain = true in
  let rec rec_internal = function
    | Int i ->
        let _ = fresh_id () in
        (string_of_int i, z3_0, z3_0,z3_0)
    | Float f ->
        let _ = fresh_id () in
        (fptaylor_float_key f, z3_0, z3_0,z3_0)
    | Var x ->
        print_endline "IN VAR";
        let _ = fresh_id () in
        let model = Hashtbl.find error_table x in

        (*(x, basic_error_add model ctx, z3_0)*)
        print_string "In x";
        print_endline (Z3.Expr.to_string (basic_error_add model ctx));
        add_compatibility_basic_error model;
        (x, basic_error_add model ctx, z3_0,z3_0)
    | Neg e1 ->
        let s1, err1, cost,domain_error = rec_internal e1 in
        let _ = fresh_id () in

        ("-(" ^ s1 ^ ")", err1, cost,domain_error)
    | Add (e1, e2) as expression ->
        let s1, err1, cost1,domain_error1 = rec_internal e1 in
        let s2, err2, cost2,domain_error2 = rec_internal e2 in
        let _ = fresh_id () in

        let key = "(" ^ s1 ^ " + " ^ s2 ^ ")" in
        print_endline ("KeyRec: " ^ key);
        (*(x, basic_error_add model ctx, z3_0)*)
        print_string "In add";
        let this_err =
          if Config.is_optuner_compatible () && not (optuner_form_matches expression)
          then z3_0
          else
            let model = Hashtbl.find error_table key in
            add_compatibility_basic_error model;
            basic_error_add model ctx
        in
        ( key,
          Z3.Arithmetic.mk_add ctx [ err1; err2; this_err ],
          Z3.Arithmetic.mk_add ctx [ cost1; cost2 ],
          Z3.Arithmetic.mk_add ctx [domain_error1;domain_error2] )
    | Sub (e1, e2) as expression ->
        print_string "In minus";
        let s1, err1, cost1,domain_error1 = rec_internal e1 in
        let s2, err2, cost2,domain_error2 = rec_internal e2 in
        let _ = fresh_id () in
        let key = "(" ^ s1 ^ " - " ^ s2 ^ ")" in
        print_endline key;
        (*(x, basic_error_add model ctx, z3_0)*)
        print_string ("KeyRec: " ^ key);

        let this_err =
          if Config.is_optuner_compatible () && not (optuner_form_matches expression)
          then z3_0
          else
            let model = Hashtbl.find error_table key in
            add_compatibility_basic_error model;
            basic_error_add model ctx
        in
        ( key,
          Z3.Arithmetic.mk_add ctx [ err1; err2; this_err ],
          Z3.Arithmetic.mk_add ctx [ cost1; cost2 ],
          Z3.Arithmetic.mk_add ctx [domain_error1;domain_error2])
    | Mul (e1, e2) as expression ->
        let s1, err1, cost1,domain_error1 = rec_internal e1 in
        let s2, err2, cost2,domain_error2 = rec_internal e2 in
        let _ = fresh_id () in
        let key = "(" ^ s1 ^ " * " ^ s2 ^ ")" in
        print_endline ("KeyRec: " ^ key);
        let this_err =
          if Config.is_optuner_compatible () && not (optuner_form_matches expression)
          then z3_0
          else
            let model = Hashtbl.find error_table key in
            add_compatibility_basic_error model;
            basic_error_add model ctx
        in
        ( key,
          Z3.Arithmetic.mk_add ctx [ err1; err2; this_err ],
          Z3.Arithmetic.mk_add ctx [ cost1; cost2 ],
          Z3.Arithmetic.mk_add ctx [domain_error1;domain_error2] )
    | Div (e1, e2) as expression ->
        let s1, err1, cost1,domain_error1 = rec_internal e1 in
        let s2, err2, cost2,domain_error2 = rec_internal e2 in
        let _ = fresh_id () in
        let key = "(" ^ s1 ^ " * 1/(" ^ s2 ^ "))" in
        print_endline ("KeyRec: " ^ key);
        let this_err =
          if Config.is_optuner_compatible () && not (optuner_form_matches expression)
          then z3_0
          else
            let model = Hashtbl.find error_table key in
            add_compatibility_basic_error model;
            basic_error_add model ctx
        in
        ( key,
          Z3.Arithmetic.mk_add ctx [ err1; err2; this_err ],
          Z3.Arithmetic.mk_add ctx [ cost1; cost2 ],
          Z3.Arithmetic.mk_add ctx [domain_error1;domain_error2] )
    | Pow (e1, e2) as expression ->
        let s1, err1, cost1,domain_error1 = rec_internal e1 in
        let s2, err2, cost2,domain_error2 = rec_internal e2 in
        let _ = fresh_id () in
        (*Unsure*)
        let key = s1 ^ "^" ^ s2 in
        let this_err =
          if Config.is_optuner_compatible () && not (optuner_form_matches expression)
          then z3_0
          else
            let model = Hashtbl.find error_table key in
            add_compatibility_basic_error model;
            basic_error_add model ctx
        in
        add_compatibility_fixed_cost "pow";
        let fixed_cost =
          z3_real ctx (string_of_float (fixed_operation_cost cost_data "pow"))
        in
        ( key,
          Z3.Arithmetic.mk_add ctx [ err1; err2; this_err ],
          Z3.Arithmetic.mk_add ctx [ cost1; cost2; fixed_cost ],
          Z3.Arithmetic.mk_add ctx [domain_error1;domain_error2] )
    | Func (f, e1) as expression -> (
        (* Reserve the tunable-site id before descending into nested calls. *)
        let tunable = is_tunable_function f in
        let this_site_id = !site_id in
        if tunable then incr site_id;
      (*Domain error needs calucalted up here*)
        let s1, err1, cost1,domain_error1 = rec_internal e1 in
        let id = fresh_id () in
        let stable_site = stable_operation_name f in
        let key =
          match f with
          | "log1p" -> "log(1 + " ^ s1 ^ ")"
          | "expm1" -> "(exp(" ^ s1 ^ ") - 1)"
          | "fabs" -> "abs(" ^ s1 ^ ")"
          | f -> f ^ "(" ^ s1 ^ ")"
        in

        print_string ("Z3key:" ^ key);
        if not tunable then
          let this_err =
            if Config.is_optuner_compatible () && not (optuner_form_matches expression)
            then z3_0
            else
              let model = Hashtbl.find error_table key in
              add_compatibility_basic_error model;
              basic_error_add model ctx
          in
          add_compatibility_fixed_cost f;
          let fixed_cost =
            z3_real ctx (string_of_float (fixed_operation_cost cost_data f))
          in
          ( key,
            Z3.Arithmetic.mk_add ctx [ err1; this_err ],
            Z3.Arithmetic.mk_add ctx [ cost1; fixed_cost ],
            domain_error1 )
        else
        match
          if Config.is_optuner_compatible () then Hashtbl.find_opt shared_choices key
          else None
        with
        | Some (shared_delta, shared_epsilon) ->
            (if
               Config.is_optuner_compatible ()
               && optuner_form_matches expression
             then
              match Hashtbl.find_opt compatibility_sites_by_key key with
              | Some site ->
                  site.candidates <-
                    List.map
                      (fun candidate ->
                        {
                          candidate with
                          local_error_q =
                            Q.add candidate.local_error_q
                              (candidate_local_error
                                 (Hashtbl.find error_table key)
                                 candidate.delta_q candidate.epsilon_q);
                        })
                      site.candidates
              | None ->
                  failwith
                    ("Missing shared compatibility site for " ^ key));
            let this_err =
              if not (optuner_form_matches expression) then z3_0
              else
                custom_error_add (Hashtbl.find error_table key) ctx shared_delta
                  shared_epsilon
            in
            ( key,
              Z3.Arithmetic.mk_add ctx [ err1; this_err ],
              cost1,
              domain_error1 )
        | None ->
        (*I prefer not to use if statements and do match instead
          it just does a match and if it have a return value return it else continue
            the outer match handles this.*)
        (*If duplciate then the domain restriction is jsut the same most likely*)
        (* think that exp(x) + x/exp(x) will ahve the sam domain rsetiction so only one of htem will it resurive cases cannot have the same?
        exp(exp(x)+exp(x)),proporty to the left you can apply it again just to make sure the fianl domain range strection si accurate*)
        (*It is asmart enoguh to know only 1 of theese is the eplson select thing*)
        (*Make a hastmap and see if for x varible there exsits v1 epsilon and v1 delta if not normal if so add the new epsilona nd delta to the error model*)
          (*This way onlt the thing that exsits shoudl be worried about the error model*)
        (*On the way up build a hashtable for this if you know the size of a tree then you can index into an array*)
          (*When i build fptayplor I should know it ??? , then isntialise the array to be the machien epsilon and do it really fast?? *)
        match
          match allow_duplicate with
          | true -> (
              match SetStr.mem key !dup_set with
              | true -> Some (key, err1, cost1,domain_error1)
              | false ->
                  dup_set := SetStr.add key !dup_set;
                  None)
          | false -> None
        with
        | Some t -> t
        | None ->
            let choice = filter_field_member data_spec f in
            let indexed_choice = List.mapi (fun i obj -> (obj, i)) choice in

            let r1, r2, fast_domain_error =
              match fast_analysis with
              | Some analysis -> (
                  match Hashtbl.find_opt analysis.Satire_model.argument_ranges s1 with
                  | Some (range, _error) ->
                      (* SATIRE validates the selected upstream epsilon/delta
                         and every implementation-domain obligation together.
                         Filtering here with the worst error of any possible
                         upstream candidate removed valid configurations (for
                         example log1p(exp(x))).  At encoding time only prove
                         that the real argument range is covered; the
                         candidate-specific perturbed range is checked by the
                         rigorous validator before the point can be emitted. *)
                      (range.lo, range.hi, Some 0.)
                  | None ->
                      failwith
                        ("SATIRE analysis did not record argument range for "
                       ^ s1))
              | None ->
                  let real_domain =
                    Gelpia.gelpia_result s1 dom_expr isRealDomain
                  in
                  let r1, r2 = get_interval_from_string real_domain in
                  (r1, r2, None)
            in

            let node_id = "v" ^ string_of_int id in
            let site_error_model = Hashtbl.find error_table key in
            let scalar_safe =
              this_site_id < Array.length function_has_ancestor
              && not function_has_ancestor.(this_site_id)
            in

            let candidates =
              List.filter_map
                (fun (obj, index) ->
                  let delta =
                    obj |> member "delta" |> to_string_option |> Option.get
                  in
                  let epsilon =
                    obj |> member "epsilon" |> to_string_option |> Option.get
                  in
                  let cname =
                    obj |> member "cname" |> to_string_option |> Option.get
                  in
                  let cost =
                    match cost_data |> member cname with
                    | `Int i -> Some (float_of_int i)
                    | `Float f -> Some f
                    | _ -> None
                  in
                  let cost_q = Hashtbl.find_opt exact_costs cname in
                  let i1, i2 =
                    match obj |> member "domain" |> to_list with
                    | [ lower; upper ] -> (to_string lower, to_string upper)
                    | _ -> failwith "Implementation Domain Error; Check File"
                  in
                  let min_gap =
                    Q.min
                      (Q.sub (Q.of_float r1) (Q.of_string i1))
                      (Q.sub (Q.of_string i2) (Q.of_float r2))
                  in
                  let delta_q = Q.of_string delta in
                  let epsilon_q = Q.of_string epsilon in
                  match (cost, cost_q) with
                  | Some cost, Some cost_q
                    when
                      (if Config.is_optuner_compatible () then
                         Q.compare min_gap Q.zero >= 0
                       else Q.compare min_gap Q.zero > 0)
                         &&
                         (match candidate_cost_bound with
                         | Some bound -> cost <= bound
                         | None -> true) ->
                      Some
                        {
                          index;
                          cname;
                          cost;
                          cost_q;
                          delta;
                          epsilon;
                          delta_q;
                          epsilon_q;
                          local_error_q =
                            candidate_local_error site_error_model delta_q
                              epsilon_q;
                          min_gap;
                        }
                  | _ -> None)
                indexed_choice
            in
            let unfiltered_candidate_count = List.length candidates in
            (* The argument-domain error does not depend on the implementation
               selected at this site. Compute it before candidate reduction. If
               it simplifies to a rational constant, filter every candidate by
               that exact bound and stop treating unused domain margin as a
               Pareto objective. This is especially important for top-level
               exp(x) sites whose margins are orders of magnitude larger than
               the input rounding error. *)
            let domain_error_value_z3 =
              match fast_domain_error with
              | Some error ->
                  Z3.Arithmetic.Real.mk_numeral_s ctx
                    (Q.to_string (q_of_numeric_string (Printf.sprintf "%.17g" error)))
              | None ->
                  let domain_model_max =
                    fst
                      (Parse_input.build_fptaylor_query dom_expr
                         (math_expr_to_source e1))
                    |> Fptaylor.run_fptaylor_query_get_indices
                    |> fun (d_taylor_string, d_sub_string) ->
                    Fptaylor.get_model d_taylor_string d_sub_string
                    |> fun domain_model_error ->
                    Gelpia.get_max_model domain_model_error dom_expr
                  in
                  match_domain_error domain_model_max ctx e1 (id - 1)
                    error_varible_state
            in
            let constant_domain_error =
              let simplified = Z3.Expr.simplify domain_error_value_z3 None in
              if Z3.Expr.is_numeral simplified then
                Some (Z3.Arithmetic.Real.get_ratio simplified)
              else None
            in
            let candidates =
              match constant_domain_error with
              | Some domain_error ->
                  List.filter
                    (fun candidate ->
                      Q.compare domain_error candidate.min_gap < 0)
                    candidates
              | None -> candidates
            in
            let domain_proven = Option.is_some constant_domain_error in
            if not domain_proven then dp_safe := false;
            let reduced_candidates =
              if Config.is_optuner_compatible () then candidates
              else
                pareto_reduce_candidates ~scalar_safe ~domain_proven candidates
            in
            candidate_count_before :=
              !candidate_count_before + unfiltered_candidate_count;
            candidate_count_after :=
              !candidate_count_after + List.length reduced_candidates;
            if not (Config.is_quiet ()) then
              Printf.printf "Candidate reduction for %s: %d -> %d (%s)\n" key
                unfiltered_candidate_count (List.length reduced_candidates)
                (if scalar_safe && domain_proven then
                   "exact scalar + proven domain"
                 else if scalar_safe then "exact scalar"
                 else "componentwise");

            let results, sum_delta, sum_epsilon, sum_cost =
              List.fold_left
                (fun (acc_list, acc_d, acc_e, acc_c) candidate ->
                          let t_bool =
                            Z3.Boolean.mk_const_s ctx
                              (if Config.is_optuner_compatible () then
                                 "choice;" ^ stable_site ^ ";" ^ candidate.cname
                                 ^ ";" ^ string_of_int candidate.index
                               else
                                 node_id ^ ";" ^ key ^ ";" ^ candidate.cname ^ ";"
                                 ^ string_of_int candidate.index)
                          in
                          let t_numeric =
                            Z3.Boolean.mk_ite ctx t_bool z3_1 z3_0
                          in
                          let t_delta =
                            Z3.Arithmetic.mk_mul ctx
                              [
                                t_numeric;
                                z3_real ctx candidate.delta;
                              ]
                          in
                          let t_epsilon =
                            Z3.Arithmetic.mk_mul ctx
                              [
                                t_numeric;
                                z3_real ctx candidate.epsilon;
                              ]
                          in
                          let t_cost =
                            Z3.Arithmetic.mk_mul ctx
                              [
                                t_numeric;
                                Z3.Arithmetic.Real.mk_numeral_s ctx
                                  (Q.to_string
                                     (if Config.is_optuner_compatible () then
                                        candidate.cost_q
                                      else
                                        q_of_numeric_string
                                          (string_of_float candidate.cost)));
                              ]
                          in

                          ( (t_bool, candidate.min_gap) :: acc_list,
                            Z3.Arithmetic.mk_add ctx [ acc_d; t_delta ],
                            Z3.Arithmetic.mk_add ctx [ acc_e; t_epsilon ],
                            Z3.Arithmetic.mk_add ctx [ acc_c; t_cost ] )
                          )
                ([], z3_0, z3_0, z3_0)
                reduced_candidates (*Skip choices dominated in every dimension*)
            in
            (
              let dp_candidates =
                if
                  (not (Config.is_optuner_compatible ()))
                  || optuner_form_matches expression
                then
                  List.map
                    (fun candidate ->
                      if Config.is_optuner_compatible () then candidate
                      else
                        {
                          candidate with
                          cost_q =
                            q_of_numeric_string
                              (string_of_float candidate.cost);
                        })
                    reduced_candidates
                else
                  List.map
                    (fun candidate ->
                      { candidate with local_error_q = Q.zero })
                    reduced_candidates
              in
              let dp_site =
                if Config.is_optuner_compatible () then stable_site else node_id
              in
              let site =
                { key; stable_site = dp_site; node_id; candidates = dp_candidates }
              in
              compatibility_sites := site :: !compatibility_sites;
              if Config.is_optuner_compatible () then
                Hashtbl.replace compatibility_sites_by_key key site);
            print_endline "after n";

            (*Sum vars*)
            let delta_var =
              Z3.Arithmetic.Real.mk_const_s ctx
                (if Config.is_optuner_compatible () then
                   "delta_for_" ^ stable_site
                 else node_id ^ ";delta")
            in
            let epsilon_var =
              Z3.Arithmetic.Real.mk_const_s ctx
                (if Config.is_optuner_compatible () then
                   "epsilon_for_" ^ stable_site
                 else node_id ^ ";epsilon")
            in

            let choice_booleans = List.map fst results in
            let one_hot_constraints =
              exactly_one ctx node_id choice_booleans
            in
            let assert_delta = Z3.Boolean.mk_eq ctx delta_var sum_delta in
            let assert_epsilon = Z3.Boolean.mk_eq ctx epsilon_var sum_epsilon in

            let this_err =
              if
                Config.is_optuner_compatible ()
                && not (optuner_form_matches expression)
              then z3_0
              else custom_error_add site_error_model ctx delta_var epsilon_var
            in
            let local_error = Z3.Arithmetic.mk_add ctx [ err1; this_err ] in

            (*
        let local_error_var =
          Z3.Boolean.mk_eq ctx
            (Z3.Arithmetic.Real.mk_const_s ctx (error_id ^ ";local_error"))
            err1
        in*)
            (*This need to be a different error*)
            let upper = r2 in
            print_float upper;
            (*You onyl get the one function so just find the first match eg "exp(x) and then give it the new eplsion and
            delta just need to endure |v1;epsilon| is insert to the correct node "*)
            (* exp(x) is 1 then x is 0 so when back up exp(x) x do -1 to keep tract on the dfs travesal backwords?*)
            (*ignore(match_domain_error);* it will return a z3 expression that is athe addition of the error but it will use
            z3 varibles e.g basic error add and custom error add where needed*)
            (*let local_domain_value =
              Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float upper)
            in*)
            print_string "\n Himmm";
            (* the constraint used to introduce a local-domain variable here.
               Compare the exact expression directly so Z3 has one fewer Real
               equality and auxiliary variable per function site. *)
            (*let local_domain_z3 =
              Z3.Boolean.mk_eq ctx local_domain_var
              (Z3.Arithmetic.mk_add ctx[
              (Z3.Arithmetic.mk_mul ctx
                   [
                     Z3.Arithmetic.Real.mk_numeral_s ctx "1/9007199254740992";
                     domain_error_value_z3;
                   ]);domain_error1])
            in)*)
            (*I've looked into this and FPTaylor needs to be called again to get a better epsilon for these gaps*)
            (*1.The plan I need to call FPTaylor get a model of the error,run it though gelpia
            2. I will then needs too use the counter here and match it with the model provided by fptaylor. Eg I match a exp here and
          in the fptaylor model i also match a gelpia the epilon variables is the same by the max is the differnet gelpai one*)
            let implications =
              List.map
                (fun (t_i, min_gap) ->
                  Z3.Boolean.mk_implies ctx t_i
                    ((if Config.is_optuner_compatible () then
                        Z3.Arithmetic.mk_le
                      else Z3.Arithmetic.mk_lt)
                       ctx domain_error_value_z3
                       (Z3.Arithmetic.Real.mk_numeral_s ctx
                          (Q.to_string min_gap))))
                results
            in
            (*An epsilon/delta is used here,used for the domain stuff*)
            error_varible_state.(id)<- true;

            Z3.Optimize.add opt
              (one_hot_constraints @ (assert_delta :: assert_epsilon :: implications));
            if Config.is_optuner_compatible () then
              Hashtbl.replace shared_choices key (delta_var, epsilon_var);
            debug_print "Can do";

            (key, local_error, Z3.Arithmetic.mk_add ctx [ sum_cost; cost1 ],domain_error_value_z3))
    | Func2 (f, e1, e2) as expression ->
        let s1, err1, cost1, domain_error1 = rec_internal e1 in
        let s2, err2, cost2, domain_error2 = rec_internal e2 in
        let _ = fresh_id () in
        let key = f ^ "(" ^ s1 ^ ", " ^ s2 ^ ")" in
        let this_err =
          if Config.is_optuner_compatible () && not (optuner_form_matches expression)
          then z3_0
          else
            let model = Hashtbl.find error_table key in
            add_compatibility_basic_error model;
            basic_error_add model ctx
        in
        ( key,
          Z3.Arithmetic.mk_add ctx [ err1; err2; this_err ],
          Z3.Arithmetic.mk_add ctx [ cost1; cost2 ],
          Z3.Arithmetic.mk_add ctx [ domain_error1; domain_error2 ] )
  in
  let result = rec_internal ast in
  prerr_endline
    (Printf.sprintf "Z3 candidates: %d -> %d; choice Booleans: %d"
       !candidate_count_before !candidate_count_after !candidate_count_after);
  ( result,
    List.rev !compatibility_sites,
    !compatibility_base_error,
    !compatibility_base_cost,
    !dp_safe )

let nondominated_points points =
  List.filter
    (fun (cost, error) ->
      not
        (List.exists
           (fun (other_cost, other_error) ->
             Q.compare other_cost cost <= 0
             && Q.compare other_error error <= 0
             &&
             (Q.compare other_cost cost < 0 || Q.compare other_error error < 0))
           points))
    points

type dp_state = {
  dp_cost : Q.t;
  dp_error : Q.t;
  dp_selections : (string * implementation_candidate) list;
}

let pareto_reduce_dp_states states =
  let sorted =
    List.sort
      (fun state_a state_b ->
        let cost_order = Q.compare state_a.dp_cost state_b.dp_cost in
        if cost_order <> 0 then cost_order
        else Q.compare state_a.dp_error state_b.dp_error)
      states
  in
  let rec scan best_error result = function
    | [] -> List.rev result
    | state :: rest ->
        if
          match best_error with
          | Some best -> Q.compare state.dp_error best >= 0
          | None -> false
        then scan best_error result rest
        else scan (Some state.dp_error) (state :: result) rest
  in
  scan None [] sorted

let compatibility_dp_frontier sites base_cost base_error =
  let started = Unix.gettimeofday () in
  let states =
    List.fold_left
      (fun states site ->
        let combined =
          List.concat_map
            (fun state ->
              List.map
                (fun candidate ->
                  {
                    dp_cost = Q.add state.dp_cost candidate.cost_q;
                    dp_error =
                      Q.add state.dp_error candidate.local_error_q;
                    dp_selections =
                      (site.stable_site, candidate) :: state.dp_selections;
                  })
                site.candidates)
            states
        in
        pareto_reduce_dp_states combined)
      [ { dp_cost = base_cost; dp_error = base_error; dp_selections = [] } ]
      sites
  in
  let in_band state =
    (match !z3_cost_low with
    | Some low -> Q.compare state.dp_cost low >= 0
    | None -> true)
    &&
    match !z3_cost_high with
    | Some high -> Q.compare state.dp_cost high < 0
    | None -> true
  in
  let states = List.filter in_band states in
  prerr_endline
    (Printf.sprintf
       "Exact DP search: %.3fs, sites=%d, %d Pareto objective pairs"
       (Unix.gettimeofday () -. started) (List.length sites)
       (List.length states));
  states

let model_solution model =
  List.fold_left
    (fun acc decl ->
      let expression = Z3.FuncDecl.apply decl [] in
      match Z3.Model.eval model expression true with
      | Some value when Z3.Expr.is_numeral value ->
          (expression, Z3.Arithmetic.Real.get_ratio value) :: acc
      | Some value when Z3.Boolean.is_true value ->
          (expression, Q.one) :: acc
      | _ -> acc)
    [] (Z3.Model.get_const_decls model)

let audit_dp_frontier ctx sites base_cost base_error points =
  let started = Unix.gettimeofday () in
  let solver = Z3.Solver.mk_solver ctx None in
  let selected_for state stable_site =
    match
      List.find_map
        (fun (site, candidate) ->
          if site = stable_site then Some candidate else None)
        state.dp_selections
    with
    | Some candidate -> candidate
    | None -> failwith ("DP state omitted site " ^ stable_site)
  in
  (* DP has already fixed one candidate at every site.  Rebuild every claimed
     objective from those exact rational assignments and ask Z3 to verify all
     equalities in one call.  No Boolean search is needed during an audit. *)
  List.iter
    (fun state ->
      let selected =
        List.map
          (fun site -> selected_for state site.stable_site)
          sites
      in
      let cost_terms =
        Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string base_cost)
        :: List.map
             (fun candidate ->
               Z3.Arithmetic.Real.mk_numeral_s ctx
                 (Q.to_string candidate.cost_q))
             selected
      in
      let error_terms =
        Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string base_error)
        :: List.map
             (fun candidate ->
               Z3.Arithmetic.Real.mk_numeral_s ctx
                 (Q.to_string candidate.local_error_q))
             selected
      in
      Z3.Solver.add solver
        [
          Z3.Boolean.mk_eq ctx (Z3.Arithmetic.mk_add ctx cost_terms)
            (Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string state.dp_cost));
          Z3.Boolean.mk_eq ctx (Z3.Arithmetic.mk_add ctx error_terms)
            (Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string state.dp_error));
        ])
    points;
  let audit_result = Z3.Solver.check solver [] in
  (match audit_result with
  | SATISFIABLE -> ()
  | UNSATISFIABLE ->
      failwith "batched Z3 DP assignment audit found an inconsistent point"
  | UNKNOWN ->
      failwith
        ("batched Z3 DP assignment audit returned UNKNOWN: "
        ^ Z3.Solver.get_reason_unknown solver));
  let display_solution state =
    List.concat_map
      (fun (stable_site, candidate) ->
        let site =
          List.find (fun site -> site.stable_site = stable_site) sites
        in
        if Config.is_optuner_compatible () then
          [
            ( Z3.Boolean.mk_const_s ctx
                ("choice;" ^ site.stable_site ^ ";" ^ candidate.cname ^ ";"
               ^ string_of_int candidate.index),
              Q.one );
          ]
        else
          [
            ( Z3.Arithmetic.Real.mk_const_s ctx (site.node_id ^ ";delta"),
              candidate.delta_q );
            ( Z3.Arithmetic.Real.mk_const_s ctx (site.node_id ^ ";epsilon"),
              candidate.epsilon_q );
          ])
      state.dp_selections
  in
  let answers =
    List.map
      (fun state -> (display_solution state, state.dp_cost, state.dp_error))
      points
  in
  prerr_endline
    (Printf.sprintf
       "Z3 assignment audit: %.3fs, checks=%d, verified=%d"
       (Unix.gettimeofday () -. started) 1
       (List.length answers));
  answers

let exact_objective_point ctx opt total_cost total_error objectives =
  Z3.Optimize.push opt;
  List.iter (fun objective -> ignore (Z3.Optimize.minimize opt objective)) objectives;
  let parameters = Z3.Params.mk_params ctx in
  Z3.Params.add_symbol parameters (Z3.Symbol.mk_string ctx "priority")
    (Z3.Symbol.mk_string ctx "lex");
  Z3.Optimize.set_parameters opt parameters;
  let result =
    match Z3.Optimize.check opt with
    | SATISFIABLE -> (
        match Z3.Optimize.get_model opt with
        | Some model -> (
            match
              ( Z3.Model.eval model total_cost true,
                Z3.Model.eval model total_error true )
            with
            | Some cost, Some error ->
                Some
                  ( Z3.Arithmetic.Real.get_ratio cost,
                    Z3.Arithmetic.Real.get_ratio error )
            | _ -> None)
        | None -> None)
    | UNSATISFIABLE | UNKNOWN -> None
  in
  Z3.Optimize.pop opt;
  result

let exact_seed_points ctx opt total_cost total_error count =
  if count <= 0 then []
  else
    let cheapest = exact_objective_point ctx opt total_cost total_error [ total_cost; total_error ] in
    let accurate = exact_objective_point ctx opt total_cost total_error [ total_error; total_cost ] in
    let anchors = List.filter_map Fun.id [ cheapest; accurate ] in
    let weighted =
      match (cheapest, accurate) with
      | Some (min_cost, max_error), Some (max_cost, min_error)
        when count > 2 && Q.compare min_cost max_cost < 0
             && Q.compare min_error max_error < 0 ->
          let cost_range = Q.sub max_cost min_cost in
          let error_range = Q.sub max_error min_error in
          List.init (count - 2) (fun index ->
              let numerator = index + 1 in
              let denominator = count - 1 in
              let cost_weight = Q.make (Z.of_int numerator) (Z.of_int denominator) in
              let error_weight = Q.sub Q.one cost_weight in
              let cost_coefficient = Q.div cost_weight cost_range in
              let error_coefficient = Q.div error_weight error_range in
              let objective =
                Z3.Arithmetic.mk_add ctx
                  [
                    Z3.Arithmetic.mk_mul ctx
                      [ Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string cost_coefficient); total_cost ];
                    Z3.Arithmetic.mk_mul ctx
                      [ Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string error_coefficient); total_error ];
                  ]
              in
              exact_objective_point ctx opt total_cost total_error
                [ objective; total_cost; total_error ])
          |> List.filter_map Fun.id
      | _ -> []
    in
    nondominated_points (anchors @ weighted)

let add_exact_dominance_bound ctx opt total_cost total_error (cost, error) =
  let cost_bound =
    Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string cost)
  in
  let error_bound =
    Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string error)
  in
  (* Retain the certified exact point and every point that can improve either
     objective. Only points dominated by the incumbent are removed. *)
  let improves_cost = Z3.Arithmetic.mk_lt ctx total_cost cost_bound in
  let improves_error = Z3.Arithmetic.mk_lt ctx total_error error_bound in
  let is_incumbent =
    Z3.Boolean.mk_and ctx
      [
        Z3.Boolean.mk_eq ctx total_cost cost_bound;
        Z3.Boolean.mk_eq ctx total_error error_bound;
      ]
  in
  Z3.Optimize.add opt
    [ Z3.Boolean.mk_or ctx [ improves_cost; improves_error; is_incumbent ] ];
  prerr_endline
    (Printf.sprintf
       "Certified exact Pareto bound: cost=%g, error=%g"
       (Q.to_float cost) (Q.to_float error))

let patero_solution ?fast_analysis math_expr dom_expr error_look_up
    allow_duplicate num_var total_e total_c alpha_val=
  let z3_start = Unix.gettimeofday () in
  if not (Config.is_quiet ()) then print_string "Z3_Solving";
  let cost_data =
    Yojson.Basic.from_file
      (Config.project_path "implementations/all_costs.json")
  in
  let spec_data =
    Yojson.Basic.from_file
      (Config.project_path "implementations/all_specifications_str.json")
    |> to_list
  in
  (*let cost_data = Yojson.Basic.from_file "/home/ubuntu/MiniTuner/implementations/test_costs.json" in
let spec_data = Yojson.Basic.from_file "/home/ubuntu/MiniTuner/implementations/test_specifications.json" |> to_list in*)
  (*shadowing *)
  if not (Config.is_quiet ()) then (
    Hashtbl.iter (fun k _ -> print_endline k) error_look_up;
    print_string (List.fold_left (fun acc s -> acc ^ s) "" dom_expr)
  );
  let lexbuf = Lexing.from_string math_expr in
  let ast =
    (* this is just making sure we have parsed the math_expression correctly*)
    match Math_parse.Parser.expr_option Math_parse.Lexer.read lexbuf with
    | None -> failwith "No expression parsed"
    | Some a -> a
  in
  (*Z3 context*)
  let ctx = Z3.mk_context [] in
  let opt = Z3.Optimize.mk_opt ctx in

  (*Root,Toalexpr,Toalcost*)
  let effective_total_c =
    if total_c > 0. then Some total_c else None
  in

  let
    ( (_, total_error, total_cost, _),
      compatibility_sites,
      compatibility_base_error,
      compatibility_base_cost,
      dp_safe )
    =
    dfs_ast_z3 ?fast_analysis ast dom_expr spec_data cost_data error_look_up
      allow_duplicate effective_total_c ctx opt num_var
  in

  (* Cost bands are half-open [low, high).  Independent processes can solve
     these bands concurrently and their union is exactly the original model. *)
  (match !z3_cost_low with
  | Some low ->
      Z3.Optimize.add opt
        [
          Z3.Arithmetic.mk_ge ctx total_cost
            (Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string low));
        ]
  | None -> ());
  (match !z3_cost_high with
  | Some high ->
      Z3.Optimize.add opt
        [
          Z3.Arithmetic.mk_lt ctx total_cost
            (Z3.Arithmetic.Real.mk_numeral_s ctx (Q.to_string high));
        ]
  | None -> ());

  (* Apply user bounds before either exact search engine. *)
  let total_c_for_bound =
    match effective_total_c with Some cost -> cost | None -> total_c
  in
  if total_c_for_bound > 0. then
    Z3.Optimize.add opt
      [
        Z3.Arithmetic.mk_le ctx total_cost
          (z3_real ctx (string_of_float total_c_for_bound));
      ];
  if total_e > 0. then
    Z3.Optimize.add opt
      [
        Z3.Arithmetic.mk_le ctx total_error
          (z3_real ctx (string_of_float total_e));
      ];

  let selected_search_engine =
    match (effective_search_engine (), dp_safe, !search_engine) with
    | Dp, false, Auto ->
        prerr_endline
          "Exact DP disabled because a candidate-domain constraint was not \
           reduced to a rational constant; falling back to Z3";
        Z3
    | engine, _, _ -> engine
  in
  match selected_search_engine with
  | Dp ->
      if not dp_safe then
        invalid_arg
          "--search-engine dp requires exact proof of every candidate-domain constraint; use --search-engine z3";
      compatibility_dp_frontier compatibility_sites compatibility_base_cost
        compatibility_base_error
      |> List.filter (fun state ->
             (total_c_for_bound <= 0.
             || Q.compare state.dp_cost
                  (q_of_numeric_string (string_of_float total_c_for_bound))
                <= 0)
             && (total_e <= 0.
                || Q.compare state.dp_error
                     (q_of_numeric_string (string_of_float total_e))
                   <= 0))
      |> audit_dp_frontier ctx compatibility_sites compatibility_base_cost
           compatibility_base_error
  | Auto -> assert false
  | Z3 ->
  let certification_start = Unix.gettimeofday () in
  let certified_points =
    exact_seed_points ctx opt total_cost total_error !guidance_points
  in
  List.iter
    (add_exact_dominance_bound ctx opt total_cost total_error)
    certified_points;
  prerr_endline
    (Printf.sprintf "Certified exact candidate points: %d"
       (List.length certified_points));
  prerr_endline
    (Printf.sprintf "Exact candidate certification: %.3fs"
       (Unix.gettimeofday () -. certification_start));


  (* Both engines minimize the actual exact objectives.  Positive scalar
     weights do not change a Pareto frontier, but zero weights at alpha=0/1 do
     and make lexicographic staircase enumeration incomplete. *)
  let _ = alpha_val in
  let _ = Z3.Optimize.minimize opt total_cost in
  let _ = Z3.Optimize.minimize opt total_error in
  let p = Z3.Params.mk_params ctx in

  let objectives = [ total_cost; total_error ] in

  print_endline "Will try";

  (* Pareto is retained as a baseline.  Staircase uses lexicographic optimum
     queries and then demands a strict error improvement. *)
  let sym_priority = Z3.Symbol.mk_string ctx "priority" in
  let sym_box =
    Z3.Symbol.mk_string ctx
      (match !z3_engine with Pareto -> "pareto" | Staircase -> "lex")
  in
  Z3.Params.add_symbol p sym_priority sym_box;
  Z3.Optimize.set_parameters opt p;

  debug_print (Z3.Optimize.to_string opt);
  (* TO explain we keep going untill it is UNSAT e.g no more osltuion
  Each time the current sol needs to be blocked *)
  let rec enumerate opt objectives checks =
    match Z3.Optimize.check opt with
    | UNSATISFIABLE ->
        debug_print "UNSAT";
        ([], checks + 1)
    | UNKNOWN ->
        let reason = Z3.Optimize.get_reason_unknown opt in
        failwith ("Z3 returned UNKNOWN: " ^ reason)
    | SATISFIABLE -> (
        match Z3.Optimize.get_model opt with
        | None -> failwith "Z3 returned SAT without a model"
        | Some model ->
            if not (Config.is_quiet ()) then (
              print_string "Sat";
              (* print model *)
              print_endline (Z3.Model.to_string model)
            );

            (* safely evaluate objectives in the model *)
            let vals =
              List.map
                (fun expr ->
                  match Z3.Model.eval model expr true with
                  | Some v -> v
                  | None ->
                      print_string
                        "Expression canot be evaled,set default 0,Needs fixed";
                      Z3.Arithmetic.Integer.mk_numeral_i ctx 0)
                [ total_cost; total_error ]
            in

            (* Fold over constants, get the delta,epslion selction, also gets local error *)
            let solutions = model_solution model in

            (* The blockign is done by just saving that the current ones cannot be dominated*)
            let block =
              match (!z3_engine, vals) with
              | Staircase, [ _; error_value ] ->
                  Z3.Arithmetic.mk_lt ctx total_error error_value
              | Pareto, _ ->
                  Z3.Boolean.mk_or ctx
                    (List.map2
                       (fun obj v -> Z3.Arithmetic.mk_lt ctx obj v)
                       objectives vals)
              | Staircase, _ -> failwith "Expected cost and error objectives"
            in
            Z3.Optimize.add opt [ block ];

            (*Ge the cost I know it to not be non but if there is non then its none*)
            let cost_val =
              match Z3.Model.eval model total_cost true with
              | Some v -> Z3.Arithmetic.Real.get_ratio v
              | None -> Q.zero
            in
            let model_val =
              match Z3.Model.eval model total_error true with
              | Some v -> Z3.Arithmetic.Real.get_ratio v
              | None -> Q.zero
            in
            Printf.printf "%.60f,%.60f\n" (Q.to_float model_val)
              (Q.to_float cost_val);

            (*Added it to the list easier to do,next sat*)
            let remaining, total_checks = enumerate opt objectives (checks + 1) in
            ((solutions, cost_val, model_val) :: remaining, total_checks))
  in
  let answers, checks = enumerate opt objectives 0 in
  prerr_endline
    (Printf.sprintf "Z3 enumeration: %.3fs, engine=%s, checks=%d, %d Pareto models"
       (Unix.gettimeofday () -. z3_start) (z3_engine_name ()) checks
       (List.length answers));
  answers

(* I want to loop through from the top th the bottom
so a dfs appraoch is needed as you must start form the bottom to get teh error at the bottom
as the error at the bottom propegated out "like backpropigation"*)
