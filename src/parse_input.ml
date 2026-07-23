open Math_parse.Exprtype
(*This matches the input expression into varibles and domains | math (expression)*)

(*FPtalyor needs the real x in [I_1,I_2]; for input*)
let get_talyor_domain varible_domian_string =
  List.fold_left
    (fun acc s -> acc ^ "  real " ^ s ^ ";\n")
    "Variables\n" varible_domian_string

(*This gets a hash set from parsing the math expression*)
(*Each vn/action correspons to a subexpression so must be given to fptaylor as such for when you parse fptaylor*)
(* We also care about ordering here v0=x must be before v1=sin(x)*)

let get_fptaylor_hash_table ast =
  let tbl : (string, string) Hashtbl.t = Hashtbl.create 32 in
  let vars_in_order : string list ref = ref [] in
  let counter = ref 0 in

  let next_var () =
    let v = Printf.sprintf "v%d" !counter in
    incr counter;
    v
  in

  let rec ast_to_fptaylor ast =
    let repr =
      match ast with
      | Int i -> string_of_int i
      | Float f -> f
      | Var x -> x
      | Neg e1 ->
          let v1 = ast_to_fptaylor e1 in
          "-( " ^ v1 ^ ")"
      | Add (e1, e2) ->
          let v1 = ast_to_fptaylor e1 in
          let v2 = ast_to_fptaylor e2 in
          v1 ^ " + " ^ v2
      | Sub (e1, e2) ->
          let v1 = ast_to_fptaylor e1 in
          let v2 = ast_to_fptaylor e2 in
          v1 ^ " - " ^ v2
      | Mul (e1, e2) ->
          let v1 = ast_to_fptaylor e1 in
          let v2 = ast_to_fptaylor e2 in
          v1 ^ " * " ^ v2
      | Div (e1, e2) ->
          let v1 = ast_to_fptaylor e1 in
          let v2 = ast_to_fptaylor e2 in
          (*For dividsion I need to manualy tell it use inv(x) for FPTaylor*)
          (*This is jsut faster than division?*)
          let inv_v2 = next_var () in
          Hashtbl.add tbl inv_v2 ("inv(" ^ v2 ^ ")");
          vars_in_order := !vars_in_order @ [ inv_v2 ];
          if Config.is_optuner_compatible () then
            (* The common node creation below becomes OpTuner's rounded
               multiplication.  Returning a separately-created multiplication
               here would add an extra rounded identity definition. *)
            v1 ^ " * " ^ inv_v2
          else
            let v = next_var () in
            Hashtbl.add tbl v (v1 ^ " * " ^ inv_v2);
            vars_in_order := !vars_in_order @ [ v ];
            v
      | Pow (e1, e2) ->
          let v1 = ast_to_fptaylor e1 in
          let v2 = ast_to_fptaylor e2 in
          v1 ^ " ^ " ^ v2
      | Func (f, e1) -> (
          (*FPTaylor doesn't exppect log1p*)
          let v1 = ast_to_fptaylor e1 in
          match f with
          | "log1p" -> "log( 1 + " ^ v1 ^ ")"
          | "expm1" -> "exp(" ^ v1 ^ ") - " ^ "1"
          | "fabs" -> "abs(" ^ v1 ^ ")"
          | _ -> f ^ "(" ^ v1 ^ ")")
      | Func2 (f, e1, e2) ->
          let v1 = ast_to_fptaylor e1 in
          let v2 = ast_to_fptaylor e2 in
          (match f with
          | "hypot" ->
              "sqrt((" ^ v1 ^ " * " ^ v1 ^ ") + (" ^ v2 ^ " * " ^ v2 ^ "))"
          | _ -> f ^ "(" ^ v1 ^ ", " ^ v2 ^ ")")
    in
    (* Always create a new variable for each node *)
    let v = next_var () in
    Hashtbl.add tbl v repr;
    vars_in_order := !vars_in_order @ [ v ];
    v
  in

  let root_var = ast_to_fptaylor ast in
  (tbl, root_var, vars_in_order)

let build_fptaylor_query variable_domian math_expression =
  (*Create a buffer for the query, this is a buffer as query size is unkown ahead of time*)
  let query = Buffer.create 128 in
  let lexbuf = Lexing.from_string math_expression in
  let ast =
    (* this is just making sure we have parsed the math_expression correctly*)
    match Math_parse.Parser.expr_option Math_parse.Lexer.read lexbuf with
    | None -> failwith "No expression parsed"
    | Some a -> a
  in
  (*Add the Varibles and domain*)
  Buffer.add_string query (get_talyor_domain variable_domian);

  (* Add Defintions*)
  let hstable, root, vars_in_order = get_fptaylor_hash_table ast in
  Buffer.add_string query "Definitions\n";
  List.iter
    (fun v ->
      let expression = Hashtbl.find hstable v in
      (Buffer.add_string query)
        (Printf.sprintf "  %s = rnd64(%s);\n" v expression))
    !vars_in_order;

  (*Tells FPtaylor this is the expression to evalutate*)
  Buffer.add_string query "Expressions\n  ";
  Buffer.add_string query (root ^ ";\n");
  (*Debug print*)
  (*print_string (Buffer.contents query);*)
  (*Return Root,and how mayn nodes*)
  (Buffer.contents query ,List.length !vars_in_order)
