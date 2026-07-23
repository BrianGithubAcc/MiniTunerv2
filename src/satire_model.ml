open Math_parse.Exprtype

exception Unresolved of string

type interval = { lo : float; hi : float }

type node = {
  key : string;
  range : interval;
  propagated_error : float;
  children : (node * float) list;
  factors : (string * interval * int) list;
}

type analysis = {
  error_table : (string, Expr_data.expr_data) Hashtbl.t;
  argument_ranges : (string, interval * float) Hashtbl.t;
  node_count : int;
}

let down value =
  if Float.is_finite value then Float.next_after value neg_infinity else value

let up value =
  if Float.is_finite value then Float.next_after value infinity else value

let interval lo hi =
  if Float.is_nan lo || Float.is_nan hi || lo > hi then
    raise (Unresolved "invalid interval")
  else { lo = down lo; hi = up hi }

let max_abs value = max (abs_float value.lo) (abs_float value.hi)
let min_abs value =
  if value.lo <= 0. && value.hi >= 0. then 0.
  else min (abs_float value.lo) (abs_float value.hi)

let add a b = interval (a.lo +. b.lo) (a.hi +. b.hi)
let neg a = interval (-.a.hi) (-.a.lo)
let sub a b = add a (neg b)

let mul a b =
  let products = [ a.lo *. b.lo; a.lo *. b.hi; a.hi *. b.lo; a.hi *. b.hi ] in
  interval
    (List.fold_left min infinity products)
    (List.fold_left max neg_infinity products)

let pow_interval value exponent =
  if exponent = 0 then interval 1. 1.
  else
    let values =
      [
        value.lo ** float_of_int exponent;
        value.hi ** float_of_int exponent;
      ]
    in
    let values =
      if exponent mod 2 = 0 && value.lo <= 0. && value.hi >= 0. then
        0. :: values
      else values
    in
    interval
      (List.fold_left min infinity values)
      (List.fold_left max neg_infinity values)

let grouped_factors factors =
  let table = Hashtbl.create (List.length factors) in
  List.iter
    (fun (key, range, count) ->
      let _, _, previous =
        Hashtbl.find_opt table key |> Option.value ~default:(key, range, 0)
      in
      Hashtbl.replace table key (key, range, previous + count))
    factors;
  Hashtbl.to_seq_values table |> List.of_seq

let factors_of node =
  match node.factors with
  | [] -> [ (node.key, node.range, 1) ]
  | factors -> factors

let range_of_factors factors =
  List.fold_left
    (fun result (_, range, exponent) ->
      mul result (pow_interval range exponent))
    (interval 1. 1.) factors

let div a b =
  if b.lo <= 0. && b.hi >= 0. then
    raise (Unresolved "division interval contains zero");
  mul a (interval (1. /. b.hi) (1. /. b.lo))

let unary_monotone f value = interval (f value.lo) (f value.hi)

let trig_range name value =
  let width = value.hi -. value.lo in
  if width >= 2. *. Float.pi then interval (-1.) 1.
  else
    let f = if name = "sin" then sin else cos in
    let samples = ref [ f value.lo; f value.hi ] in
    let offset = if name = "sin" then Float.pi /. 2. else 0. in
    let first =
      int_of_float (ceil ((value.lo -. offset) /. Float.pi))
    in
    let last = int_of_float (floor ((value.hi -. offset) /. Float.pi)) in
    for k = first to last do
      samples := f (offset +. (float_of_int k *. Float.pi)) :: !samples
    done;
    interval
      (List.fold_left min infinity !samples)
      (List.fold_left max neg_infinity !samples)

let q_of_numeric_string value =
  let value = String.trim value in
  try Q.of_string value
  with Invalid_argument _ ->
    let index =
      match String.index_opt value 'e' with
      | Some index -> Some index
      | None -> String.index_opt value 'E'
    in
    match index with
    | None -> raise (Invalid_argument ("invalid numeric value " ^ value))
    | Some index ->
        let mantissa = Q.of_string (String.sub value 0 index) in
        let exponent =
          int_of_string
            (String.sub value (index + 1) (String.length value - index - 1))
        in
        let scale = Q.of_bigint (Z.pow (Z.of_int 10) (abs exponent)) in
        if exponent >= 0 then Q.mul mantissa scale else Q.div mantissa scale

let float_key value =
  let rational = q_of_numeric_string value in
  if Z.equal (Q.den rational) Z.one then Z.to_string (Q.num rational)
  else
    Printf.sprintf "(%s / %s)" (Z.to_string (Q.num rational))
      (Z.to_string (Q.den rational))

let float_string value =
  if not (Float.is_finite value) then raise (Unresolved "non-finite bound");
  Printf.sprintf "%.17g" value

let make_error_data delta epsilon =
  {
    Expr_data.delta = Some "1";
    higher = None;
    epsilon = Some "1";
    max_delta = Some (float_string delta);
    max_higher = None;
    max_epsilon = Some (float_string epsilon);
  }

let parse_domains domains =
  let table = Hashtbl.create 16 in
  let pattern =
    Str.regexp
      "^[ \t]*\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t]+in[ \t]*\\[[ \t]*\\([^,]+\\),[ \t]*\\([^]]+\\)\\]"
  in
  List.iter
    (fun domain ->
      if Str.string_match pattern domain 0 then
        let name = Str.matched_group 1 domain in
        let lower = Q.to_float (q_of_numeric_string (Str.matched_group 2 domain)) in
        let upper = Q.to_float (q_of_numeric_string (Str.matched_group 3 domain)) in
        Hashtbl.replace table name (interval lower upper)
      else raise (Unresolved ("unsupported domain: " ^ domain)))
    domains;
  table

let candidate_noise_by_operation () =
  let json =
    Yojson.Basic.from_file
      (Config.project_path "implementations/all_specifications_str.json")
  in
  let table = Hashtbl.create 16 in
  let open Yojson.Basic.Util in
  json |> to_list
  |> List.iter (fun row ->
         match row |> member "operation" |> to_string_option with
         | None -> ()
         | Some operation ->
             let epsilon =
               row |> member "epsilon" |> to_string_option
               |> Option.map q_of_numeric_string |> Option.map Q.to_float
               |> Option.value ~default:0.
             in
             let delta =
               row |> member "delta" |> to_string_option
               |> Option.map q_of_numeric_string |> Option.map Q.to_float
               |> Option.value ~default:0.
             in
             let old_epsilon, old_delta =
               Hashtbl.find_opt table operation |> Option.value ~default:(0., 0.)
             in
             Hashtbl.replace table operation
               (max old_epsilon epsilon, max old_delta delta));
  table

let analyse domains expression =
  let domain_table = parse_domains domains in
  let candidate_noise = candidate_noise_by_operation () in
  let arguments = Hashtbl.create 64 in
  let nodes = ref 0 in
  let unit_roundoff = 0x1p-53 in
  let minimum_subnormal = Int64.float_of_bits 1L in
  let rounding_error range =
    (unit_roundoff *. max_abs range) +. minimum_subnormal
  in
  let rec visit = function
    | Int value ->
        incr nodes;
        let range = interval (float_of_int value) (float_of_int value) in
        {
          key = string_of_int value;
          range;
          propagated_error = 0.;
          children = [];
          factors = [ (string_of_int value, range, 1) ];
        }
    | Float value ->
        incr nodes;
        let number = float_of_string value in
        let range = interval number number in
        {
          key = float_key value;
          range;
          propagated_error = 0.;
          children = [];
          factors = [ (float_key value, range, 1) ];
        }
    | Var name ->
        incr nodes;
        let range =
          match Hashtbl.find_opt domain_table name with
          | Some range -> range
          | None -> raise (Unresolved ("missing finite domain for " ^ name))
        in
        {
          key = name;
          range;
          propagated_error = rounding_error range;
          children = [];
          factors = [ (name, range, 1) ];
        }
    | Neg expression ->
        incr nodes;
        let child = visit expression in
        {
          key = "-(" ^ child.key ^ ")";
          range = neg child.range;
          propagated_error = child.propagated_error;
          children = [ (child, 1.) ];
          factors = [];
        }
    | (Add (left, right) | Sub (left, right)) as whole ->
        incr nodes;
        let left = visit left and right = visit right in
        let is_sub = match whole with Sub _ -> true | _ -> false in
        let range =
          if is_sub && left.key = right.key then interval 0. 0.
          else if is_sub then sub left.range right.range
          else add left.range right.range
        in
        {
          key =
            "(" ^ left.key ^ (if is_sub then " - " else " + ") ^ right.key ^ ")";
          range;
          propagated_error =
            left.propagated_error +. right.propagated_error +. rounding_error range;
          children = [ (left, 1.); (right, 1.) ];
          factors = [];
        }
    | Mul (left, right) ->
        incr nodes;
        let left = visit left and right = visit right in
        let factors =
          grouped_factors (factors_of left @ factors_of right)
        in
        let range = range_of_factors factors in
        let left_factor = max_abs right.range and right_factor = max_abs left.range in
        {
          key = "(" ^ left.key ^ " * " ^ right.key ^ ")";
          range;
          propagated_error =
            (left_factor *. left.propagated_error)
            +. (right_factor *. right.propagated_error)
            +. (left.propagated_error *. right.propagated_error)
            +. rounding_error range;
          children = [ (left, left_factor); (right, right_factor) ];
          factors;
        }
    | Div (left, right) ->
        incr nodes;
        let left = visit left and right = visit right in
        let minimum = min_abs right.range in
        if minimum = 0. then raise (Unresolved "division domain contains zero");
        let range = div left.range right.range in
        let left_factor = 1. /. minimum in
        let right_factor = max_abs left.range /. (minimum *. minimum) in
        {
          key = "(" ^ left.key ^ " * 1/(" ^ right.key ^ "))";
          range;
          propagated_error =
            (left_factor *. left.propagated_error)
            +. (right_factor *. right.propagated_error)
            +. rounding_error range;
          children = [ (left, left_factor); (right, right_factor) ];
          factors = [];
        }
    | Pow (base, Int exponent) ->
        incr nodes;
        let base = visit base in
        let values = [ base.range.lo ** float_of_int exponent; base.range.hi ** float_of_int exponent ] in
        let values = if exponent mod 2 = 0 && base.range.lo <= 0. && base.range.hi >= 0. then 0. :: values else values in
        let range =
          interval (List.fold_left min infinity values)
            (List.fold_left max neg_infinity values)
        in
        let derivative =
          abs_float (float_of_int exponent)
          *. (max_abs base.range ** float_of_int (max 0 (exponent - 1)))
        in
        {
          key = base.key ^ "^" ^ string_of_int exponent;
          range;
          propagated_error =
            (derivative *. base.propagated_error) +. rounding_error range;
          children = [ (base, derivative) ];
          factors = [];
        }
    | Pow _ -> raise (Unresolved "non-integer power")
    | Func (name, expression) ->
        incr nodes;
        let child = visit expression in
        let normalized = if name = "fabs" then "abs" else name in
        Hashtbl.replace arguments child.key (child.range, child.propagated_error);
        let range, derivative =
          match normalized with
          | "exp" -> (unary_monotone exp child.range, exp child.range.hi)
          | "log" ->
              if child.range.lo <= 0. then raise (Unresolved "log domain nonpositive");
              (unary_monotone log child.range, 1. /. child.range.lo)
          | "log1p" ->
              if child.range.lo <= -1. then raise (Unresolved "log1p domain invalid");
              (unary_monotone log1p child.range, 1. /. (1. +. child.range.lo))
          | "expm1" -> (unary_monotone expm1 child.range, exp child.range.hi)
          | "sqrt" ->
              if child.range.lo < 0. then raise (Unresolved "sqrt domain negative");
              let derivative =
                if child.range.lo = 0. then infinity
                else 1. /. (2. *. sqrt child.range.lo)
              in
              (unary_monotone sqrt child.range, derivative)
          | "sin" | "cos" -> (trig_range normalized child.range, 1.)
          | "tan" ->
              let first_pole =
                ceil ((child.range.lo -. (Float.pi /. 2.)) /. Float.pi)
              in
              let pole = (first_pole *. Float.pi) +. (Float.pi /. 2.) in
              if pole <= child.range.hi then raise (Unresolved "tan interval crosses pole");
              let range = unary_monotone tan child.range in
              let derivative =
                max (1. +. (tan child.range.lo ** 2.))
                  (1. +. (tan child.range.hi ** 2.))
              in
              (range, derivative)
          | "abs" ->
              let range =
                if child.range.lo >= 0. then child.range
                else if child.range.hi <= 0. then neg child.range
                else interval 0. (max_abs child.range)
              in
              (range, 1.)
          | "atan" -> (unary_monotone atan child.range, 1.)
          | other -> raise (Unresolved ("unsupported function " ^ other))
        in
        if not (Float.is_finite derivative) then
          raise (Unresolved (normalized ^ " derivative unresolved"));
        let local_epsilon, local_delta =
          Hashtbl.find_opt candidate_noise normalized
          |> Option.value ~default:(unit_roundoff, minimum_subnormal)
        in
        let local =
          (local_epsilon *. max_abs range) +. local_delta +. rounding_error range
        in
        let key =
          match normalized with
          | "log1p" -> "log(1 + " ^ child.key ^ ")"
          | "expm1" -> "(exp(" ^ child.key ^ ") - 1)"
          | _ -> normalized ^ "(" ^ child.key ^ ")"
        in
        {
          key;
          range;
          propagated_error = (derivative *. child.propagated_error) +. local;
          children = [ (child, derivative) ];
          factors = [];
        }
    | Func2 _ -> raise (Unresolved "binary function not normalized")
  in
  let root = visit expression in
  let sensitivities = Hashtbl.create (2 * !nodes) in
  let rec reverse sensitivity node =
    let previous = Hashtbl.find_opt sensitivities node.key |> Option.value ~default:0. in
    Hashtbl.replace sensitivities node.key (max previous sensitivity);
    List.iter
      (fun (child, derivative) -> reverse (sensitivity *. derivative) child)
      node.children
  in
  reverse 1. root;
  let table = Hashtbl.create (2 * !nodes) in
  let rec populate node =
    let sensitivity = Hashtbl.find sensitivities node.key in
    Hashtbl.replace table node.key
      (make_error_data sensitivity (sensitivity *. max_abs node.range));
    List.iter (fun (child, _) -> populate child) node.children
  in
  populate root;
  { error_table = table; argument_ranges = arguments; node_count = !nodes }
