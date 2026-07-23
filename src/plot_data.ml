let rn_eps_and_delta epsilon delta =
  let epsilon_mantissa, epsilon_exponent = Stdlib.frexp epsilon in
  let delta_mantissa, delta_exponent = Stdlib.frexp delta in
  match (epsilon = 0., delta = 0.) with
  | true, true -> (1., 0, 0)
  | true, false ->
      if delta_exponent = 0 then
        (delta_mantissa *. 2., 0, -1)
      else (delta_mantissa, 0, delta_exponent)
  | false, true ->
      if epsilon_exponent = 0 then
        (epsilon_mantissa *. 2., -1, 0)
      else (epsilon_mantissa, epsilon_exponent, 0)
  | false, false ->
      let coefficient = max epsilon_mantissa delta_mantissa in
      if epsilon_exponent <> 0 && delta_exponent <> 0 then
        (coefficient, epsilon_exponent, delta_exponent)
      else
        (* FPTaylor reserves exponent zero to mean that the corresponding
           error is absent. Shift the shared scale and both exponents by the
           same amount so a real nonzero epsilon/delta is never disabled. *)
        let rec choose_shift = function
          | [] -> assert false
          | shift :: rest ->
              if epsilon_exponent - shift <> 0
                 && delta_exponent - shift <> 0
              then shift
              else choose_shift rest
        in
        let shift = choose_shift [ -1; 1; -2; 2; -3; 3 ] in
        ( coefficient *. (2. ** float_of_int shift),
          epsilon_exponent - shift,
          delta_exponent - shift )

let fptaylor_compatible_noise epsilon delta =
  let coefficient, epsilon_exponent, delta_exponent =
    rn_eps_and_delta (Q.to_float epsilon) (Q.to_float delta)
  in
  let coefficient =
    Q.of_string (Printf.sprintf "%.17g" coefficient)
  in
  let scaled exponent =
    if exponent = 0 then Q.zero
    else
      let power = Q.of_bigint (Z.pow (Z.of_int 2) (abs exponent)) in
      if exponent > 0 then Q.mul coefficient power
      else Q.div coefficient power
  in
  (scaled epsilon_exponent, scaled delta_exponent)

let unquote_z3_name name =
  let length = String.length name in
  if length >= 2 && name.[0] = '|' && name.[length - 1] = '|' then
    String.sub name 1 (length - 2)
  else name

let compatibility_assignment solution =
  List.filter_map
    (fun (expression, selected) ->
      if not (Q.equal selected Q.one) then None
      else
        match
          String.split_on_char ';'
            (unquote_z3_name (Z3.Expr.to_string expression))
        with
        | [ "choice"; site; cname; index ] ->
            Some (site, cname, int_of_string index)
        | _ -> None)
    solution

let compatibility_specs assignment =
  let selected = Hashtbl.create (List.length assignment) in
  List.iter
    (fun (site, cname, index) ->
      Hashtbl.replace selected site (cname, index))
    assignment;
  let specs =
    Yojson.Basic.from_file
      (Config.project_path "implementations/all_specifications_str.json")
    |> Yojson.Basic.Util.to_list
  in
  let by_candidate = Hashtbl.create 128 in
  let operation_indexes = Hashtbl.create 16 in
  let numeric_q value =
    try Q.of_string value
    with Invalid_argument _ ->
      let exponent_index =
        match String.index_opt value 'e' with
        | Some index -> Some index
        | None -> String.index_opt value 'E'
      in
      match exponent_index with
      | None -> Q.of_float (float_of_string value)
      | Some index ->
          let mantissa = String.sub value 0 index in
          let exponent =
            int_of_string
              (String.sub value (index + 1) (String.length value - index - 1))
          in
          let base = Q.of_string mantissa in
          let scale = Q.of_bigint (Z.pow (Z.of_int 10) (abs exponent)) in
          if exponent >= 0 then Q.mul base scale else Q.div base scale
  in
  List.iter
    (fun row ->
      let open Yojson.Basic.Util in
      let operation = row |> member "operation" |> to_string in
      let cname = row |> member "cname" |> to_string in
      let index =
        Hashtbl.find_opt operation_indexes operation |> Option.value ~default:0
      in
      Hashtbl.replace operation_indexes operation (index + 1);
      let domain_lower, domain_upper =
        match row |> member "domain" |> to_list with
        | [ lower; upper ] -> (to_string lower, to_string upper)
        | _ -> failwith "Implementation candidate has no closed domain"
      in
      Hashtbl.replace by_candidate (operation, index)
        ( cname,
          row |> member "epsilon" |> to_string |> numeric_q,
          row |> member "delta" |> to_string |> numeric_q,
          domain_lower,
          domain_upper ))
    specs;
  (selected, by_candidate)

let compatibility_query_parts prefix ast solution =
  let assignment = compatibility_assignment solution in
  let selected, specs = compatibility_specs assignment in
  let counts : (string, int) Hashtbl.t = Hashtbl.create 32 in
  let next_name prefix =
    let index = Hashtbl.find_opt counts prefix |> Option.value ~default:0 in
    Hashtbl.replace counts prefix (index + 1);
    Printf.sprintf "%s_%02d" prefix index
  in
  let generated name = prefix ^ name in
  let custom_rounding name body =
    match Hashtbl.find_opt selected name with
    | None -> Printf.sprintf "rnd64(%s)" body
    | Some (cname, index) ->
        let operation =
          match String.rindex_opt name '_' with
          | Some separator -> String.sub name 0 separator
          | None -> name
        in
        let expected_cname, epsilon_q, delta_q, _, _ =
          Hashtbl.find specs (operation, index)
        in
        if cname <> expected_cname then
          failwith
            (Printf.sprintf
               "OpTuner candidate mapping mismatch at %s: %s <> %s"
               name cname expected_cname);
        let mantissa, epsilon_exponent, delta_exponent =
          rn_eps_and_delta (Q.to_float epsilon_q) (Q.to_float delta_q)
        in
        Printf.sprintf "rnd[64,ne,%.17g,%d,%d](%s)" mantissa
          epsilon_exponent delta_exponent body
  in
  let rec definitions = function
    | Math_parse.Exprtype.Int value ->
        let name = next_name "num" |> generated in
        (name, [ Printf.sprintf "  %s = rnd64(%d);" name value ])
    | Float value ->
        let name = next_name "num" |> generated in
        (name, [ Printf.sprintf "  %s = rnd64(%s);" name value ])
    | Var variable ->
        let name = next_name ("var_" ^ variable) |> generated in
        (name, [ Printf.sprintf "  %s = rnd64(%s);" name variable ])
    | Neg expression ->
        let child, lines = definitions expression in
        let name = next_name "sub" |> generated in
        (name, lines @ [ Printf.sprintf "  %s = rnd64(-%s);" name child ])
    | Add (left, right)
    | Sub (left, right)
    | Mul (left, right)
    | Pow (left, right) as expression ->
        let left_name, left_lines = definitions left in
        let right_name, right_lines = definitions right in
        let prefix, operator =
          match expression with
          | Add _ -> ("add", "+")
          | Sub _ -> ("sub", "-")
          | Mul _ -> ("mul", "*")
          | Pow _ -> ("pow", "^")
          | _ -> assert false
        in
        let name = next_name prefix |> generated in
        ( name,
          left_lines @ right_lines
          @ [
              Printf.sprintf "  %s = rnd64(%s %s %s);" name left_name
                operator right_name;
            ] )
    | Div (left, right) ->
        (* OpTuner normalises division to a separately rounded reciprocal
           followed by a rounded multiplication.  The shape and stable names
           matter because FPTaylor assigns one error term to each definition. *)
        let left_name, left_lines = definitions left in
        let right_name, right_lines = definitions right in
        let inverse_name = next_name "inv" |> generated in
        let multiply_name = next_name "mul" |> generated in
        ( multiply_name,
          left_lines @ right_lines
          @ [
              Printf.sprintf "  %s = rnd64(inv(%s));" inverse_name right_name;
              Printf.sprintf "  %s = rnd64(%s * %s);" multiply_name left_name
                inverse_name;
            ] )
    | Func (operation, argument) ->
        let child, lines = definitions argument in
        let site_name = next_name operation in
        let name = generated site_name in
        let body =
          match operation with
          | "expm1" -> Printf.sprintf "(exp(%s) - 1)" child
          | "log1p" -> Printf.sprintf "log(1 + %s)" child
          | "fabs" -> Printf.sprintf "abs(%s)" child
          | _ -> Printf.sprintf "%s(%s)" operation child
        in
        (name, lines @ [ Printf.sprintf "  %s = %s;" name (custom_rounding site_name body) ])
    | Func2 (operation, left, right) ->
        let left_name, left_lines = definitions left in
        let right_name, right_lines = definitions right in
        let site_name = next_name operation in
        let name = generated site_name in
        let body = Printf.sprintf "%s(%s, %s)" operation left_name right_name in
        ( name,
          left_lines @ right_lines
          @ [ Printf.sprintf "  %s = %s;" name (custom_rounding site_name body) ] )
  in
  definitions ast

let compatibility_query domains ast solution =
  let root, lines = compatibility_query_parts "" ast solution in
  Parse_input.get_talyor_domain domains ^ "\nDefinitions\n"
  ^ String.concat "\n" lines
  ^ "\n\nExpressions\n  " ^ root ^ ";\n"

let compatibility_batch_query domains ast solutions =
  let definitions, roots =
    List.mapi
      (fun index (solution, _, _) ->
        let prefix = Printf.sprintf "candidate_%03d_" index in
        let root, lines = compatibility_query_parts prefix ast solution in
        (lines, root))
      solutions
    |> List.split
  in
  Parse_input.get_talyor_domain domains ^ "\nDefinitions\n"
  ^ String.concat "\n" (List.concat definitions)
  ^ "\n\nExpressions\n"
  ^ String.concat "\n" (List.map (Printf.sprintf "  %s;") roots)
  ^ "\n"

let compatibility_noises ast solution =
  let assignment = compatibility_assignment solution in
  let selected, specs = compatibility_specs assignment in
  let counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let first_site_by_key : (string, string) Hashtbl.t = Hashtbl.create 32 in
  let occurrences = ref [] in
  let next_site operation =
    let index = Hashtbl.find_opt counts operation |> Option.value ~default:0 in
    Hashtbl.replace counts operation (index + 1);
    Printf.sprintf "%s_%02d" operation index
  in
  let rec visit = function
    | Math_parse.Exprtype.Int value -> string_of_int value
    | Float value -> value
    | Var variable -> variable
    | Neg expression -> "-(" ^ visit expression ^ ")"
    | Add (left, right) -> "(" ^ visit left ^ " + " ^ visit right ^ ")"
    | Sub (left, right) -> "(" ^ visit left ^ " - " ^ visit right ^ ")"
    | Mul (left, right) -> "(" ^ visit left ^ " * " ^ visit right ^ ")"
    | Div (left, right) ->
        "(" ^ visit left ^ " * 1/(" ^ visit right ^ "))"
    | Pow (left, right) -> visit left ^ "^" ^ visit right
    | Func (operation, argument) ->
        let child = visit argument in
        let site = next_site operation in
        let key =
          match operation with
          | "log1p" -> "log(1 + " ^ child ^ ")"
          | "expm1" -> "(exp(" ^ child ^ ") - 1)"
          | "fabs" -> "abs(" ^ child ^ ")"
          | _ -> operation ^ "(" ^ child ^ ")"
        in
        let first_site =
          match Hashtbl.find_opt first_site_by_key key with
          | Some existing -> existing
          | None ->
              Hashtbl.replace first_site_by_key key site;
              site
        in
        occurrences := (site, first_site) :: !occurrences;
        key
    | Func2 (operation, left, right) ->
        let left_key = visit left in
        let right_key = visit right in
        let _ = next_site operation in
        operation ^ "(" ^ left_key ^ ", " ^ right_key ^ ")"
  in
  let _ = visit ast in
  List.filter_map
    (fun (site, selected_site) ->
      match Hashtbl.find_opt selected selected_site with
      | None -> None
      | Some (cname, index) ->
          let operation =
          match String.rindex_opt selected_site '_' with
          | Some separator -> String.sub selected_site 0 separator
          | None -> selected_site
          in
          let expected_cname, epsilon, delta, domain_lower, domain_upper =
            Hashtbl.find specs (operation, index)
          in
          if cname <> expected_cname then
            failwith
              (Printf.sprintf "OpTuner candidate mapping mismatch at %s" site);
          let epsilon, delta = fptaylor_compatible_noise epsilon delta in
          Some
            ( site,
              epsilon,
              delta,
              Some (operation, domain_lower, domain_upper) ))
    (List.rev !occurrences)

let q_of_decimal value =
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
      | _ -> invalid_arg ("Invalid FPTaylor number: " ^ value)
    in
    let base =
      Q.make (Z.of_string (whole ^ fraction))
        (Z.pow (Z.of_int 10) (String.length fraction))
    in
    if exponent >= 0 then
      Q.mul base (Q.of_bigint (Z.pow (Z.of_int 10) exponent))
    else Q.div base (Q.of_bigint (Z.pow (Z.of_int 10) (-exponent)))

let split_widest_domain domains =
  let pattern =
    Str.regexp
      "^\\(.*in[ \\t]*\\[[ \\t]*\\)\\([^,]+\\),[ \\t]*\\([^]]+\\)\\(\\].*\\)$"
  in
  let candidates =
    List.mapi
      (fun index domain ->
        if Str.string_match pattern domain 0 then
          let lower = String.trim (Str.matched_group 2 domain) in
          let upper = String.trim (Str.matched_group 3 domain) in
          Some
            ( index,
              domain,
              Str.matched_group 1 domain,
              Str.matched_group 4 domain,
              lower,
              upper,
              Q.abs (Q.sub (q_of_decimal upper) (q_of_decimal lower)) )
        else None)
      domains
    |> List.filter_map Fun.id
  in
  match
    List.sort
      (fun (_, _, _, _, _, _, a) (_, _, _, _, _, _, b) -> Q.compare b a)
      candidates
  with
  | [] -> None
  | (index, _, prefix, suffix, lower, upper, _) :: _ ->
      let midpoint =
        (q_of_decimal lower |> fun low ->
         q_of_decimal upper |> fun high -> Q.div (Q.add low high) (Q.of_int 2))
        |> Q.to_string
      in
      let replace replacement =
        List.mapi (fun i domain -> if i = index then replacement else domain) domains
      in
      Some
        ( replace (prefix ^ lower ^ "," ^ midpoint ^ suffix),
          replace (prefix ^ midpoint ^ "," ^ upper ^ suffix) )

let rec validate_with_subdivision depth domains output_str root =
  let query =
    Parse_input.get_talyor_domain domains ^ "Definitions\n" ^ output_str
    ^ "Expressions\n  " ^ root ^ ";\n"
  in
  try
    let value = Fptaylor.get_fptaylor_power2 query |> String.trim in
    if value = "" then failwith "FPTaylor returned no exact absolute error";
    Some value
  with Failure message ->
    if depth <= 0 then (
      prerr_endline ("FPTaylor validation failed after subdivision: " ^ message);
      None)
    else
      match split_widest_domain domains with
      | None ->
          prerr_endline ("FPTaylor validation could not split domain: " ^ message);
          None
      | Some (left, right) -> (
          prerr_endline
            (Printf.sprintf "Retrying FPTaylor validation with domain subdivision (depth %d)" depth);
          match
            ( validate_with_subdivision (depth - 1) left output_str root,
              validate_with_subdivision (depth - 1) right output_str root )
          with
          | Some left_error, Some right_error ->
              if Q.compare (q_of_decimal left_error) (q_of_decimal right_error) >= 0
              then Some left_error
              else Some right_error
          | _ -> None)

let parallel_map_processes jobs f values =
  let indexed = List.mapi (fun index value -> (index, value)) values in
  let results = Array.make (List.length values) None in
  let rec take count taken remaining =
    if count <= 0 then (List.rev taken, remaining)
    else
      match remaining with
      | [] -> (List.rev taken, [])
      | head :: tail -> take (count - 1) (head :: taken) tail
  in
  let rec run remaining =
    match remaining with
    | [] -> ()
    | _ ->
        let batch, rest = take jobs [] remaining in
        let children =
          List.map
            (fun (index, value) ->
              let path = Filename.temp_file "minituner-validation-" ".marshal" in
              match Unix.fork () with
              | 0 ->
                  let result =
                    try Ok (f value) with
                    | exception_value ->
                        Error (Printexc.to_string exception_value)
                  in
                  let output = open_out_bin path in
                  Marshal.to_channel output result [];
                  close_out output;
                  Unix._exit 0
              | pid -> (pid, index, path))
            batch
        in
        List.iter
          (fun (pid, index, path) ->
            let _, status = Unix.waitpid [] pid in
            let result =
              match status with
              | Unix.WEXITED 0 -> (
                  let input = open_in_bin path in
                  let value = Marshal.from_channel input in
                  close_in input;
                  value)
              | Unix.WEXITED code ->
                  Error (Printf.sprintf "validation worker exited %d" code)
              | Unix.WSIGNALED signal ->
                  Error (Printf.sprintf "validation worker signal %d" signal)
              | Unix.WSTOPPED signal ->
                  Error (Printf.sprintf "validation worker stopped %d" signal)
            in
            Sys.remove path;
            match result with
            | Ok value -> results.(index) <- Some value
            | Error message ->
                prerr_endline ("FPTaylor validation worker failed: " ^ message))
          children;
        run rest
  in
  run indexed;
  Array.to_list results
  |> List.map (function Some value -> value | None -> "")

type hybrid_result = {
  hybrid_status : string;
  hybrid_error : string;
  hybrid_reason : string;
  hybrid_first_order : string;
  hybrid_remainder : string;
  hybrid_scalar_bound : string;
  hybrid_regions : string;
  hybrid_subdivisions : string;
  hybrid_dominant_site : string;
  hybrid_stopping_reason : string;
  hybrid_refinement_seconds : string;
}

let last_validation_statuses : (string * string) list ref = ref []
let get_last_validation_statuses () = !last_validation_statuses
let last_validation_details : hybrid_result list ref = ref []
let get_last_validation_details () = !last_validation_details

let empty_hybrid_result status reason =
  {
    hybrid_status = status;
    hybrid_error = "";
    hybrid_reason = reason;
    hybrid_first_order = "";
    hybrid_remainder = "";
    hybrid_scalar_bound = "";
    hybrid_regions = "";
    hybrid_subdivisions = "";
    hybrid_dominant_site = "";
    hybrid_stopping_reason = "";
    hybrid_refinement_seconds = "";
  }

let native_query_and_noises domains hstable root vars_in_order solution =
  let values : (string, Q.t) Hashtbl.t = Hashtbl.create (List.length solution) in
  List.iter
    (fun (expression, value) ->
      Hashtbl.replace values (Z3.Expr.to_string expression) value)
    solution;
  let noises = ref [] in
  let definitions =
    List.map
      (fun variable ->
        let epsilon_key = "|" ^ variable ^ ";epsilon|" in
        let delta_key = "|" ^ variable ^ ";delta|" in
        match
          (Hashtbl.find_opt values epsilon_key, Hashtbl.find_opt values delta_key)
        with
        | Some epsilon, Some delta ->
            noises := (variable, epsilon, delta, None) :: !noises;
            let mantissa, epsilon_exponent, delta_exponent =
              rn_eps_and_delta (Q.to_float epsilon) (Q.to_float delta)
            in
            Printf.sprintf "  %s = rnd[64,ne,%.17g,%d,%d](%s);" variable
              mantissa epsilon_exponent delta_exponent
              (Hashtbl.find hstable variable)
        | _ ->
            Printf.sprintf "  %s = rnd64(%s);" variable
              (Hashtbl.find hstable variable))
      vars_in_order
  in
  ( Parse_input.get_talyor_domain domains ^ "\nDefinitions\n"
    ^ String.concat "\n" definitions
    ^ "\n\nExpressions\n  " ^ root ^ ";\n",
    List.rev !noises )

let query_and_noises domains ast hstable root vars_in_order solution =
  if Config.is_optuner_compatible () then
    (compatibility_query domains ast solution, compatibility_noises ast solution)
  else
    native_query_and_noises domains hstable root vars_in_order solution

let noise_json noises =
  `Assoc
    (List.map
       (fun (name, epsilon, delta, domain) ->
         ( name,
           `Assoc
             ([
                ("epsilon", `String (Q.to_string epsilon));
                ("delta", `String (Q.to_string delta));
              ]
             @
             (match domain with
             | None -> []
             | Some (operation, lower, upper) ->
                 [
                   ("operation", `String operation);
                   ("domain_lower", `String lower);
                   ("domain_upper", `String upper);
                 ])) ))
       noises)

let run_satire_hybrid queries =
  let query_json =
    `Assoc
      [
        ("version", `Int 3);
        ("satire_revision", `String "dda63f321a905f8be51e3648427bafea42352d40");
        ("refinement_budget_seconds", `Float 0.2);
        ("validation_jobs", `Int (Config.get_validation_jobs ()));
        ( "queries",
          `List
            (List.map
               (fun (query, noises) ->
                 `Assoc
                   [
                     ("query", `String query);
                     ("noises", noise_json noises);
                   ])
               queries) );
      ]
  in
  let manifest_text = Yojson.Basic.to_string query_json in
  let script = Config.project_path "src/scripts/satire_validate.py" in
  let script_digest =
    if Sys.file_exists script then Digest.to_hex (Digest.file script) else "missing"
  in
  let key =
    Digest.to_hex
      (Digest.string
         ("satire-hybrid-v3-affine-refined\n" ^ Config.execution_mode_name () ^ "\n"
        ^ Config.site_semantics_name () ^ "\n" ^ script_digest ^ "\n"
        ^ manifest_text))
  in
  let cache_directory = Fptaylor.ensure_cache_directory () in
  let cache_file = Filename.concat cache_directory ("satire-" ^ key ^ ".json") in
  let manifest_file =
    Filename.concat cache_directory ("satire-manifest-" ^ key ^ ".json")
  in
  if not (Sys.file_exists manifest_file) then (
    let temporary =
      manifest_file ^ "." ^ string_of_int (Unix.getpid ()) ^ ".tmp"
    in
    Yojson.Basic.to_file temporary query_json;
    (try Sys.rename temporary manifest_file
     with Sys_error _ -> if Sys.file_exists temporary then Sys.remove temporary));
  let parse_payload payload =
    let open Yojson.Basic.Util in
    let string_member name payload =
      match payload |> member name with
      | `String value -> value
      | `Int value -> string_of_int value
      | `Float value -> Printf.sprintf "%.9g" value
      | _ -> ""
    in
    payload |> member "results" |> to_list
    |> List.map (fun result ->
           {
             hybrid_status = result |> member "status" |> to_string;
             hybrid_error = result |> member "error" |> to_string;
             hybrid_reason = result |> member "reason" |> to_string;
             hybrid_first_order = string_member "first_order" result;
             hybrid_remainder = string_member "nonlinear_remainder" result;
             hybrid_scalar_bound = string_member "scalar_bound" result;
             hybrid_regions = string_member "regions" result;
             hybrid_subdivisions = string_member "subdivisions" result;
             hybrid_dominant_site = string_member "dominant_site" result;
             hybrid_stopping_reason = string_member "stopping_reason" result;
             hybrid_refinement_seconds =
               string_member "refinement_seconds" result;
           })
  in
  if Sys.file_exists cache_file then
    Yojson.Basic.from_file cache_file |> parse_payload
  else
    match Sys.getenv_opt "MINITUNER_SATIRE_SOCKET" with
    | Some socket_path -> (
        try
          let input, output =
            Unix.open_connection (Unix.ADDR_UNIX socket_path)
          in
          output_string output manifest_text;
          output_char output '\n';
          flush output;
          let response = input_line input in
          Unix.shutdown_connection input;
          close_in_noerr input;
          close_out_noerr output;
          let payload = Yojson.Basic.from_string response in
          Yojson.Basic.to_file cache_file payload;
          parse_payload payload
        with (Unix.Unix_error _ | Sys_error _ | End_of_file) ->
          prerr_endline
            "Persistent SATIRE worker unavailable; using isolated process";
          let input_file = Filename.temp_file "minituner-satire-" ".json" in
          let output_file =
            Filename.temp_file "minituner-satire-result-" ".json"
          in
          Sys.remove output_file;
          Yojson.Basic.to_file input_file query_json;
          let python =
            Sys.getenv_opt "MINITUNER_PYTHON" |> Option.value ~default:"python3"
          in
          let command =
            String.concat " "
              (List.map Filename.quote
                 [ python; script; "--input"; input_file; "--output"; output_file ])
          in
          let status = Sys.command command in
          Sys.remove input_file;
          if status <> 0 || not (Sys.file_exists output_file) then
            List.map
              (fun _ ->
                empty_hybrid_result "ambiguous"
                  "SATIRE validator process failed")
              queries
          else
            let payload = Yojson.Basic.from_file output_file in
            Sys.remove output_file;
            Yojson.Basic.to_file cache_file payload;
            parse_payload payload)
    | None ->
    let input_file = Filename.temp_file "minituner-satire-" ".json" in
    let output_file = Filename.temp_file "minituner-satire-result-" ".json" in
    Sys.remove output_file;
    Yojson.Basic.to_file input_file query_json;
    let python =
      Sys.getenv_opt "MINITUNER_PYTHON" |> Option.value ~default:"python3"
    in
    let command =
      String.concat " "
        (List.map Filename.quote
           [ python; script; "--input"; input_file; "--output"; output_file ])
    in
    let status = Sys.command command in
    Sys.remove input_file;
    if status <> 0 || not (Sys.file_exists output_file) then
      List.map
        (fun _ ->
          empty_hybrid_result "ambiguous" "SATIRE hybrid process failed")
        queries
    else
      let payload = Yojson.Basic.from_file output_file in
      let temporary =
        cache_file ^ "." ^ string_of_int (Unix.getpid ()) ^ ".tmp"
      in
      Yojson.Basic.to_file temporary payload;
      Sys.rename temporary cache_file;
      Sys.remove output_file;
      parse_payload payload

let plot_solutions solutions math_expr domains (* Domains Might no need*) =
  (*this fucntion doesn't plot the data I tried but i couldn't get log scaling to work in x-axis
  I am not familar with plotting in ocaml so just let a python script figure it out, as such we write the data to a file after this function
  I should rename this function *)
  print_endline "Now in plotting";
  let lexbuf = Lexing.from_string math_expr in
  let ast =
    (* this is just making sure we have parsed the math_expression correctly*)
    match Math_parse.Parser.expr_option Math_parse.Lexer.read lexbuf with
    | None -> failwith "No expression parsed"
    | Some a -> a
  in
  let taylor_domain = Parse_input.get_talyor_domain domains in

  (* Add Defintions*)
  let hstable, root, vars_in_order = Parse_input.get_fptaylor_hash_table ast in

  let validate_solution (sol, _, _) =
        (* create a hashtable for this solution *)
        let ht : (string, Q.t) Hashtbl.t =
          Hashtbl.create (List.length sol)
        in

        (* fill the hashtable *)
        List.iter
          (fun (e, q) ->
            let name = Z3.Expr.to_string e in
            Hashtbl.add ht name q)
          sol;

        (* iterate over variables in order *)
        let output_str =
          List.fold_left
            (fun acc v ->
              let epsilon_key = "|" ^ v ^ ";epsilon|" in
              let delta_key = "|" ^ v ^ ";delta|" in

              let line =
                if Hashtbl.mem ht epsilon_key then
                  (* both delta and epsilon exist *)
                  let m, e, n =
                    rn_eps_and_delta
                      (Q.to_float (Hashtbl.find ht epsilon_key))
                      (Q.to_float (Hashtbl.find ht delta_key))
                  in
                  Printf.sprintf "  %s = rnd[64,ne,%0.10f,%d,%d](%s);\n" v m e n
                    (Hashtbl.find hstable v)
                else
                  (* otherwise, print normally *)
                  Printf.sprintf "  %s = rnd64(%s);\n" v
                    (Hashtbl.find hstable v)
              in
              acc ^ line)
            "" !vars_in_order
        in
        let _ = taylor_domain in
        let acc_error =
          if Config.is_optuner_compatible () then
            try
              compatibility_query domains ast sol
              |> Fptaylor.get_fptaylor_power2 |> String.trim
            with Failure message ->
              prerr_endline ("OpTuner-compatible FPTaylor validation failed: " ^ message);
              ""
          else
            match validate_with_subdivision 2 domains output_str root with
            | Some error -> error
            | None -> ""
        in

        print_string acc_error;
        acc_error
  in
  let validation_jobs = Config.get_validation_jobs () in
  if
    not (Config.is_quiet ())
    && Config.effective_validator () = Config.Fptaylor
  then
    Printf.printf "FPTaylor validation workers: %d\n" validation_jobs;
  let uses_hybrid =
    match Config.effective_validator () with
    | Config.Satire_hybrid -> true
    | Config.Fptaylor -> false
    | Config.Auto -> assert false
  in
  let errors = Array.make (List.length solutions) "" in
  let statuses = Array.make (List.length solutions) ("validation_failed", "") in
  let details =
    Array.make (List.length solutions)
      (empty_hybrid_result "validation_failed" "")
  in
  let ambiguous =
    if not uses_hybrid then List.mapi (fun index solution -> (index, solution)) solutions
    else
      let satire_started = Unix.gettimeofday () in
      let queries =
        List.map
          (fun (solution, _, _) ->
            query_and_noises domains ast hstable root !vars_in_order solution)
          solutions
      in
      let results = run_satire_hybrid queries in
      if List.length results <> List.length solutions then
        failwith "SATIRE hybrid returned the wrong number of candidate results";
      let unresolved = ref [] in
      List.iteri
        (fun index result ->
          details.(index) <- result;
          if
            result.hybrid_status = "satire_certified"
            && String.trim result.hybrid_error <> ""
          then (
            errors.(index) <- result.hybrid_error;
            statuses.(index) <-
              ("satire_certified", result.hybrid_reason))
          else if result.hybrid_status = "invalid_configuration" then
            statuses.(index) <-
              ("invalid_configuration", result.hybrid_reason)
          else
            statuses.(index) <-
              ("validation_failed", result.hybrid_reason))
        results;
      let certified =
        Array.fold_left
          (fun count (status, _) ->
            if status = "satire_certified" then count + 1 else count)
          0 statuses
      in
      prerr_endline
        (Printf.sprintf
           "SATIRE hybrid validation: %.3fs, certified=%d, ambiguous=%d, \
            fallback=%d"
           (Unix.gettimeofday () -. satire_started)
           certified
           (Array.fold_left
              (fun count (status, _) ->
                if status = "validation_failed" then count + 1 else count)
              0 statuses)
           0);
      List.rev !unresolved
  in
  let fallback_started = Unix.gettimeofday () in
  let fallback_solutions = List.map snd ambiguous in
  let fallback_errors =
    if validation_jobs <= 1 || List.length fallback_solutions <= 1 then
      List.map validate_solution fallback_solutions
    else parallel_map_processes validation_jobs validate_solution fallback_solutions
  in
  List.iter2
    (fun (index, _) error ->
      errors.(index) <- error;
      statuses.(index) <-
        if String.trim error = "" then
          ("validation_failed", snd statuses.(index))
        else ("fptaylor_certified", "");
      details.(index) <-
        empty_hybrid_result
          (if String.trim error = "" then "validation_failed"
           else "fptaylor_certified")
          (snd statuses.(index)))
    ambiguous fallback_errors;
  if uses_hybrid && ambiguous <> [] then
    prerr_endline
      (Printf.sprintf "FPTaylor fallback validation: %.3fs"
         (Unix.gettimeofday () -. fallback_started));
  last_validation_statuses := Array.to_list statuses;
  last_validation_details := Array.to_list details;
  let error_cost =
    List.map2
      (fun acc_error (sol, cost, model_error) ->
        (acc_error, cost, model_error, sol))
      (Array.to_list errors) solutions
  in
  error_cost
