(*Input varibles referance*)

let verbose = ref false
let expression_string = ref ""
let output_file = ref ""
let duplicate = ref false
let quiet_mode = ref false
let optuner_compatible = ref false
let optuner_satire_compatible = ref false
let validation_jobs = ref 1
let total_error_constraint= ref (-1.)
let total_cost_constraint= ref (-1.)
let alpha_val = ref (0.5)
let no_guidance = ref false
let guidance_points = ref 4
let z3_engine = ref "pareto"
let search_engine = ref "auto"
let validator = ref "auto"
let target_seconds = ref 10.
let z3_cost_low = ref ""
let z3_cost_high = ref ""
let output_directory = ref "output/miniTune"


(* When ./program --help is run*)
let usage_msg =
  "./program [-verbose] -e <expression> -o <output> -d <remove-duplicates>"

(*For inputs you are unsure if they will be prsent *)
let anon_fun _ = ()

(*Flags for the program*)
let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
    ("-o", Arg.Set_string output_file, "Set output file name");
    ("-e", Arg.Set_string expression_string, "Set Varible,domian and Interval");
    ("-d", Arg.Set duplicate, "Deduplicate the expression");
    ("-t", Arg.Set_float total_error_constraint, "Restrict The Total error");
    ("-c", Arg.Set_float total_cost_constraint, "Restrict Total Cost");
    (
      "--no-guidance",
      Arg.Set no_guidance,
      "Disable exact candidate cuts and run the complete baseline");
    (
      "--guidance-points",
      Arg.Set_int guidance_points,
      "Number of exact candidate points used for dominance cuts");
    ("--quiet", Arg.Set quiet_mode, "Suppress Z3 verbose output");
    (
      "--optuner-compatible",
      Arg.Set optuner_compatible,
      "Use OpTuner deduplication/objectives with FPTaylor validation");
    (
      "--optuner-satire-compatible",
      Arg.Set optuner_satire_compatible,
      "Use OpTuner deduplication/objectives with SATIRE validation");
    (
      "--validation-jobs",
      Arg.Set_int validation_jobs,
      "Concurrent FPTaylor certification processes");
    (
      "--z3-engine",
      Arg.Set_string z3_engine,
      "Exact enumeration engine: pareto (default) or staircase");
    (
      "--search-engine",
      Arg.Set_string search_engine,
      "Compatibility search: auto (default), dp, or z3");
    (
      "--validator",
      Arg.Set_string validator,
      "Certification backend: auto, fptaylor, or satire-hybrid");
    (
      "--target-seconds",
      Arg.Set_float target_seconds,
      "Performance target recorded by the suite; not a timeout");
    (
      "--z3-cost-low",
      Arg.Set_string z3_cost_low,
      "Inclusive exact lower cost bound for a solver band");
    (
      "--z3-cost-high",
      Arg.Set_string z3_cost_high,
      "Exclusive exact upper cost bound for a solver band");
    (
      "--output-dir",
      Arg.Set_string output_directory,
      "Project-relative CSV output directory");
    ("-a", Arg.Set_float alpha_val, "Set alpha value");
    (*exp(x)+sin(exp(x)) is 2 function calls instead of 3 as exp(x) is treated the same *)
  ]

(*This is what the tutorial had*)
let () = Arg.parse speclist anon_fun usage_msg

let match_math_string ref_string =
  match List.rev (String.split_on_char ';' !ref_string) with
  | h :: t -> (h, t)
  | [] -> raise (Invalid_argument "Input string is empty")

let q_of_numeric_string value =
  let value = String.trim value in
  if value = "" then
    raise (Invalid_argument "Empty rational number");
  try Q.of_string value
  with Invalid_argument _ ->
    let exponent_index =
      match String.index_opt value 'e' with
      | Some index -> Some index
      | None -> String.index_opt value 'E'
    in
    match exponent_index with
    | None -> raise (Invalid_argument ("Invalid rational number: " ^ value))
    | Some index ->
        let mantissa = Q.of_string (String.sub value 0 index) in
        let exponent =
          int_of_string
            (String.sub value (index + 1) (String.length value - index - 1))
        in
        let scale = Q.of_bigint (Z.pow (Z.of_int 10) (abs exponent)) in
        if exponent >= 0 then Q.mul mantissa scale else Q.div mantissa scale

let final_pareto_reduce data =
  let numeric =
    List.filter_map
      (fun (error, cost, model_error, solution) ->
        try
          let error_value = q_of_numeric_string error in
            Some (error, error_value, cost, model_error, solution)
        with Invalid_argument _ -> None)
      data
  in
  List.filter
    (fun (_, error, cost, _, _) ->
      not
        (List.exists
           (fun (_, other_error, other_cost, _, _) ->
             Q.compare other_error error <= 0
             && Q.compare other_cost cost <= 0
             && (Q.compare other_error error < 0
                || Q.compare other_cost cost < 0))
           numeric))
    numeric
  |> List.map (fun (error, _, cost, model_error, solution) ->
         (error, cost, model_error, solution))
  |> List.sort (fun (_, cost_a, _, _) (_, cost_b, _, _) ->
         Q.compare cost_a cost_b)

let is_choice_variable name =
  let suffix suffix =
    let name_length = String.length name and suffix_length = String.length suffix in
    name_length >= suffix_length
    && String.sub name (name_length - suffix_length) suffix_length = suffix
  in
  suffix ";epsilon|" || suffix ";delta|"

let csv_choice_name name =
  name |> String.to_seq
  |> Seq.filter (fun character -> character <> '|' && character <> ';')
  |> String.of_seq
  |> fun compact ->
  match String.index_opt compact 'e' with
  | Some index when String.sub compact index (String.length compact - index) = "epsilon" ->
      String.sub compact 0 index ^ "_epsilon"
  | _ -> (
      match String.index_opt compact 'd' with
      | Some index -> String.sub compact 0 index ^ "_delta"
      | None -> compact)

let choice_columns data =
  data
  |> List.concat_map (fun (_, _, _, solution) ->
         List.filter_map
           (fun (expression, _) ->
             let name = Z3.Expr.to_string expression in
             if is_choice_variable name then Some name else None)
           solution)
  |> List.sort_uniq String.compare

let find_choice solution name =
  List.find_map
    (fun (expression, value) ->
      if Z3.Expr.to_string expression = name then Some value else None)
    solution

let unquote_z3_name name =
  let length = String.length name in
  if length >= 2 && name.[0] = '|' && name.[length - 1] = '|' then
    String.sub name 1 (length - 2)
  else name

let compatibility_assignment solution =
  solution
  |> List.filter_map (fun (expression, value) ->
         if not (Q.equal value Q.one) then None
         else
           match
             String.split_on_char ';'
               (unquote_z3_name (Z3.Expr.to_string expression))
           with
           | [ "choice"; site; cname; _ ] -> Some (site, `String cname)
           | _ -> None)
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  |> fun fields -> `Assoc fields |> Yojson.Basic.to_string

let csv_quote value =
  "\"" ^ String.concat "\"\"" (String.split_on_char '"' value) ^ "\""

let output_path filename =
  let directory =
    if Filename.is_relative !output_directory then
      Config.project_path !output_directory
    else !output_directory
  in
  Filename.concat directory filename

(*Main*)
let () =
  let preprocessing_start = Unix.gettimeofday () in
  Config.set_quiet_mode !quiet_mode;
  if !optuner_compatible && !optuner_satire_compatible then
    invalid_arg
      "--optuner-compatible and --optuner-satire-compatible are mutually exclusive";
  let selected_mode =
    if !optuner_compatible then "optuner-fptaylor"
    else if !optuner_satire_compatible then "optuner-satire"
    else "native"
  in
  Config.set_execution_mode selected_mode;
  Config.set_validation_jobs !validation_jobs;
  Config.set_validator !validator;
  if Config.is_optuner_compatible () && !validator <> "auto" then
    invalid_arg
      "--validator is a native-mode testing override; compatibility modes lock the validator";
  if Config.is_optuner_compatible () && !duplicate then
    invalid_arg "OpTuner compatibility modes cannot be combined with -d";
  (*Should of parsed teh experssion at the top-level*)
  let math_expression, domains = match_math_string expression_string in
  let ast =
    let lexbuf = Lexing.from_string math_expression in
    match Math_parse.Parser.expr_option Math_parse.Lexer.read lexbuf with
    | Some expression -> expression
    | None -> failwith "No expression parsed"
  in
  let fptaylor_preprocess () =
    let fptaylor_query, num_var =
        Parse_input.build_fptaylor_query domains math_expression
    in
    if not (Config.is_quiet ()) then print_string fptaylor_query;
    let cache_key =
      let source_digest name =
        let path = Config.project_path ("src/" ^ name) in
        if Sys.file_exists path then Digest.to_hex (Digest.file path)
        else "missing"
      in
      Digest.to_hex
        (Digest.string
           ("exact-analysis-v3\n" ^ Config.execution_mode_name () ^ "\n"
          ^ Config.site_semantics_name () ^ "\n" ^ Config.validator_name ()
          ^ "\n" ^ Fptaylor.flags ^ "\n" ^ fptaylor_query ^ "\n"
          ^ String.concat ";" domains ^ "\n"
          ^ source_digest "fptaylor.ml" ^ "\n"
          ^ source_digest "gelpia.ml" ^ "\n"
          ^ source_digest "gelpia_wrapper" ^ "\n"
          ^ source_digest "parse_input.ml"))
    in
    let cache_file =
      Filename.concat (Fptaylor.ensure_cache_directory ())
        ("exact-analysis-" ^ cache_key ^ ".marshal")
    in
    let model =
      if Sys.file_exists cache_file then (
        let input = open_in_bin cache_file in
        let model = Marshal.from_channel input in
        close_in input;
        prerr_endline ("Exact analysis cache hit: " ^ cache_key);
        model)
      else
        let taylor_string, sub_string =
          Fptaylor.run_fptaylor_query_get_indices fptaylor_query
        in
        let model_error = Fptaylor.get_model taylor_string sub_string in
        let model = Gelpia.get_max_model model_error domains in
        let temporary =
          cache_file ^ "." ^ string_of_int (Unix.getpid ()) ^ ".tmp"
        in
        let output = open_out_bin temporary in
        Marshal.to_channel output model [];
        close_out output;
        Sys.rename temporary cache_file;
        model
    in
    (model, None, num_var)
  in
  let model_max_error, fast_analysis, num_var =
    if Config.effective_validator () = Config.Satire_hybrid then
      try
        let analysis = Satire_model.analyse domains ast in
        prerr_endline
          (Printf.sprintf
             "SATIRE DAG preprocessing: nodes=%d, FPTaylor/Gelpia calls=0"
             analysis.node_count);
        (analysis.error_table, Some analysis, analysis.node_count)
      with Satire_model.Unresolved reason ->
        prerr_endline
          ("SATIRE preprocessing unresolved; explicit FPTaylor fallback: "
         ^ reason);
        fptaylor_preprocess ()
    else fptaylor_preprocess ()
  in
  prerr_endline
    (Printf.sprintf "Exact preprocessing: %.3fs"
       (Unix.gettimeofday () -. preprocessing_start));
  (* The expression should be aprsed into ast once :/ doing it >twice is not optimal *)
  Z3_solver.set_guidance_points
    (if !no_guidance then 0 else !guidance_points);
  Z3_solver.set_z3_engine !z3_engine;
  Z3_solver.set_search_engine !search_engine;
  Z3_solver.set_z3_cost_band
    (if !z3_cost_low = "" then None else Some !z3_cost_low)
    (if !z3_cost_high = "" then None else Some !z3_cost_high);
  let parteo_sols =
    Z3_solver.patero_solution ?fast_analysis math_expression domains
      model_max_error !duplicate num_var !total_error_constraint
      !total_cost_constraint !alpha_val
  in
  let validation_start = Unix.gettimeofday () in
  let raw_data = Plot_data.plot_solutions parteo_sols math_expression domains in
  let validation_statuses = Plot_data.get_last_validation_statuses () in
  let validation_details = Plot_data.get_last_validation_details () in
  if List.length validation_statuses <> List.length raw_data then
    failwith "validation status/result count mismatch";
  if List.length validation_details <> List.length raw_data then
    failwith "validation detail/result count mismatch";
  prerr_endline
    (Printf.sprintf "%s validation: %.3fs"
       (match Config.effective_validator () with
       | Config.Satire_hybrid -> "SATIRE"
       | Config.Fptaylor -> "FPTaylor"
       | Config.Auto -> assert false)
       (Unix.gettimeofday () -. validation_start));
  let data = final_pareto_reduce raw_data in
  prerr_endline
    (Printf.sprintf
       "Certified frontier reduction: candidates=%d, feasible=%d, pareto=%d"
       (List.length raw_data)
       (List.fold_left
          (fun count (error, _, _, _) ->
            if String.trim error = "" then count else count + 1)
          0 raw_data)
       (List.length data));
  if data = [] then
    failwith
      "the rigorous validator produced no feasible certified frontier points";
  let diagnostic_columns = choice_columns raw_data in
  let diagnostic_path =
    output_path (!output_file ^ "_diagnostics.csv")
  in
  let diagnostic_oc = open_out diagnostic_path in
  if Config.is_optuner_compatible () then
    output_string diagnostic_oc
      "benchmark,site_semantics,validator,kind,validation_status,reason,cost,cost_exact,model_error,\
       model_error_exact,validated_error,validated_error_exact,first_order,nonlinear_remainder,\
       scalar_bound,regions,subdivisions,dominant_site,stopping_reason,refinement_seconds,assignment"
  else (
    output_string diagnostic_oc
      "error,cost,error_exact,cost_exact,site_semantics,validator,validation_status,reason,\
       first_order,nonlinear_remainder,scalar_bound,regions,subdivisions,dominant_site,\
       stopping_reason,refinement_seconds";
    List.iter
      (fun column ->
        Printf.fprintf diagnostic_oc ",%s" (csv_choice_name column))
      diagnostic_columns);
  output_char diagnostic_oc '\n';
  List.iter2
    (fun ((acc_error, cost, model_error, solution), (validation_status, validation_reason))
         detail ->
      let valid = String.trim acc_error <> "" in
      if Config.is_optuner_compatible () then
        Printf.fprintf diagnostic_oc
          "%s,%s,%s,candidate,%s,%s,%.17g,%s,%.17g,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s"
          !output_file (Config.site_semantics_name ()) (Config.validator_name ())
          validation_status (csv_quote validation_reason)
          (Q.to_float cost) (Q.to_string cost) (Q.to_float model_error)
          (Q.to_string model_error) (if valid then String.trim acc_error else "")
          (if valid then String.trim acc_error else "")
          detail.Plot_data.hybrid_first_order
          detail.Plot_data.hybrid_remainder
          detail.Plot_data.hybrid_scalar_bound
          detail.Plot_data.hybrid_regions
          detail.Plot_data.hybrid_subdivisions
          (csv_quote detail.Plot_data.hybrid_dominant_site)
          (csv_quote detail.Plot_data.hybrid_stopping_reason)
          detail.Plot_data.hybrid_refinement_seconds
          (compatibility_assignment solution |> csv_quote)
      else (
        Printf.fprintf diagnostic_oc "%s,%.10f,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s"
          (if valid then acc_error else "") (Q.to_float cost)
          (if valid then String.trim acc_error else "") (Q.to_string cost)
          (Config.site_semantics_name ()) (Config.validator_name ())
          validation_status (csv_quote validation_reason)
          detail.Plot_data.hybrid_first_order
          detail.Plot_data.hybrid_remainder
          detail.Plot_data.hybrid_scalar_bound
          detail.Plot_data.hybrid_regions
          detail.Plot_data.hybrid_subdivisions
          (csv_quote detail.Plot_data.hybrid_dominant_site)
          (csv_quote detail.Plot_data.hybrid_stopping_reason)
          detail.Plot_data.hybrid_refinement_seconds;
        List.iter
          (fun column ->
            match find_choice solution column with
            | Some value ->
                Printf.fprintf diagnostic_oc ",%.17g" (Q.to_float value)
            | None -> output_char diagnostic_oc ',')
          diagnostic_columns);
      output_char diagnostic_oc '\n')
    (List.combine raw_data validation_statuses)
    validation_details;
  close_out diagnostic_oc;
  let unresolved_count =
    List.fold_left
      (fun count (status, _) ->
        if status = "validation_failed" then count + 1 else count)
      0 validation_statuses
  in
  if unresolved_count > 0 then
    failwith
      (Printf.sprintf
         "%d potentially relevant candidate(s) remain unresolved; refusing to \
          write a complete frontier"
         unresolved_count);
  (*Open file*)
  let oc =
    open_out
      (output_path (!output_file ^ ".csv"))
  in

  (*write collums into csv file *)
  let columns = choice_columns data in
  if Config.is_optuner_compatible () then
    output_string oc
      "benchmark,site_semantics,validator,cost,error,cost_exact,error_exact,model_error,\
       model_error_exact,assignment"
  else (
    output_string oc "error,cost,error_exact,cost_exact,site_semantics,validator";
    List.iter
      (fun column -> Printf.fprintf oc ",%s" (csv_choice_name column))
      columns);
  output_char oc '\n';

  (* Write the lists data with 60 d.p (don't need but may keep)*)
  List.iter
    (fun (acc_error, cost, model_error, solution) ->
      (*Sometime FPTaylor does not produce an error in that case don't write to file*)
      if acc_error = "" then ()
      else
        let cost_f = Q.to_float cost in
        if Config.is_optuner_compatible () then
          Printf.fprintf oc "%s,%s,%s,%.17g,%s,%s,%s,%.17g,%s,%s" !output_file
            (Config.site_semantics_name ()) (Config.validator_name ())
            cost_f (String.trim acc_error) (Q.to_string cost)
            (String.trim acc_error) (Q.to_float model_error)
            (Q.to_string model_error)
            (compatibility_assignment solution |> csv_quote)
        else (
          Printf.fprintf oc "%s,%.10f,%s,%s,%s,%s" acc_error cost_f
            (String.trim acc_error) (Q.to_string cost)
            (Config.site_semantics_name ()) (Config.validator_name ());
          List.iter
            (fun column ->
              match find_choice solution column with
              | Some value -> Printf.fprintf oc ",%.17g" (Q.to_float value)
              | None -> output_char oc ',')
            columns);
        output_char oc '\n')
    data;

  (*beat 3m56.962s*)
  (*With flag 0m4.830s*)

  (*Close file*)
  close_out oc;
  ()
