(* Flags for FPTaylor*)
open Taylor_parse.Taylortype
open Expr_data

let flags =
  "--abs-error false --const-approx-real-vars false --find-bounds false \
   --fp-power2-model false --intermediate-opt false --opt gelpia --opt-approx \
   false --opt-exact false --opt-f-abs-tol 0.2 --opt-f-rel-tol 0.1 \
   --opt-max-iters 0 --opt-timeout 10000 --opt-x-abs-tol 0.2 --opt-x-rel-tol \
   0.0 --rel-error false --rel-error-threshold 0.0001 --uncertainty false \
   --unique-indices true --all-indices true --debug false --print-hex-floats \
   true --print-opt-lower-bounds true --print-second-order-errors true \
   --verbosity 2"

let native_power_flags =
  "--abs-error true --verbosity 2 --fp-power2-model true \
   --intermediate-opt true --opt-exact true --uncertainty true"

let optuner_power_flags =
  "--abs-error true --all-indices true --const-approx-real-vars false \
   --debug false --fail-on-exception false --find-bounds false \
   --fp-power2-model true --intermediate-opt true --opt gelpia \
   --opt-approx false --opt-exact true --opt-f-abs-tol 0.2 \
   --opt-f-rel-tol 0.1 --opt-max-iters 0 --opt-timeout 10000 \
   --opt-x-abs-tol 0.2 --opt-x-rel-tol 0.0 \
   --print-hex-floats true --print-opt-lower-bounds true \
   --print-second-order-errors true --rel-error false \
   --rel-error-threshold 0.0001 --uncertainty true \
   --unique-indices false --verbosity 2"

let power_flags () =
  if Config.is_optuner_compatible () then optuner_power_flags
  else native_power_flags

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path
      |> Array.iter (fun child -> remove_tree (Filename.concat path child));
      Unix.rmdir path)
    else Sys.remove path

let create_work_directory prefix =
  let path = Filename.temp_file prefix ".work" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  path

(*Honestly make a new fucntion for this as is easiesit*)
let run_fptaylor_power2_all file_query =
  let work_directory = create_work_directory "fptaylor-validation-" in
  let tmp_file = Filename.concat work_directory "query.fptaylor" in
  let () =
    let oc = open_out tmp_file in
    output_string oc file_query;
    close_out oc
  in

  let command =
    Printf.sprintf "cd %s && fptaylor %s query.fptaylor"
      (Filename.quote work_directory) (power_flags ())
  in
  if not (Config.is_quiet ()) then (
    print_endline file_query;
    Printf.printf "Running command: %s\n" command);

  let ic = Unix.open_process_in command in

  let rec process_lines ic result =
    match input_line ic with
    | exception End_of_file -> List.rev result
    | line ->
        let result =
          let line_trim = String.trim line in
          match String.split_on_char ':' line_trim with
          | [ label; value ] when String.trim label = "Absolute error (exact)" ->
              let first_word =
                List.hd (String.split_on_char ' ' (String.trim value))
              in
              first_word :: result
          | _ -> result
        in
        (* FPTaylor is verbose. Keep draining its output after finding the
           value, otherwise closing the pipe early kills it with SIGPIPE. *)
        process_lines ic result
  in

  let errors = process_lines ic [] in

  let status = Unix.close_process_in ic in
  remove_tree work_directory;

  match status with
  | Unix.WEXITED 0 -> errors
  | Unix.WEXITED 141 -> errors (*Ignore the warnings as they are not important*)
  | Unix.WEXITED n -> failwith (Printf.sprintf "FPTaylor exited with code %d" n)
  | Unix.WSIGNALED n ->
      failwith (Printf.sprintf "FPTaylor killed by signal %d" n)
  | Unix.WSTOPPED n ->
      failwith (Printf.sprintf "FPTaylor stopped by signal %d" n)

let run_fptaylor_power2 file_query =
  match run_fptaylor_power2_all file_query with
  | error :: _ -> error
  | [] -> ""

let ensure_cache_directory () =
  let approx = Config.project_path ".cache" in
  let cache = Filename.concat approx "minituner" in
  if not (Sys.file_exists approx) then Unix.mkdir approx 0o755;
  if not (Sys.file_exists cache) then Unix.mkdir cache 0o755;
  cache

let get_fptaylor_power2 file_query =
  let key =
    Digest.to_hex (Digest.string (power_flags () ^ "\n" ^ file_query))
  in
  let cache_file =
    Filename.concat (ensure_cache_directory ()) ("fptaylor-power2-" ^ key ^ ".txt")
  in
  if Sys.file_exists cache_file then (
    let input = open_in cache_file in
    let value = input_line input in
    close_in input;
    prerr_endline ("FPTaylor cache hit: " ^ key);
    value)
  else
    let value = run_fptaylor_power2 file_query in
    if String.trim value <> "" then (
      let temporary =
        cache_file ^ "." ^ string_of_int (Unix.getpid ()) ^ ".tmp"
      in
      let output = open_out temporary in
      output_string output (String.trim value ^ "\n");
      close_out output;
      Sys.rename temporary cache_file);
    value

let get_fptaylor_power2_batch file_query =
  let key =
    Digest.to_hex
      (Digest.string ("batch\n" ^ power_flags () ^ "\n" ^ file_query))
  in
  let cache_file =
    Filename.concat (ensure_cache_directory ())
      ("fptaylor-power2-batch-" ^ key ^ ".txt")
  in
  if Sys.file_exists cache_file then (
    let input = open_in cache_file in
    let rec read result =
      match input_line input with
      | line -> read (line :: result)
      | exception End_of_file -> List.rev result
    in
    let values = read [] in
    close_in input;
    prerr_endline ("FPTaylor batch cache hit: " ^ key);
    values)
  else
    let values = run_fptaylor_power2_all file_query in
    if values <> [] then (
      let temporary =
        cache_file ^ "." ^ string_of_int (Unix.getpid ()) ^ ".tmp"
      in
      let output = open_out temporary in
      List.iter (fun value -> output_string output (String.trim value ^ "\n")) values;
      close_out output;
      Sys.rename temporary cache_file);
    values

let run_fptaylor_query_get_indices file_query =
  let work_directory = create_work_directory "fptaylor-analysis-" in
  let tmp_file = Filename.concat work_directory "query.fptaylor" in
  let () =
    let oc = open_out tmp_file in
    output_string oc file_query;
    close_out oc
  in

  let command =
    Printf.sprintf "cd %s && fptaylor %s query.fptaylor"
      (Filename.quote work_directory) flags
  in
  (*Printf.printf "Running command: %s\n" command;*)

  let ic = Unix.open_process_in command in
  let taylor_buf = Buffer.create 1024 in
  let original_buf = Buffer.create 1024 in

  (* Skip n lines *)
  let rec skip_n_lines ic n =
    if n <= 0 then ()
    else
      match input_line ic with
      | exception End_of_file -> ()
      | _ -> skip_n_lines ic (n - 1)
  in

  (* Collect lines until blank line *)
  let rec collect_until_blank ic buf =
    match input_line ic with
    | exception End_of_file -> ()
    | line ->
        if String.trim line = "" then ()
        else begin
          Buffer.add_string buf (line ^ "\n");
          collect_until_blank ic buf
        end
  in

  (* Process all lines *)
  let rec process_lines ic =
    match input_line ic with
    | exception End_of_file -> ()
    | line ->
        let line_trim = String.trim line in
        if line_trim = "success" then begin
          skip_n_lines ic 1;
          (* Skip 2 lines *)
          collect_until_blank ic taylor_buf;
          (* Collect Taylor part *)
          process_lines ic
        end
        else if line_trim = "Corresponding original subexpressions:" then begin
          (*skip_n_lines ic 0;*)
          (* Skip 1 line *)
          collect_until_blank ic original_buf;
          (* Collect original subexpressions *)
          process_lines ic
        end
        else process_lines ic
  in

  process_lines ic;

  let status = Unix.close_process_in ic in
  remove_tree work_directory;
  if not (Config.is_quiet ()) then (
    print_endline (Buffer.contents taylor_buf);
    print_endline (Buffer.contents original_buf));

  match status with
  | Unix.WEXITED 0 -> (Buffer.contents taylor_buf, Buffer.contents original_buf)
  | Unix.WEXITED n -> failwith (Printf.sprintf "FPTaylor exited with code %d" n)
  | Unix.WSIGNALED n ->
      failwith (Printf.sprintf "FPTaylor killed by signal %d" n)
  | Unix.WSTOPPED n ->
      failwith (Printf.sprintf "FPTaylor stopped by signal %d" n)

(*  let parse_fptaylor_result fptaylor_restult=()

the first part looks like the epsilon
second aprt is the delta,higher order and the 
last is the actual expression
a reverse order to amtch ?

Use the bottom part of the orginal subexpressionsas a custom ahstable key
{exp :{higher,delta,epsilon}}
Negative values for missing data???

(* The oginal subepxressions is the subexpression the number 
  in the elft tells you what error to map it to at the top and what type e.g ne delta ,up higher*)
  Assume delta first always then hgiher order after
  Eplsion is jsut the expression itself???

*)
(*You need to get the rounding type of the orginal string*)
(* You then need to simplfy it on the right (remove round)*)
(*then match the detla or higher oder or epsilon*)

let preprocess_line line n =
  let parts = String.split_on_char ':' line in
  let first = try List.nth parts 0 with _ -> "" in
  let nth = try List.nth parts n with _ -> "" in
  (String.trim first, String.trim nth)

let parse_expr line =
  let line_trimmed = String.trim line in
  let lexbuf = Lexing.from_string line_trimmed in
  match Taylor_parse.Parser.expr_option Taylor_parse.Lexer.read lexbuf with
  | Some e -> e
  | None ->
      Printf.eprintf "Parser returned None for line: %S\n" line_trimmed;
      failwith "Empty expression"

let rec extract_rounding = function
  | Rnd (_, m, _, _, _, _) -> Some m
  | Add (a, b) | Pow (a, b) | Sub (a, b) | Div (a, b) | Mul (a, b) -> (
      match extract_rounding a with
      | Some x -> Some x
      | None -> extract_rounding b)
  | Func (_, e) -> extract_rounding e
  | Func2 (_, a, b) -> (
      match extract_rounding a with Some _ as result -> result | None -> extract_rounding b)
  | _ -> None

let rec expr_to_string = function
  | IntZ n -> Z.to_string n
  | Q q -> Q.to_string q
  | Var v -> v
  | Neg e -> "-(" ^ expr_to_string e ^ ")"
  | Add (a, b) -> "(" ^ expr_to_string a ^ " + " ^ expr_to_string b ^ ")"
  | Sub (a, b) -> "(" ^ expr_to_string a ^ " - " ^ expr_to_string b ^ ")"
  | Mul (a, b) -> "(" ^ expr_to_string a ^ " * " ^ expr_to_string b ^ ")"
  | Div (a, b) -> "(" ^ expr_to_string a ^ " / " ^ expr_to_string b ^ ")"
  | Pow (a, b) -> expr_to_string a ^ " ^ " ^ expr_to_string b
  | Func (f, e) -> (
      match (f, e) with
      | "log", Add (_, IntZ z) when Z.equal z Z.one -> f ^ expr_to_string e
      | "log", Add (IntZ z, _) when Z.equal z Z.one -> f ^ expr_to_string e
      | _ -> f ^ "(" ^ expr_to_string e ^ ")")
  | Func2 (f, a, b) ->
      f ^ "(" ^ expr_to_string a ^ ", " ^ expr_to_string b ^ ")"
  | Rnd64 (_, e) -> expr_to_string e
  | Inv (_, e) ->
      "1/(" ^ expr_to_string e
      ^ ")" (* Get rid of Inv as only FPTaylor uses it*)
  | Rnd (_, _, _, _, _, e) -> expr_to_string e

(*Why is rnd64 missing in the parse tree*)
let rec extract_math_expr = function
  | Rnd (_, _, _, _, _, e) -> extract_math_expr e (*Remove rnd*)
  | Add (a, b) -> Add (extract_math_expr a, extract_math_expr b)
  | Sub (a, b) -> Sub (extract_math_expr a, extract_math_expr b)
  | Mul (a, b) -> Mul (extract_math_expr a, extract_math_expr b)
  | Div (a, b) -> Div (extract_math_expr a, extract_math_expr b)
  | Pow (a, b) -> Pow (extract_math_expr a, extract_math_expr b)
  | Func (f, e) -> Func (f, extract_math_expr e)
  | Func2 (f, a, b) -> Func2 (f, extract_math_expr a, extract_math_expr b)
  | other -> other

module ExprMap = Map.Make (String)

type expr_data = Expr_data.expr_data

let get_model taylor_string original_string =
  (* split into lines *)
  let t_lines =
    String.split_on_char '\n' taylor_string
    |> List.filter (fun l -> String.trim l <> "")
  in
  let o_lines =
    String.split_on_char '\n' original_string
    |> List.filter (fun l -> String.trim l <> "")
  in

  (* Holde delta,epsilons,higher order and the max versions *)
  let dict : (string, expr_data) Hashtbl.t = Hashtbl.create 32 in

  (* Iter as we wnat to change hash map (side effect)?*)
  List.iter2
    (fun t_line o_line ->
      let indice_str, expr_line = preprocess_line t_line 2 in
      let _, string_work = preprocess_line o_line 1 in
      let indice = int_of_string indice_str in
      if indice < 0 then
        let ast_o = parse_expr string_work in
        let math_expr = extract_math_expr ast_o in
        let key = expr_to_string math_expr in

        let round =
          match extract_rounding ast_o with
          | Some r -> r
          | None -> failwith ("No rounding found in line: " ^ o_line)
        in

        let data =
          match Hashtbl.find_opt dict key with
          | Some d -> d
          | None ->
              let d =
                {
                  delta = None;
                  higher = None;
                  epsilon = None;
                  max_delta = None;
                  max_higher = None;
                  max_epsilon = None;
                }
              in
              Hashtbl.add dict key d;
              d
        in
        (* Assign batch on rounding
        deta is ne
        higher is up idk why*)
        match round with
        | NE -> data.delta <- Some expr_line
        | UP -> data.higher <- Some expr_line
        (* epsilon is always the Taylor value itself *)
      else
        let key = expr_to_string (parse_expr string_work) in
        if not (Config.is_quiet ()) then print_endline ("KeyFP:" ^ key);
        let _, o_error = preprocess_line t_line 2 in

        (*Printf.eprintf "Epsilon key: %s\n" key;
        Printf.eprintf "Epsilon value (raw): %s\n" expr_line;*)

        (* lookup or create entry in dictionary *)
        let data =
          match Hashtbl.find_opt dict key with
          | Some d -> d
          | None ->
              (* There is nothing just do this*)
              let d =
                {
                  delta = None;
                  higher = None;
                  epsilon = None;
                  max_delta = None;
                  max_higher = None;
                  max_epsilon = None;
                }
              in
              Hashtbl.add dict key d;
              d
        in
        (* store epsilon *)
        if not (Config.is_quiet ()) then print_endline "DOwn here";
        print_string o_error;
        data.epsilon <- Some o_error
      (*Printf.eprintf "Stored epsilon for key %s\n" key*))
    t_lines o_lines;
  dict
