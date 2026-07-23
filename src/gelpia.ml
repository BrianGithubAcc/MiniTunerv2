(* This should be shared in a file*)
open Expr_data
open Taylor_parse.Taylortype

type expr_data = Expr_data.expr_data

let get_gelpia_domain lst =
  String.concat ";"
    (List.map (fun s -> Str.global_replace (Str.regexp_string "in") "=" s) lst)
  ^ ";"
(*Fptaylor shares a similar parse thing*)

(*This is just a dfs to get rid of bracekts for operations*)

let rec expr_to_string = function
  | IntZ n -> Z.to_string n
  | Q q -> Q.to_string q
  | Var v -> v
  | Neg e -> "-" ^ expr_to_string e
  | Add (a, b) -> expr_to_string a ^ " + " ^ expr_to_string b
  | Sub (a, b) -> expr_to_string a ^ " - " ^ expr_to_string b
  | Mul (a, b) -> expr_to_string a ^ " * " ^ expr_to_string b
  | Div (a, b) -> expr_to_string a ^ " / " ^ expr_to_string b
  | Pow (a, b) -> expr_to_string a ^ " ^ " ^ expr_to_string b
  | Func (f, e) -> f ^ "(" ^ expr_to_string e ^ ")"
  | Func2 (f, a, b) ->
      f ^ "(" ^ expr_to_string a ^ ", " ^ expr_to_string b ^ ")"
  | Rnd64 (_, e) -> expr_to_string e
  | Inv (_, e) ->
      "1/(" ^ expr_to_string e
      ^ ")" (* Get rid of Inv as only FPTaylor uses it*)
  | Rnd (_, _, _, _, _, e) -> expr_to_string e

(*We have to remove the brackets for some unkown reason,email author?*)
(*It has literally no effect ??? I would rather not then*)
let remove_ordering math_expr =
  let ast =
    (* this is just making sure we have parsed the math_expression correctly*)
    match
      Taylor_parse.Parser.expr_option Taylor_parse.Lexer.read
        (Lexing.from_string math_expr)
    with
    | None -> failwith "No expression parsed"
    | Some a -> a
  in
  expr_to_string ast

let gelpia_result query domains isRealDomain =
  let python_bin =
    match Sys.getenv_opt "MINITUNER_PYTHON" with
    | Some path -> path
    | None -> "python3"
  in
  let wrapper = Config.project_path "src/gelpia_wrapper" in
  let command =
    Filename.quote python_bin ^ " " ^ Filename.quote wrapper ^ " "
    ^ (if isRealDomain then "-R " else "")
    ^ " -f " ^ Filename.quote (get_gelpia_domain domains ^ query)
  in
  if not (Config.is_quiet ()) then (
    print_string "\nCOmmand is\n";
    print_string (command ^ "\n")
  );
  let buf = Buffer.create 1024 in
  let ic = Unix.open_process_in command in

  (* process all lines and store in buffer *)
  let rec process_lines () =
    match input_line ic with
    | exception End_of_file -> ()
    | line ->
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
        process_lines ()
  in
  process_lines ();

  (* close process,check status *)
  let status = Unix.close_process_in ic in

  if not (Config.is_quiet ()) then
    print_string (Buffer.contents buf);
  match status with
  | Unix.WEXITED 0 -> Buffer.contents buf
  | Unix.WEXITED n -> failwith (Printf.sprintf "Gelpia exited with code %d" n)
  | Unix.WSIGNALED n -> failwith (Printf.sprintf "Gelpia killed by signal %d" n)
  | Unix.WSTOPPED n -> failwith (Printf.sprintf "Gelpia stopped by signal %d" n)

let opt s = match s with Some x -> x | None -> "None"

let get_max_model model_error domains =
  let isRealDomain = false in
  Hashtbl.iter
    (fun key data ->
      print_endline "";
      if not (Config.is_quiet ()) then (
        print_endline ("                 |Delta:" ^ opt data.delta);
        print_endline ("Key:" ^ key ^ "= |Epsilon:" ^ opt data.epsilon);
        print_endline ("                 |Higher:" ^ opt data.higher);
        print_endline "";
        print_endline ("Key:" ^ key)
      );

      (* call Gelpia to fill max fields only if input is Some *)
      (match data.delta with
      | Some q ->
          data.max_delta <-
            Some (gelpia_result ( q) domains isRealDomain) (* incase it breaks remove_ordering q *)
      | None -> ());
      (match data.higher with
      | Some q ->
          data.max_higher <-
            Some (gelpia_result (  q) domains isRealDomain)
      | None -> ());
      (match data.epsilon with
      | Some q ->
          if not (Config.is_quiet ()) then (
            print_string "Result of that";
            print_string (  q)
          );
          data.max_epsilon <-
            Some (gelpia_result ( q) domains isRealDomain)
      | None -> ());

      print_endline "";
      if not (Config.is_quiet ()) then (
        print_endline ("                 |Max_Delta:" ^ opt data.max_delta);
        print_endline ("Key:" ^ key ^ "= |Max_Epsilon:" ^ opt data.max_epsilon);
        print_endline ("                 |Max_Higher:" ^ opt data.max_higher);
        print_endline ""
      );

      ())
    model_error;

  (*prerr_string "Gelpia print complete\n";*)
  model_error
(*amend to output the dict instead of unit()*)
(*./gelpia_wrapper -f "x = [0.0078125,0.5];exp(x) - 1 * -x / x * x + 1 / x * exp(x) * x"*)
