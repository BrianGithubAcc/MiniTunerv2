(* Global program configuration *)
let quiet_mode = ref false
let set_quiet_mode quiet = quiet_mode := quiet
let is_quiet () = !quiet_mode

type execution_mode =
  | Native
  | Optuner_fptaylor
  | Optuner_satire

let execution_mode = ref Native

let set_execution_mode = function
  | "native" -> execution_mode := Native
  | "optuner-fptaylor" -> execution_mode := Optuner_fptaylor
  | "optuner-satire" -> execution_mode := Optuner_satire
  | value -> invalid_arg ("Unknown execution mode: " ^ value)

(* Kept as the site/objective-semantics predicate used throughout the exact
   encoding.  Both OpTuner modes share structurally identical call sites. *)
let is_optuner_compatible () =
  match !execution_mode with
  | Native -> false
  | Optuner_fptaylor | Optuner_satire -> true

let is_optuner_fptaylor_mode () = !execution_mode = Optuner_fptaylor
let is_optuner_satire_mode () = !execution_mode = Optuner_satire

let execution_mode_name () =
  match !execution_mode with
  | Native -> "native"
  | Optuner_fptaylor -> "optuner_fptaylor"
  | Optuner_satire -> "optuner_satire"

let site_semantics_name () =
  if is_optuner_compatible () then "optuner_deduplicated"
  else "native_independent"

let validation_jobs = ref 1
let set_validation_jobs jobs = validation_jobs := max 1 jobs
let get_validation_jobs () = !validation_jobs

type validator = Auto | Fptaylor | Satire_hybrid

let validator = ref Auto

let set_validator = function
  | "auto" -> validator := Auto
  | "fptaylor" -> validator := Fptaylor
  | "satire-hybrid" -> validator := Satire_hybrid
  | value -> invalid_arg ("Unknown validator: " ^ value)

let get_validator () = !validator

let effective_validator () =
  match !execution_mode with
  | Optuner_fptaylor -> Fptaylor
  | Optuner_satire -> Satire_hybrid
  | Native -> (
      match !validator with Auto -> Satire_hybrid | selected -> selected)

let validator_name () =
  match effective_validator () with
  | Fptaylor -> "fptaylor"
  | Satire_hybrid -> "satire_hybrid"
  | Auto -> assert false

let project_root () =
  match Sys.getenv_opt "MINITUNER_ROOT" with
  | Some root -> root
  | None -> Filename.dirname (Sys.getcwd ())

let project_path path = Filename.concat (project_root ()) path
