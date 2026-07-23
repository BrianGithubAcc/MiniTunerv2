type rounding_mode = NE | UP

(*Deal with fractions *)
type expr =
  | IntZ of Z.t
  | Q of Q.t
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
  | Func of string * expr
  | Func2 of string * expr * expr
  | Inv of string * expr
  | Rnd64 of string * expr
  | Rnd of int * rounding_mode * Q.t * int * int * expr
  | Neg of expr
