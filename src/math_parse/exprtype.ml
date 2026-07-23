(*The type of expressions allowed*)
type expr =
  | Int of int
  | Float of string
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
  | Func of string * expr
  | Func2 of string * expr * expr
  | Neg of expr
