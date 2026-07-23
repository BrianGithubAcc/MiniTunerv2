{
open Parser
}
(*What should happen when I meet some string,nice that regex is allowed here*)
let whitespace = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int_lit = ['-']? digit+
let float_lit = ['-']? digit+ '.' digit+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | whitespace { read lexbuf }
  | "*" { TIMES }
  | "/" { DIV }
  | "+" { PLUS }
  | "-" { MINUS }
  | "^" { POW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "sin"|"cos"|"tan"|"exp"|"log"|"log1p"|"expm1"|"sqrt"|"abs"|"fabs"|"atan"|"hypot"|"atan2" { FUNC (Lexing.lexeme lexbuf) }
  | float_lit { FLOAT (Lexing.lexeme lexbuf) }
  | int_lit   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id        { VARIABLE (Lexing.lexeme lexbuf) }
  | eof       { EOF }
