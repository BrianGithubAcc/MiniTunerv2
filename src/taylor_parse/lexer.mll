{
open Parser
}
(*What should happen when I meet some string,nice that regex is allowed here*)
let whitespace = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int_lit = ['-']? digit+
let float_lit = ['-']? digit+ '.' digit+
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
(*inv messes it up need to retranslate it*)
rule read = parse
  | whitespace { read lexbuf }
  | "*" { TIMES }
  | "/" { DIV }
  | "+" { PLUS }
  | "-" { MINUS }
  | "^" { POW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "ne"  { NE }
  | "up"  { UP }
  | "rnd64" {RND64 (Lexing.lexeme lexbuf)}
  | "inv" {INV (Lexing.lexeme lexbuf)}
  | "rnd"  { RND }
  | "["   { LBRACK }
  | "]"   { RBRACK }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | ","   { COMMA }
  |"sin"|"cos"|"tan"|"exp"|"log"|"sqrt"|"abs"|"atan"|"hypot"|"atan2" { FUNC (Lexing.lexeme lexbuf) }
  | float_lit { FLOAT (Lexing.lexeme lexbuf) }
  | int_lit   { INT (Lexing.lexeme lexbuf) }
  | id        { VARIABLE (Lexing.lexeme lexbuf) }

  | eof       { EOF }
