%{
open Exprtype
%}
(*Token allowed*)

%token <int> INT
%token <string> FLOAT
%token <string> VARIABLE
%token <string> FUNC
%token TIMES
%token DIV
%token PLUS
%token MINUS
%token POW
%token LPAREN
%token RPAREN
%token COMMA
%token EOF

%left PLUS MINUS
%left TIMES DIV
%right POW
%right UMINUS

%start <Exprtype.expr option> expr_option

%%

expr_option:
    | EOF { None }
    | e = expr EOF { Some e }
    ;

expr:
    | i = INT { Int i }
    | f = FLOAT { Float f }
    | v = VARIABLE { Var v }
    | e1 = expr PLUS e2 = expr { Add (e1, e2) }
    | e1 = expr MINUS e2 = expr { Sub (e1, e2) }
    | e1 = expr TIMES e2 = expr { Mul (e1, e2) }
    | e1 = expr DIV e2 = expr { Div (e1, e2) }
    | e1 = expr POW e2 = expr { Pow (e1, e2) }
    | f = FUNC LPAREN e = expr RPAREN { Func (f, e) }
    | f = FUNC LPAREN e1 = expr COMMA e2 = expr RPAREN {
        if f = "hypot" then Func ("sqrt", Add (Mul (e1, e1), Mul (e2, e2)))
        else Func2 (f, e1, e2)
      }
    | MINUS e = expr %prec UMINUS { Neg e }
    | LPAREN e = expr RPAREN { e }
