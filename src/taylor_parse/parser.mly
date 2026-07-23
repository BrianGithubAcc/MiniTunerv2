%{
open Taylortype
%}

%token <string> INT
%token <string> FLOAT
%token <string> VARIABLE
%token <string> FUNC
%token <string> INV
%token <string> RND64
%token TIMES
%token DIV
%token PLUS
%token MINUS
%token POW
%token LPAREN
%token RPAREN
%token RND 
%token NE 
%token UP
%token LBRACK
%token RBRACK
%token COMMA
%token EOF

%left PLUS MINUS
%left TIMES DIV
%right POW

%type <Taylortype.rounding_mode> mode
%start <Taylortype.expr option> expr_option

%%

expr_option:
  | EOF { None }
  | e = expr EOF { Some e }

expr:
    (*For now ingnore the minus I need to try get OpTuner running again to see the real solution*)
    | MINUS e = expr { Neg e }
    | i = INT { IntZ(Z.of_string i) }
    | f = FLOAT { Q(Q.of_string f) }
    | v = VARIABLE { Var v }
    | e1 = expr PLUS e2 = expr { Add (e1, e2) }
    | e1 = expr MINUS e2 = expr { Sub (e1, e2) }
    | e1 = expr TIMES e2 = expr { Mul (e1, e2) }
    | e1 = expr DIV e2 = expr { Div (e1, e2) }
    | e1 = expr POW e2 = expr { Pow (e1, e2) }
    | f = FUNC LPAREN e = expr RPAREN { Func (f, e) }
    | f = FUNC LPAREN e1 = expr COMMA e2 = expr RPAREN { Func2 (f, e1, e2) }
    | f = RND64 LPAREN e = expr RPAREN { Rnd64 (f, e) }
    | f = INV LPAREN e = expr RPAREN { Inv (f, e) }
    | LPAREN e = expr RPAREN { e }
    | RND LBRACK n1=INT COMMA m=mode COMMA f=FLOAT COMMA n2=INT COMMA n3=INT RBRACK LPAREN e=expr RPAREN { Rnd(int_of_string n1, m, Q.of_string f, int_of_string n2, int_of_string n3, e) }

mode:
  | NE { NE }
  | UP { UP }
