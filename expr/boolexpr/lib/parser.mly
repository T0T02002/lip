%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token OR   (* DIVERSO DA OR  CLASSICO*)
%token AND  (* DIVERSO DA AND CLASSICO*)
%token EOF


%start <boolExpr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | LPAREN; e = expr; RPAREN {e}
  | e1 = expr; AND; e2 = expr; { If(e1, e2, False) } (* if a then b else false *)
  | e1 = expr; OR; e2 = expr; { If(e1, True, e2) }   (* if a then true else b  *)
;

