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
%token EOF
%token AND
%token OR
%token NOT

(* left si usa per indicare la priorità; va dal basso all'alto *)
%left OR 
%left AND
%right NOT
(*) capire perché:
    Warning: one state has shift/reduce conflicts.
    Warning: 2 shift/reduce conflicts were arbitrarily resolved. *)


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
  | e1 = expr; AND; e2 = expr; { And(e1, e2) } 
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) } 
  | NOT; e1 = expr; { Not(e1) }
  
  (* AND e OR senza modificare le semantiche (TASK 6 boolexp): )
  | e1 = expr; AND; e2 = expr; { If(e1, e2, False) } (* if a then b else false *)
  | e1 = expr; OR; e2 = expr; { If(e1, True, e2) }   (* if a then true else b  *) *)
;

