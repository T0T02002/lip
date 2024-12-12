// PARSER YACC/BISON
%{
open Ast
%}


// DICHIARAZIONI
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token EOF
%token AND
%token OR
%token NOT
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token LBRACKET
%token RBRACKET
%token INT
%token BOOL
%token <string> CONST (* stringa perché è tokenizzato come stringa *)
%token <string> VAR 


(* priorità: va dal basso all'alto *)
(* Da applicare solo agli operatori *)
%left OR
%left AND
%left NOT
%left EQ LEQ
%left ADD SUB
%left MUL
%left SEQ
%nonassoc ELSE
%nonassoc DO


// REGOLE
%start <cmd> prog  // prende tutte le regole in automatico


%%

prog:
  | c = cmd; EOF { c }  
;

expr:
  | TRUE { True }
  | FALSE { False }
  | e1 = expr; AND; e2 = expr; { And(e1,e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1,e2) }
  | LPAREN; e = expr; RPAREN {e}
  | NOT; e0 = expr; { Not(e0) }
  | e1 = expr; ADD; e2 = expr; { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr; { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr; { Mul(e1,e2) }
  | e1 = expr; EQ; e2 = expr; { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr; { Leq(e1,e2) }

  | num = CONST { Const( int_of_string num )} (* prende string e trasforma in intero *)
  | var = VAR { Var( var )} (* è già string *)
;

cmd:
  | SKIP { Skip }
  | var = VAR; ASSIGN; e = expr; { Assign(var,e) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1,c2) }
  
  | IF; e = expr; THEN c_then = cmd; ELSE; c_else = cmd; { If(e, c_then, c_else) }
  | IF; e = expr; THEN c_then = cmd; ELSE; LPAREN; c_else = cmd; RPAREN; { If(e, c_then, c_else) }
  | IF; e = expr; THEN; LPAREN; c_then = cmd; RPAREN; ELSE; c_else = cmd; { If(e, c_then, c_else) }
  
  | WHILE; e = expr; DO; c = cmd; { While(e,c) }
  | WHILE; e = expr; DO; LPAREN; c = cmd; RPAREN; { While(e,c) }

  | LBRACKET; lst=list(decl); c = cmd; RBRACKET; { Decl(lst,c) }
;

decl:
  | INT; int_var = VAR; SEQ { IntVar(int_var) }
  | BOOL; bool_var = VAR; SEQ { BoolVar(bool_var) }
;
