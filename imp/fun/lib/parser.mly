// PARSER YACC/BISON
%{
open Ast
%}


// DICHIARAZIONI
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token AND
%token OR
%token NOT

%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ

%token SEQ
%token ASSIGN
%token SKIP
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO

%token INT
%token FUN 
%token RETURN 

%token <string> CONST (* stringa perché è tokenizzato come stringa *)
%token <string> IDE  (* prima era VAR, ma può essere sia il nome di una variabile che di una funzione *)

%token EOF


(* priorità: va dal basso all'alto *)
(* Da applicare solo agli operatori *)
%left SEQ
%nonassoc ELSE DO
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left ADD SUB
%left MUL


// REGOLE
%start <prog> prog  

%%

(* HAS TO BE type prog = Prog of (decl list * cmd)*)
prog:
  | d = decl; c = cmd; EOF { Prog(d,c) }
;

decl:
  | INT; ivar = IDE { IntVar(ivar) }
  | FUN; foo = IDE; LPAREN; param = IDE; RPAREN; LBRACKET; command = cmd; SEQ; RETURN; exp = expr; RBRACKET { Fun(foo,param,command,exp) }
  | d1 = decl; SEQ; d2 = decl { DSeq(d1,d2) }   (* REMOVE *)
  | { EmptyDecl }  (* REMOVE *)
;

expr:
  | TRUE { True }
  | FALSE { False }
  | LPAREN; e = expr; RPAREN { e }
  | NOT; e = expr { Not e }
  | e1 = expr; AND; e2 = expr { And(e1,e2) }
  | e1 = expr; OR; e2 = expr { Or(e1,e2) }
  | e1 = expr; ADD; e2 = expr { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1,e2) }

  | foo = IDE; LPAREN; e = expr; RPAREN { Call(foo,e) }
  | x = IDE { Var(x) }
  | num = CONST { Const(int_of_string num) }
;

cmd:
  | SKIP { Skip }
  
  | IF; e = expr; THEN c_then = cmd; ELSE; c_else = cmd; { If(e, c_then, c_else) }
  | IF; e = expr; THEN c_then = cmd; ELSE; LPAREN; c_else = cmd; RPAREN; { If(e, c_then, c_else) }
  | IF; e = expr; THEN; LPAREN; c_then = cmd; RPAREN; ELSE; c_else = cmd; { If(e, c_then, c_else) }
  
  | WHILE; e = expr; DO; c = cmd; { While(e,c) }
  | WHILE; e = expr; DO; LPAREN; c = cmd; RPAREN; { While(e,c) }

  | var = IDE; ASSIGN; e = expr; { Assign(var,e) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1,c2) }
  | LPAREN; c = cmd; RPAREN { c }

