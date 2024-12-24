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
%token BOOL
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
%start <prog> prog  //prog tra parentesi angolari

%%

// prog rappresenta l'intero programma; d sono le variabili, c sono le istruzioni di comandi
prog:
  | d = decl; c = cmd; EOF { Prog(d,c) }   
;

expr:
  | TRUE { True }
  | FALSE { False }
  | LPAREN; e = expr; RPAREN {e}
  | NOT; e = expr; { Not(e) }
  | e1 = expr; AND; e2 = expr; { And(e1,e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1,e2) }
  | e1 = expr; ADD; e2 = expr; { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr; { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr; { Mul(e1,e2) }
  | e1 = expr; EQ; e2 = expr; { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr; { Leq(e1,e2) }

  | num = CONST { Const( int_of_string num ) } (* prende string e trasforma in intero *)
  | var = IDE { Var( var ) } 
  | foo = IDE; LPAREN; e = expr; RPAREN { Call(foo,e) }
;

cmd:
  | SKIP { Skip }
  
  | IF; e = expr; THEN c_then = cmd; ELSE; c_else = cmd; { If(e, c_then, c_else) }
  | IF; e = expr; THEN c_then = cmd; ELSE; LPAREN; c_else = cmd; RPAREN; { If(e, c_then, c_else) }
  | IF; e = expr; THEN; LPAREN; c_then = cmd; RPAREN; ELSE; c_else = cmd; { If(e, c_then, c_else) }
  
  | WHILE; e = expr; DO; c = cmd; { While(e,c) }
  | WHILE; e = expr; DO; LPAREN; c = cmd; RPAREN; { While(e,c) }

  | var = IDE; ASSIGN; e = expr; { Assign(var,e) }
  | command1 = cmd; SEQ; command2 = cmd; { Seq(command1,command2) }
  | LPAREN; c = cmd; RPAREN { c }
  (* | LBRACKET; lst=list(decl); c = cmd; RBRACKET; { Decl(lst,c) }  POSSIBILE IMPLEMENTAZIONE SENZA I TIPI NUOVI DI AST *)
;

decl:
  | INT; int_var = IDE { IntVar(int_var) }
  (* | BOOL; bool_var = IDE; SEQ { BoolVar(bool_var) } (* può dare problemi *) *)
  | FUN; f = IDE; LPAREN; x = IDE; RPAREN; LBRACKET; c = cmd; SEQ; RETURN; e = expr; RBRACKET { Fun(f,x,c,e) }
  | d1 = decl; SEQ; d2 = decl { DSeq(d1,d2) }
  (* | { EmptyDecl } *)
;
