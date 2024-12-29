// Parser Yacc-Bison
// Analizza una sequenza di token forniti dal lexer e costruisce un AST (abstract-syntax-tree)

%{
  open Ast
%}

// Lista di token, rimpiazzabili con un analogo tra doppi apici.  LPAREN = "("   
%token TRUE FALSE
%token INT FUN
%token LPAREN RPAREN 
%token LBRACKET RBRACKET 
%token EQ LEQ NOT AND OR
%token IF THEN ELSE DO WHILE SKIP ASSIGN SEQ RETURN
%token ADD SUB MUL
%token <string> IDE CONST  // formattati come stringhe di caratteri alfanumerici
%token EOF

// Precedenze
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left ADD SUB
%left MUL

%left SEQ
%nonassoc ELSE DO

// Tipi
%type <decl> decl
%type <expr> expr
%type <cmd> cmd
%start <prog> prog

%%


// Punto di ingresso che descrive l'intero programma
prog:
  | dl = list(decl); c = cmd; EOF { Prog(dl,c) }


// Comandi eseguibili, ossia le istruzioni del linguaggio imp/fun
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


// Dichiarazioni: sono solo variabili o funzioni
decl:
  (* CHECK *)| INT; ivar = IDE; { IntVar(ivar) }
  | INT; ivar = IDE; SEQ { IntVar(ivar) }
  (* CHECK *)| FUN; foo = IDE; LPAREN; param = IDE; RPAREN; LBRACKET; body = cmd; SEQ; RETURN; exp = expr; RBRACKET; SEQ { Fun(foo,param,body,exp) }
  | FUN; foo = IDE; LPAREN; param = IDE; RPAREN; LBRACKET; body = cmd; SEQ; RETURN; exp = expr; RBRACKET; { Fun(foo,param,body,exp) }


// Espressioni che producono valori
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

  | var = IDE { Var(var) }
  | num = CONST { Const(int_of_string num) }
  | foo = IDE; LPAREN; e = expr; RPAREN { Call(foo,e) }

  
