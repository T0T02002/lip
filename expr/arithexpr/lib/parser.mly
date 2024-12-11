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
%token ZERO
%token ISZERO 
%token PRED 
%token SUCC

(* left si usa per indicare la priorità; va dal basso all'alto *)
%nonassoc ELSE
%left OR        // Precedenza bassa
%left AND       // Precedenza intermedia
%right NOT      // Precedenza più alta degli operatori logici
%left ISZERO    // Ancora più alta, per i controlli numerici
%left PRED SUCC // Stessa precedenza per operazioni numeriche

(*one state has shift/reduce conflicts si ha quando non si assegnano le precedenze *)


%start <expr> prog 

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | e1 = expr; AND; e2 = expr; { And (e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or (e1, e2) }
  | NOT; e0 = expr; { Not(e0) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | SUCC; e1 = expr; { Succ(e1) }
  | PRED; e1 = expr; { Pred(e1) }
  | ISZERO; e1 = expr; { IsZero(e1) }
  | ZERO { Zero }
  | LPAREN; e=expr; RPAREN {e}
;

