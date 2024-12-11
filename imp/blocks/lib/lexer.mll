{
open Parser
}

let white = [' ' '\t']+
let const = ['0'-'9']['0'-'9']*                           (* '0','01' accettate *)
let var = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*  (* 'a','Aa_1' accettate *)


rule read =
  parse
  | white { read lexbuf }  

  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE } 
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }

  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ } (* singolo uguale *)
  | "<=" { LEQ }

  | ";" { SEQ }
  | ":=" { ASSIGN }
  | "skip" { SKIP }
  | "while" { WHILE }
  | "do" { DO }

  | "{" { LBRACKET }
  | "}" { RBRACKET }
  | "int" { INT }
  | "bool" { BOOL }

  (* precedenza bassa perché altrimenti "if" viene letto come VAR e non come IF *)
  | const { CONST (Lexing.lexeme lexbuf) }
  | var { VAR (Lexing.lexeme lexbuf) }
  
  | eof { EOF }