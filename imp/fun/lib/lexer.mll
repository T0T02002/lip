{
open Parser
}

let white = [' ' '\t' '\n']+                                  (* aggiunta l'andata a capo *)
let const = ['0'-'9']|['1'-'9']['0'-'9']*                     (* CONST, numeri, non accetta '00' *)
let ide = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9''_']*  (* ID, 'a','Aa_1' accettate *)


rule read =
  parse
  | white { read lexbuf }  

  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN } 
  | ")" { RPAREN } 
  | "{" { LBRACKET }
  | "}" { RBRACKET }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }

  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ } (* uguaglianza *)
  | "<=" { LEQ }

  | ";" { SEQ }
  | ":=" { ASSIGN }
  | "skip" { SKIP }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE } 
  | "while" { WHILE }
  | "do" { DO }

  | "int" { INT }
  | "bool" { BOOL }
  | "fun" { FUN }
  | "return" { RETURN }

  (* precedenza bassa perché altrimenti "if" viene letto come VAR e non come IF, *)
  (* se ha problemi, scambia const con var *)
  | const { CONST (Lexing.lexeme lexbuf) }
  | ide { IDE (Lexing.lexeme lexbuf) }
  
  | eof { EOF }