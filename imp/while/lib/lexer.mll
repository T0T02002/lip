{
open Parser
}

let white = [' ' '\t']+
let var = [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]*
let const = ['0'-'9']|['1'-'9']['0'-'9']*


rule read =
  parse
  | white { read lexbuf }  
  | var { VAR (Lexing.lexeme lexbuf) }
  | const { CONST (Lexing.lexeme lexbuf) }
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
  | "succ" { SUCC }
  | "0" { ZERO }
  | "pred" { PRED }
  | "iszero" { ISZERO }

  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "==" { EQ }
  | "<=" { LEQ }

  | ";" { SEQ }
  | ":=" { ASSIGN }
  | "skip" { SKIP }
  | "while" { WHILE }
  | "do" { DO }
  | eof { EOF }