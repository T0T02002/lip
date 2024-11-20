{
open Parser
}

let white = [' ' '\t']+

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
  | "succ" { SUCC }
  | "zero" { ZERO }
  | "pred" { PRED }
  | "iszero" { ISZERO }
  | eof { EOF }