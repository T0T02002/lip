{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex_prefix = ['0'] (['x']|['X'])
let hex_suffix = ['a'-'f' 'A'-'F' '0'-'9']+
let hex_num = hex_prefix hex_suffix

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULTIPLY }
  | "/" { DIVIDE }
  | num { CONST (Lexing.lexeme lexbuf) }
  | hex_num { CONST (Lexing.lexeme lexbuf) } (* funziona, scopri perché*)
  | eof { EOF }