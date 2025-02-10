(* Ocamllex lexer *)
(* Trasforma dei caratteri e li trasforma in token seguendo delle regole specifiche *)

{
  open Parser
}

let white = [' ' '\t' '\n']+                                  
let const = ['0'-'9']|['1'-'9']['0'-'9']*                  (* numeri: non accetta '00' *)
let ide = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*  (* identificativi: 'a','Aa_1' accettate *)


rule next_token = parse

  | white { next_token lexbuf }   (* ignora e va avanti *)

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
  | "=" { EQ }   (* uguaglianza *)
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
  | "fun" { FUN }
  | "return" { RETURN }

  (* precedenza bassa perché altrimenti i costrutti (if,while) vengono letti come nomi di variabile *)
  | const { CONST (Lexing.lexeme lexbuf) }  (* restituisce il valore testuale convertito come CONST string *)
  | ide { IDE (Lexing.lexeme lexbuf) }      (* restituisce il valore testuale convertito come IDE string *)
  | eof { EOF }