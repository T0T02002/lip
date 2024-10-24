{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let digits = ['0'-'9']
let single_vowel = ['a']|['e']|['i']|['o']|['u']|['A']|['E']|['I']|['O']|['U']
let consonant = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z' 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']
let exad_start = (['0']['x'])|(['0']['X'])
let exad_value = ['0'-'9' 'a'-'z' 'A'-'Z']

let atok = ['A'-'Z'] chr*                              (*all strings of letters and digits starting by a capital letter*)
let btok = ['a'-'z']+                                  (*all strings of lowercase vowels*)
let ctok = consonant* single_vowel consonant*          (*all strings of letters containing at most one vowel*)
let dtok = ['-']? digits* ['.'] digits*                (*all strings of digits possibly starting with `-` and possibly containing a `.` followed by other digits (e.g., 3.14, -7., -.3)*)
let etok = exad_start exad_value+                      (*all strings representing hexadecimal numbers in C syntax  (i.e., strings starting with 0x or 0X, and containing hexadecimal value HH, where HH is 1 or more hex digits, '0'-'9','A'-'F','a'-'f')*)

let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*


rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | atok { ATOK (Lexing.lexeme lexbuf) }
  | btok { BTOK (Lexing.lexeme lexbuf) }
  | ctok { CTOK (Lexing.lexeme lexbuf) }
  | dtok { DTOK (Lexing.lexeme lexbuf) }
  | etok { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }


  (* come aggiungere test in test_toylexer.ml ?*)
