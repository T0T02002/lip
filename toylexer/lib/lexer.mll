{
  open Token
}

(* espressioni regolari - MIGLIORABILI *)
(* TODO capire esattamente come funziona la priorità *)
let white = [' ' '\t']+
let capital = ['A'-'Z']
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let low_vowels = ['a']|['e']|['i']|['o']|['u']
let vowels = ['a']|['e']|['i']|['o']|['u']|['A']|['E']|['I']|['O']|['U']
let consonant = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z' 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex_prefix = ['0'] (['x']|['X'])
let hex_suffix = ['a'-'f' 'A'-'F' '0'-'9']+

let atok = capital chr*
let btok = low_vowels+ 
let ctok = consonant* vowels? consonant*
let dtok = ['-']? num ['.']? num
let etok = hex_prefix hex_suffix

let id = letter chr* 

(* associazioni simboli con token *)
rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | atok { ATOK (Lexing.lexeme lexbuf) } (* all strings of letters and digits starting by a capital letter *)
  | btok { BTOK (Lexing.lexeme lexbuf) } (* all strings of lowercase vowels *)
  | ctok { CTOK (Lexing.lexeme lexbuf) } (* all strings of letters containing at most one vowel *)
  | dtok { DTOK (Lexing.lexeme lexbuf) } (* all strings of digits possibly starting with - and possibly containing a . followed by other digits (e.g., 3.14, -7., -.3) *)
  | etok { ETOK (Lexing.lexeme lexbuf) } (* all strings representing hexadecimal numbers in C syntax *)
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }  
  | eof { EOF }