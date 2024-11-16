
{
  open Parser
}

let white = [' ' '\t']+             (* spazi bianchi *)
let seq = ['0'-'9']+                (* 1 o più cifre "123" *)
let ext_seq = seq (','seq)*         (* cifre intervallate da virgole "1,2,3" *)
let range = seq".."seq              (* cifre intervallate da due punti *)
let range_seq = range (','range)*   (* intervalli *)

rule read_token =
  parse
  | white { read_token lexbuf } 
  | "/" { SLASH } 
  | "E" { E }
  | "S" { S }
  | "B" { B }
  | seq { SEQ (Lexing.lexeme lexbuf) }
  | ext_seq { SEQ (Lexing.lexeme lexbuf) }
  | range_seq { RANGE_SEQ (Lexing.lexeme lexbuf) }
  | eof { EOF }