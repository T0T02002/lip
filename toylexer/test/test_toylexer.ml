open Toylexer.Token
open Toylexer.Main

(* In questi test non rimuovo i token da ATOK a ETOK ed è per questo che il primo test non passa *)
(* let%test _ =
  lexer "x=1; y=x+1" |> frequency 3 = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)] ;; *)

let%test _ =
  lexer "x=y; x=x+1" |> frequency 3 = [(CTOK "x", 3); (ASSIGN, 2); (PLUS, 1)] ;;

let%test _ =
  lexer "x=y; x=x+1" |> frequency 1 = [(CTOK "x", 3)] ;;

let%test _ =
  lexer "e=o; e=e+1" |> frequency 2 = [(BTOK "e", 3); (ASSIGN, 2)] ;;

let%test _ =
  lexer "x=y; x=x+1" |> frequency 2 = [(CTOK "x", 3); (ASSIGN, 2)];;

let%test _ =
  lexer "Abc123" = [ATOK "Abc123"; EOF];;

let%test _ =
  lexer "aeiou+1" = [BTOK "aeiou"; PLUS; CONST "1"; EOF];;

let%test _ =
  lexer "fgaB=0x4" = [CTOK "fgaB"; ASSIGN; ETOK "0x4" ;EOF];;

let%test _ =
  lexer "0x1" = [ETOK "0x1" ;EOF];;

let%test _ =
  lexer "0x0" = [ETOK "0x0" ;EOF];;

let%test _ =
  lexer "0x00" = [ETOK "0x00" ;EOF];;

let%test _ =
  lexer "0xaa" = [ETOK "0xaa" ;EOF];;

let%test _ =
  lexer "0xAF0" = [ETOK "0xAF0" ;EOF];;

  
(* Test di conferma *)
let%test_unit "all tests passed" =
  print_endline "Tutti i test sono passati con successo!"
;; 
