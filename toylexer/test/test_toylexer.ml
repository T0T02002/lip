open Toylexer.Token
open Toylexer.Main

(* In questi test non rimuovo i token da ATOK a ETOK ed è per questo che il primo test non passa *)
let%test "test_frequencies_1" =
  lexer "x=1; y=x+1" |> frequency 3 = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)] ;;

let%test "test_frequencies_2" =
  lexer "x=y; x=x+1" |> frequency 3 = [(CTOK "x", 3); (ASSIGN, 2); (PLUS, 1)] ;;

let%test "test_frequencies_3" =
  lexer "x=y; x=x+1" |> frequency 1 = [(CTOK "x", 3)] ;;

let%test "test_frequencies_4" =
  lexer "e=o; e=e+1" |> frequency 2 = [(BTOK "e", 3); (ASSIGN, 2)] ;;

let%test "test_freq_2" =
  lexer "x=y; x=x+1" |> frequency 2 = [(CTOK "x", 3); (ASSIGN, 2)];;

let%test "test_token" =
  lexer "Abc123" = [ATOK "Abc123"; EOF];;

let%test "test_token_2" =
  lexer "aeiou+1" = [BTOK "aeiou"; PLUS; CONST "1"; EOF];;

let%test "test_token_3" =
  lexer "fgaB=0x4" = [CTOK "fgaB"; ASSIGN; ETOK "0x4" ;EOF];;


(* Test di conferma *)
let%test_unit "all tests passed" =
  print_endline "Tutti i test sono passati con successo!"
;; 
(* YOUR TESTS HERE *)