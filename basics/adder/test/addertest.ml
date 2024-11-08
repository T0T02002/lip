open Adder

let%test _ = addlist [] = 0
let%test _ = addlist [3] = 3
let%test _ = addlist [1;2] = 3
let%test _ = addlist [1;2;3] = 6
(*let%test _ = addlist [1;2;3] = 5*)

(* Test di conferma *)
let%test_unit "all tests passed" =
  print_endline "Tutti i test sono passati con successo!"
;;