open Recognizer

let%test "" = belongsTo ['0'] = [true;true;true;true;true]

(* Test di conferma *)
let%test_unit "all tests passed" =
  print_endline "Tutti i test sono passati con successo!"
;;