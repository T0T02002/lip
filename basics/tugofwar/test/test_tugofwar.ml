open Tugofwar (* componente *)

let%test "" = win [A] = A
let%test "" = toklist_of_string "AAAAA===BB" = [A; A; A; A; A; X; X; X; B; B]
let%test "" = valid [A; A; A; A; A; X; X; X; B; B] = true

(* Test di conferma *)
let%test_unit "all tests passed" =
  print_endline "Tutti i test sono passati con successo!"
;; 