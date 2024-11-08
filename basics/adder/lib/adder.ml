(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

(* prende una lista, la scorre e ne restituisce la somma degli elementi*)
let rec addlist l = match l with
  [] -> 0
  | a :: tail -> a + addlist tail;;(* replace 0 with actual code *)
;;
