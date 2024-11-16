(* Tipo custom ast *)
type rule =
    (* Const of int *)
    | Seq of int list              (* Seq prende una lista di interi*)
    | Rule of int list * int list  (* Rule prende una coppia di liste di interi *)
  