(* Espressioni *)
type expr =
    True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr

  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr

(* Tag union: tipo (Bool) valore (true) *)
type exprval = Bool of bool | Nat of int

(* Tag union *)
type exprtype = BoolT | NatT

