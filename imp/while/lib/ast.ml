(* Identificatori *)
type ide = string
  
(* Espressioni *)
type expr =
  | True
  | False
  | Var of string
  | Const of int     
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr

(* Comandi *)
type cmd =
  | Skip
  | Assign of string * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd

 
