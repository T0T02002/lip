open Ast

(* Tag union: tipo (Bool) valore (true) *)
type exprval = Bool of bool | Nat of int       

(* state = map from identifiers to expression values *)
type state = ide -> exprval    

(* configuration = state | (command,state) *)
type conf = 
 | St of state         (* stato corrente, sistema fermo *)
 | Cmd of cmd * state  (* coppia comando, state (modificato) *)
 
exception TypeError of string
exception UnboundVar of string
exception NoRuleApplies
