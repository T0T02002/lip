open Ast

(* Tag union: (Tipo,Valore), ad es.(Nat 0) *)
type exprval = 
| Bool of bool 
| Nat of int       


(* state = map from identifiers to expression values *)
type state = ide -> exprval    
(* let state x = match x with 
   | "x" -> Nat 10 
   | "y" -> Bool true 
   | _ -> failwith "unbound" *)
(* state "x" restituisce Nat 10 *)
(* state "y" restituisce Bool true *)
(* state "z" restituisce errore, perché z non esiste *)


(* configuration = state | (command,state) *)
(* Rappresenta lo stato d'esecuzione del programma *)
type conf = 
 | St of state         (* esecuzione conclusa*)
 | Cmd of cmd * state  (* configurazione intermedia, contiene un comando ancora da eseguire e lo stato corrente *)

(* St è un costruttore di tipo che indica lo stato finale *)
(* state è una funzione che associa variabili a valori, ossia il tipo definito sopra *)
 

exception TypeError of string
exception UnboundVar of string (* usata in eval_expr per variabili non definite  *)
exception NoRuleApplies
