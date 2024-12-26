open Ast
    
type loc = int

type envval = 
  | IVar of loc   (* location in memory *)
  | IFun of ide * cmd * expr   (* parameter name, body, return expr *)

type memval = int

type env = ide -> envval (* The {b environment} maps identifiers to memory locations or functions. *)

type mem = loc -> memval (* The {b memory} maps locations (addresses) to values. *)

(* Raised when an operation is performed on an incompatible value. *)
exception TypeError of string

(* Raised when trying to access an unknown identifier. *)
exception UnboundVar of ide

(* Raised when trying to access an unknown location. *)
exception UnboundLoc of loc

(* Raised when an expression cannot take a step. *)
exception NoRuleApplies

(* The type of states. The third component of the state is the first free location.
  We assume that the store is unbounded. *)
(* HAS TO BE type state = { envstack : env list; memory : mem; firstloc : loc } *)
type state = env list * mem * loc

(* Constructor for state values. *)
(* IMPLEMENT let make_state envstack memory firstloc = { envstack; memory; firstloc } *)




let topenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv (el,_,_) = el
let getmem (_,m,_) = m
let getloc (_,_,l) = l
  
type conf = St of state | Cmd of cmd * state



(* Recupera il valore associato a x in uno stato state *)
let apply (state : state) (x : ide) : memval = 
  match (topenv state) x with       (* controlla il tipo dello stato*)
  | IVar loc -> (getmem state) loc  (* se è Int, ritorna il valore della locazione loc nello stato state *)
  | IFun _ -> raise (TypeError "%d is a function, but I expected a value here.")

(* Recupera i componenti della funzione associata a foo nello stato state *)  
let apply_fun (state : state) (foo : ide) : ide * cmd * expr =
  match (topenv state) foo with
  | IFun (param, body, expr) -> (param, body, expr)
  | IVar _ -> raise (TypeError "%d is a value, but I expected a function here.")  
  