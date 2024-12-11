open Ast


(* Rappresenta una locazione di memoria *)
type loc = int

(* Indica se loc è collegato a una variabile di tipo intero o booleano *)
type envval = 
 | BVar of loc 
 | IVar of loc

(* Indica nella pratica il tipo del valore nella variabile *) 
type memval = 
 | Bool of bool 
 | Int of int

(* Mappa la stringa identificativa con l'enval, il quale specifica il tipo del valore contenuto in loc  *) 
type env = ide -> envval

(* Tipo funzionale che mappa una locazione (intera) con il suo contenuto effettivo (val/id)*)
type mem = loc -> memval

exception TypeError of string
exception UnboundVar of ide
exception UnboundLoc of loc
exception NoRuleApplies

(* The third component of the state is the first free location.
   We assume that the store is unbounded *)
type state = { envstack : env list; memory : mem; firstloc : loc }

let getenv st = st.envstack
let getmem st = st.memory
let getloc st = st.firstloc

let setenv st envstack =
  { envstack; memory = st.memory; firstloc = st.firstloc }

let setmem st memory =
  { envstack = st.envstack; memory; firstloc = st.firstloc }

let setloc st firstloc =
  { envstack = st.envstack; memory = st.memory; firstloc }

(* testa dello stato *)
let topenv st =
  match st.envstack with
  | [] -> failwith "empty environment stack"
  | e :: _ -> e

let popenv st =
  match st.envstack with
  | [] -> failwith "empty environment stack"
  | _ :: el' -> el'

let make_state envstack memory firstloc = { envstack; memory; firstloc }

let bind_env f x v y = if String.equal x y then v else f y
let bind_mem f x v y = if Int.equal x y then v else f y

let bottom_env : env = fun x -> raise (UnboundVar x)
let bottom_mem : mem = fun l -> raise (UnboundLoc l)

let state0 = make_state [bottom_env] bottom_mem 0

type conf = St of state | Cmd of cmd * state
