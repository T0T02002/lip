open Ast

(* Rappresenta una locazione di memoria (indirizzo int) *)
type loc = int

(* Rappresenta valori d'ambiente*)
type envval =
  | IVar of loc  (* variabile intera, un indirizzo di memoria loc *)
  | IFun of ide * cmd * expr  (* chiamata a funzione, con nome (ide), blocco di comandi (cmd) e espressione di return (expr) *)

(* Valori memorizzati nella memoria *)  
type memval = int

(* Associa gli identificatori (i nomi) ai loro valori d'ambiente (IVar o IFun) *)
type env = ide -> envval

(* Associa locazioni di memoria a valori da inserire in memoria  *)
type mem = loc -> memval


(* Raised when an operation is performed on an incompatible value. *)
exception TypeError of string
(* Raised when trying to access an unknown identifier. *)
exception UnboundVar of ide
(* Raised when trying to access an unknown location. *)
exception UnboundLoc of loc
(* Raised when an expression cannot take a step. *)
exception NoRuleApplies


(* Rappresenta lo stato del programma:
** - lista di ambienti env: (ide -> envval)
** - locazioni di memoria mem: (loc -> memval=int)
** - prima locazione di memoria libera (loc=int).    We assume that the store is unbounded.*)
type state = { envstack : env list; memory : mem; firstloc : loc }


(* Costruttore di stati: crea un nuovo stack di ambienti, un nuovo memory e un nuovo firstloc *)
let make_state envstack memory firstloc = 
             { envstack; memory; firstloc }

(* Restituisce l'envstack selezionandolo dallo state *)
let getenv st = st.envstack

(* Restituisce memory selezionandola dallo state *)
let getmem st = st.memory

(* Restituisce firstloc selezionandola dallo state *)
let getloc st = st.firstloc

(* Sostituisce l'envstack dello stato con uno nuovo fornito *)
let setenv st envstack = { st with envstack }

(* Sostituisce memory dallo stato con una nuova fornita *)
let setmem st memory = { st with memory }

(* Sostituisce firstloc dallo stato con una nuova fornita *)
let setloc st firstloc = { st with firstloc }

(* Restituisce l'env più in alto nello stack envstack. Se è vuoto restituisce errore *)
let topenv st =
  match st.envstack with
  | [] -> failwith "empty environment stack"
  | e :: _ -> e

(* Restituisce lo stack di env senza l'env più in alto. Rimuove il primo e restituisce la pila  *)
let popenv st =
  match st.envstack with
  | [] -> failwith "empty environment stack"
  | _ :: el' -> el'

(* Aggiunge un env in cima all'envstack *)
let pushenv st env = env :: st.envstack

(* Crea un nuovo ambiente che associa un ide a un valore envval, lasciando invariato il resto *)
let bind_env (old_env : env) (x : ide) (v : envval) : env =
 fun y -> if String.equal x y then v else old_env y

(* Crea un mem associando una locazione loc a un valore *)
let bind_mem (old_mem : mem) (x : loc) (v : memval) : mem =
 fun y -> if Int.equal x y then v else old_mem y

(* Solleva l'eccezione per ambienti non dichiarati o vuoti *)
let bottom_env : env = fun x -> raise (UnboundVar x)

(* Solleva l'eccezione per mem vuota *)
let bottom_mem : mem = fun l -> raise (UnboundLoc l)

(* Stato iniziale con envstack vuota, memoria vuota e locazione libera 0 *)
let state0 = make_state [ bottom_env ] bottom_mem 0

(* Restituisce il valore associato a un identificatore x nello stato *)
let apply (st : state) (x : ide) : memval =
  match (topenv st) x with
  | IVar l -> (getmem st) l
  | IFun _ -> raise (TypeError "%d is a function, but I expected a value here.")

(* Restituisce i dettagli (parametro,body,return) di una funzione associata a un ide *)
let apply_fun (st : state) (x : ide) : ide * cmd * expr =
  match (topenv st) x with
  | IFun (par, body, e) -> (par, body, e)
  | IVar _ -> raise (TypeError "%d is a value, but I expected a function here.")

(* Associa un valore v a una variabile x esistente nello stato *)
let bind_ivar (st : state) x v =
  let env = topenv st in
  match env x with
  | IVar l ->
      let mem' = bind_mem st.memory l v in
      setmem st mem'
  | IFun (f, _, _) ->
      raise
        (TypeError
           (Printf.sprintf
              "Can't assign %d to %s because %s was declared a function." v f f))

(* Rappresenta una configurazione per la small-step*)
type conf = 
  | St of state  (* stato semplice *)
  | Cmd of cmd * state  (* comando insieme a uno stato *)


  