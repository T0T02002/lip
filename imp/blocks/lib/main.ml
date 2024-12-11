open Ast
open Types


(* Prende una stringa in input rappresentante il comando da analizzare "x:=0" in una 
 * rappresentazione strutturata di tipo cmd ( ad esempio (Assign of string * expr)) *)
let parse (s : string) : cmd =  
  (* trasforma s in un buffer lessicale utilizzato dal lexer per leggere incrementalmente *) 
  let lexbuf = Lexing.from_string s in  
  (* Lexer.read è il lexer che scansiona il buffer lexbuf e lo dà in pasto al parser Parser.prog *)
  (* Parser.prog riceve i token (es. TRUE) dal lexer, li analizza e li trasforma in AST (NOT;e0=expr;{Not(e0)}) *)
  let ast = Parser.prog Lexer.read lexbuf in  
  ast (* Il risultato è in ast *)


  
(* BIG STEP SEMANTIC, restituisce senza passaggi intermedi un risultato *)
(* Prende in input state (funzione che mappa una variabile un valore in un certo stato)
 * e expr (espressione da valutare) *)
let rec eval_expr : (state -> expr -> memval) = fun 
  state expr -> 
  (* esamina il tipo di expr e in base a esso codifica un exprval (Bool true) *)
  match expr with
  | True -> Bool true
  | False -> Bool false 

  | And (e0,e1) -> (
    match (eval_expr state e0, eval_expr state e1) with 
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | _ -> failwith "I parametri di And devono essere booleani")

  | Or (e0,e1) -> (
    match (eval_expr state e0, eval_expr state e1) with 
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | _ -> failwith "I parametri di Or devono essere booleani")

  | Not (e0) -> (
    match (eval_expr state e0) with 
    | Bool b -> Bool (not b)
    | _ -> failwith "Il parametro di Not deve essere booleano")

  | Add (e0,e1) -> (
    match (eval_expr state e0, eval_expr state e1) with 
    | Nat n1, Nat n2 -> Nat (n1+n2)
    | _ -> failwith "I parametri di Add devono essere numeri")

  | Sub (e0,e1) -> (
    match (eval_expr state e0, eval_expr state e1) with 
    | Nat n1, Nat n2 -> Nat (n1-n2)
    | _ -> failwith "I parametri di Sub devono essere numeri")

  | Mul (e0,e1) -> (
    match (eval_expr state e0, eval_expr state e1) with 
    | Nat n1, Nat n2 -> Nat (n1*n2)
    | _ -> failwith "I parametri di Mul devono essere numeri")  
    
  | Eq (a,b) -> (
    match (eval_expr state a, eval_expr state b) with 
    | Bool a, Bool b -> Bool (a=b)
    | Nat a, Nat b -> Bool (a=b)
    | _ -> failwith "I valori di Eq devono essere dello stesso tipo" )

  | Leq (a,b) -> (
    match (eval_expr state a, eval_expr state b) with 
    | Nat a, Nat b -> Bool (a<=b)
    | _ -> failwith "I valori di Leq devono essere entrambi numerici" )

  (* Const è sempre un numero, quindi ne viene restituito il valore *)
  | Const num -> Nat num 

  (* Var cambia, quindi viene restituito il valore dentro state (valore corrente) *)
  | Var nomevar_ide -> (
    let env = topenv state in
    match env nomevar_ide with 
    | BVar l 
    | IVar l -> (getmem state) l
    
  )
  


(* bind restituisce una funzione aggiornata dello stato per una variabile *)
(* lega un valore a una variabile in uno stato e restituisce un nuovo stato modificato *)
(* state: funzione che rappresenta uno stato. mappa un identificativo a un'exprval *)
(* x: la variabile che vogliamo aggiornare *)
(* value: il nuovo valore che vogliamo dare a x *)
(* y: variabile che vogliamo cercare nel vecchio stato *)
let bind state x value y = 
  (* se stiamo cercando y, che è uguale a x, allora restituisce il nuovo valore associato a x (value) *)
  if x = y then value 
  (* il valore viene restituito invariato altrimenti *)  
  else state y
(* ESEMPIO *)
(* let state = fun x -> if x = "a" then 1 else 2 *)
(* state "a" = 1    mappo 1 ad 'a' *)
(* state "b" = 2    mappo 2 a  'b' *)
(* let new_state = bind state "a" 10 *)
(* new_state "a" = 10    'a' corrisponde a new_state mappato ad 'a', quindi modifica 'a' in 10 *)
(* new_state "b" = 2     'b' non corrisponde a new_state mappato ad 'a', quindi ritorna il valore corrente di b *)



(* SMALL STEP SEMANTIC: l'espressione viene valutata passo dopo passo usando una regola alla volta, visualizza gli stati intermedi

-------------------------- [Skip]
  Cmd (skip, st) --> St st

            st |- e ==> v
--------------------------------------- [Assign]
  Cmd (x := e, st) --> St st[x |-> v]

      Cmd (c1, st) --> St st'
------------------------------------- [Seq_St]
  Cmd (c1;c2, st) --> Cmd (c2, st')

    Cmd (c1, st) --> Cmd (c1', st')
----------------------------------------- [Seq_Cmd]
  Cmd (c1;c2, st) --> Cmd (c1';c2, st')

          st |- e ==> false
--------------------------------------------------- [If_False]
  Cmd (if e then c1 else c2, st) --> Cmd (c2, st)

          st |- e ==> true
--------------------------------------------------- [If_True]
  Cmd (if e then c1 else c2, st) --> Cmd (c1, st)

          st |- e ==> false
------------------------------------ [While_False]
  Cmd (while e do c, st) --> St st

          st |- e ==> true
-------------------------------------------------------- [While_True]
  Cmd (while e do c, st) --> Cmd (c; while e do c, st)
*)


let rec trace1 = function
  (* Non fa nulla, restituisce lo stato corrente state *)
  | Cmd (Skip, state) -> St state 

  (* Se è un'assegnamento, calcola il valore di expr e lo associa a x nel nuvo stato *)
  | Cmd (Assign (x,expr), state) -> 
    let new_state = bind state x (eval_expr state expr) in St new_state
  
  (* Se è una sequenza di comandi, esegui in modo ricorsivo entrambi finché non si ha St state' *)  
  | Cmd (Seq (comand1,comand2), state) -> (
    match trace1 (Cmd (comand1,state)) with 
    | Cmd (comand1',state') -> Cmd (Seq (comand1',comand2), state')
    | St state' -> Cmd (comand2, state'))

  (* In If valuta expr, confronta il tipo ed esegui il tipo di comando in base al valore della condizione *)  
  | Cmd (If (expr,c_then,c_else), state) -> (
    match eval_expr state expr with 
    | Bool b -> if b then Cmd (c_then, state) else Cmd (c_else, state)
    | _ -> failwith "Errore, expr di If vuole un valore booleano" )

  (* In While valuta la condizione di while, se essa è vera continua con comand altrimenti termina con St state *)  
  | Cmd (While (expr,comand), state) -> (
    match eval_expr state expr with 
    | Bool true -> Cmd (Seq (comand, While (expr, comand)), state)
    | Bool false -> St state
    | _ -> failwith "Errore, expr di While non è un valore booleano")
  
  | _ -> raise NoRuleApplies


(* Fallisce sempre, serve per la funzione dello stato iniziale *)
let bottom _ = failwith "fail"


(* crea un interprete passo passo per il linguaggio, seguendo la semantica dei comandi *)
(* esegue un numero specifico di passi, limitando la valutazione a un numero n_steps di passi *)
let trace (n_steps : int) (comand : cmd) : conf list =
  (* crea una configurazione iniziale conf0, ossia un comando in uno stato iniziale (tipo conf) *)
  let conf0 = Cmd (comand, bottom) in
  (* esegue il passo ricorsivo con trace *)
  let rec helper n_steps conf =
    if n_steps > 0 then
      try
        (* configurazione attuale rinominata e aggiunta all'inizio della lista, steps decrementati *)
        let conf1 = trace1 conf in
        conf :: helper (n_steps - 1) conf1
      with NoRuleApplies -> [ conf ]
    (* altrimenti termina la ricosione *)  
    else [ conf ]
  in helper n_steps conf0 (* TODO CAPIRE *)

 
