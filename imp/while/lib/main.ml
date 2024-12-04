open Ast
open Types

(* Esegue le regole di parsing su una stringa *)
let parse (s : string) : cmd =   (* cambia in cmd *)
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast 

  
(* BIG STEP SEMANTIC: l'espressione viene valutata senza passi intermedi, ricorsione che riduce l'espressione in un singolo passo*)
(* Valuta una boolExpr per ottenere true o false, contempla l'if nativo di ocaml*)
let rec eval_expr = fun state expr -> 
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
    | Nat a, Nat b -> Bool (a=b)
    | Bool a, Bool b -> Bool (a=b)
    | _ -> failwith "I valori di Eq devono essere dello stesso tipo" )

  | Leq (a,b) -> (
    match (eval_expr state a, eval_expr state b) with 
    | Nat a, Nat b -> Bool (a=b)
    | Bool a, Bool b -> Bool (a<=b)
    | _ -> failwith "I valori di Leq devono essere entrambi numerici" )

  | Const num -> Nat num 
  | Var var -> state var     
  

(* bind restituisce una funzione ch rappresenta lo stato aggiornato *)
(* se applicata a y, e y è uguale x, restituisce value *)
(* altrimenti restituisce il valore originale di y nello stato state *)
let bind state x value y = 
  if x = y then value else state y

(* SMALL STEP SEMANTIC: l'espressione viene valutata passo dopo passo usando una regola alla volta, visualizza gli stati intermedi*)
(* Applica una regola di valutazione all'espressione booleana, con contempla l'if nativo di ocaml *)
let rec trace1 = function
  | Cmd (Skip, state) -> St state (* non fa nulla, termina subito *)

  | Cmd (Assign (lvalue,rvalue), state) -> 
    let new_state = bind state lvalue (eval_expr state rvalue) in St new_state
  
  | Cmd (Seq (comand1,comand2), state) -> (
    match trace1 (Cmd (comand1,state)) with 
    | Cmd (comand1',state') -> Cmd (Seq (comand1',comand2), state')
    | St state' -> Cmd (comand2, state'))

  | Cmd (If (expr,comand1,comand2), state) -> (
    match eval_expr state expr with 
    | Bool e -> if e then Cmd (comand1, state) else Cmd (comand2, state)
    | _ -> failwith "Errore, expr di If vuole un valore booleano" )

  | Cmd (While (expr,comand), state) -> (
    match eval_expr state expr with 
    | Bool true -> Cmd (Seq (comand, While (expr, comand)), state)
    | Bool false -> St state
    | _ -> failwith "Errore, expr di While non è un valore booleano")
  
  | _ -> raise NoRuleApplies

let bottom _ = failwith "fail"

(* crea un interprete passo passo per il linguaggio, seguendo la semantica dei comandi *)
let trace (n_steps : int) (c : cmd) : conf list =
  let conf0 = Cmd (c, bottom) in
  let rec helper n conf =
    if n > 0 then
      try
        let conf' = trace1 conf in
        conf :: helper (n - 1) conf'
      with NoRuleApplies -> [ conf ]
    else [ conf ]
  in
  helper n_steps conf0
