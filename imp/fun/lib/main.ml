open Ast
open Types

(* Esegue il parsing dei token *)
let parse (s : string) : prog =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.next_token lexbuf in
    ast

(* Scorre una lista di dichiarazioni valutandole *)    
let eval_decl : state -> decl list -> state =
 fun state ds ->
  let env, loc =
    List.fold_left
      (fun (env, loc) d ->
        match d with
        | IntVar x -> (bind_env env x (IVar loc), loc + 1)
        | Fun (f, x, c, e) -> (bind_env env f (IFun (x, c, e)), loc + 1))
      (topenv state, getloc state)
      ds
  in
  let envstack = getenv state in
  make_state (env :: envstack) (getmem state) loc

    
(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

(* Prende un'expr e restituisce una versione ridotta con la small step semantic *)
let rec trace_expr (st : state) (e : expr) : state * expr =
    match e with

    (* Espressioni restituite così come sono senza cambiare stato *)
    | True -> (st, True)
    | False -> (st, False)
    | Const num -> (st, Const num)

    | Var(var) -> (
        match (topenv st) var with
        | IVar l -> (st, Const ((getmem st) l))
        | _ -> failwith "apply error") (* Ottiene il valore di var attraverso apply nello stato attuale *)

    | Not True -> (st, False)
    | Not False -> (st, True)
    | Not e -> ( match trace_expr st e with 
        st', e' -> (st', Not e')) (* Riduzione ricorsiva creando un'espressione diversa e' *)

    | And (True, e) -> ( (* riduzione secondo termine*)
        match trace_expr st e with
        | st', True -> (st', True)
        | st', False -> (st', False)
        | st', e' -> (st', And (True, e')))
    | And (False, _) -> (st, False) (* caso base *)
    | And (e1, e2) -> ( (* riduzione primo termine *)
        match trace_expr st e1 with
        | st', True -> (st', And (True, e2))
        | st', False -> (st', And (False, e2))
        | st', e' -> (st', And (e', e2)))

    | Or (True, _) -> (st, True)
    | Or (False, e) -> (
        match trace_expr st e with
        | st', True -> (st', True)
        | st', False -> (st', False)
        | st', e' -> (st', Or (False, e')))
    | Or (e1, e2) -> (
        match trace_expr st e1 with
        | st', True -> (st', Or (True, e2))
        | st', False -> (st', Or (False, e2))
        | st', e1' -> (st', Or (e1', e2)))

    | Add (Const n1, Const n2) -> (st, Const (n1 + n2))
    | Add (Const n, e) -> (
        match trace_expr st e with
        | st', Const n' -> (st', Add (Const n, Const n'))
        | st', e' -> (st', Add (Const n, e')))
    | Add (e1, e2) -> (
        match trace_expr st e1 with st', e' -> (st', Add (e', e2)))

    | Sub (Const n1, Const n2) -> (st, Const (n1 - n2))
    | Sub (Const n, e) -> (
        match trace_expr st e with
        | st', Const n' -> (st', Sub (Const n, Const n'))
        | st', e' -> (st', Sub (Const n, e')))
    | Sub (e1, e2) -> (
        match trace_expr st e1 with
        | st', Const n -> (st', Sub (Const n, e2))
        | st', e' -> (st', Sub (e', e2)))

    | Mul (Const n1, Const n2) -> (st, Const (n1 * n2))
    | Mul (Const n, e) -> (
        match trace_expr st e with
        | st', Const n' -> (st', Mul (Const n, Const n'))
        | st', e' -> (st', Mul (Const n, e')))
    | Mul (e1, e2) -> (
        match trace_expr st e1 with
        | st', Const n -> (st', Mul (Const n, e2))
        | st', e' -> (st', Mul (e', e2)))

    | Eq (Const n, Const n1) -> if n = n1 then (st, True) else (st, False)
    | Eq (Const n, e) -> (
        match trace_expr st e with
        | st', Const n' -> (st', Eq (Const n, Const n'))
        | st', e' -> (st', Eq (Const n, e')))
    | Eq (e1, e2) -> (
        match trace_expr st e1 with
        | st', Const n -> (st', Eq (Const n, e2))
        | st', e1' -> (st', Eq (e1', e2)))

    | Leq (Const n, Const n1) -> if n <= n1 then (st, True) else (st, False)
    | Leq (Const n, e) -> (
        match trace_expr st e with
        | st', Const n' -> (st', Leq (Const n, Const n'))
        | st', e' -> (st', Eq (Const n, e')))
    | Leq (e1, e2) -> (
        match trace_expr st e1 with
        | st', Const n -> (st', Leq (Const n, e2))
        | st', e1' -> (st', Eq (e1', e2)))

    (* Chiamata diretta di funzione dal programma con argomento costante, foo(n) *)       
    | Call (foo, Const num) -> (
        match (topenv st) foo with
        | IFun (param, body, retexpr') ->
            let env = bind_env (topenv st) param (IVar (getloc st)) in (* associa param alla nuova locazione di memoria nell'ambiente top-level corrente *)
            let mem = bind_mem (getmem st) (getloc st) num in (* associa loc al valore num nella memoria dello stato *)
            let loc = getloc st + 1 in (* Calcola una nuova locazione recuperando il primo indirizzo libero *)
            (make_state (pushenv st env) mem loc, CallExec (body, retexpr'))
        | IVar _ -> raise (TypeError "Must take a number or is not a function"))

    | Call (foo, e) -> ( (* Riduzione dell'espressione *)
        match trace_expr st e with
        | _, True | _, False -> failwith "unbound value"
        | st', Const n -> (st', Call (foo, Const n))
        | st', e' -> (st', Call (foo, e')))

    (* Valutazione dei comandi della funzione chiamata *)  
    | CallExec (body, retexpr) -> (
        match trace1 (Cmd (body, st)) with (* si valuta passo-passo il comando da eseguire e si confronta con un pattern matching *)
        | St st' -> (st', CallRet retexpr)  (* la riduzione del comando è uno stato: computazione finita e se ne occupa CallRet *)
        | Cmd (body', st') -> (st', CallExec (body', retexpr))) (* la riduzione dà vita a un nuovo comando: ripeti *)

    (* Otteniamo un valore di ritorno numerico terminale: restituiamo *)  
    | CallRet (Const num) ->
        (make_state (popenv st) (getmem st) (getloc st), Const num)

    (* Otteniamo un valore di ritorno da ridurre *)      
    | CallRet e -> ( match trace_expr st e with st', e' -> 
        (st', CallRet e'))
  

    (* Implementa la small-step per i comandi *)
    (* I comandi sono nella forma conf: stato finale (St) o da ridurre (Cmd) *)
  and trace1 : conf -> conf = function
    (* Stato finale*)   
    | St _ -> raise NoRuleApplies

    (* Stato da ridurre*)
    | Cmd (Skip, st) -> St st (* non fa niente e restituisce lo stato corrente *)

    | Cmd (Assign (x, Const num), st) -> ( (* matcha l'assegnazione di una variabile *)
        match (topenv st) x with
        | IVar loc ->
            St (make_state (getenv st) (bind_mem (getmem st) loc num) (getloc st))
        | _ -> raise (TypeError "symbol not valid"))

    | Cmd (Assign (ide, e), st) ->
        let st', e' = trace_expr st e in
        Cmd (Assign (ide, e'), st')

    | Cmd (Seq (c1, c2), st) -> (
        match trace1 (Cmd (c1, st)) with
        | Cmd (c1', st') -> Cmd (Seq (c1', c2), st')
        | St st' -> Cmd (c2, st'))

    | Cmd (If (cond, c_then, c_else), st) -> (
        match trace_expr st cond with
        | st', True -> Cmd (c_then, st')
        | st', False -> Cmd (c_else, st')
        | _, Const _ -> raise (TypeError "unbound type")
        | st', e' -> Cmd (If (e', c_then, c_else), st'))

    | Cmd (While (cond, body), st) -> Cmd(If(cond, Seq(body,While(cond,body)),Skip),st)
  

(* Traccia passo-passo l'esecuzione di un programma *)
(* Prende un numero max di passi da eseguire (n_steps) e una rappresentazione di un programma *)
(* Restituisce una lista di configurazioni, ossia gli stati del programma in ogni configurazione *)
  let trace (n_steps : int) (Prog (d, c) : prog) : conf list =
    (* Associa il comando principale c con lo stato appena inizializzato *)
    let conf0 = Cmd (c, eval_decl state0 d) in
    (* Itera da i a n per computare i comandi *)
    let rec helper n conf =
      if n > 0 then
        try
          let conf' = trace1 conf in
          conf :: helper (n - 1) conf' (* prova ad andare avanti di un passo *)
        with NoRuleApplies -> [ conf ]
      else [ conf ] (* termina se il numero di passi è finito *)
    in
    helper n_steps conf0 (* avvia con il max numero di passi e la configurazione iniziale *)