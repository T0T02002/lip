open Ast
open Types
(* open Prettyprint  *) (* DECOMMENTA PER ABILITARE IL DEBUG *)


(* Modulo parametrico Nice_parser di nome P. Prende come param una struttura definita come segue: *)
module P = Nice_parser.Make (struct
  type result = Ast.prog    (* Il risultato del parsing, ossia un AST *)
  type token = Parser.token (* I token del parser *)
  exception ParseError = Parser.Error (* Debug errori del parser *)
  let parse = Parser.prog  (* Le regole di parsing *)
  include Lexer  (* include la logica di lexing *)
end)

(* Abilita gli errori di Nice_parser *)
let _ = P.pp_exceptions ()

(* Consente di parsare stringhe (?) *)
let parse = P.parse_string

(* Restituisce uno stato con le dichiarazioni processate*)
let exec_decl (st : state) (dl : decl list) : state =
  let loc', env' = List.fold_left
      (fun (l, env) d ->
        ( l + 1,
          match d with
          | IntVar x -> bind_env env x (IVar l)
          | Fun (f, par, body, e) -> bind_env env f (IFun (par, body, e)) ))
      (getloc st, topenv st) dl in
  make_state (env' :: getenv st) (getmem st) loc'


(* Modulo MONADE per la gestione funzionale dello stato *)  
module WithState = struct
  (* prende uno stato state e restituisce una coppia composta dallo stato e un tipo polimorfo *)
  type 'a t = state -> state * 'a 

  (* restituisce il valore e senza cambiare lo stato *)
  let return (e : 'a) : 'a t = fun st -> (st, e)

  (* imposta lo stato aggiornandolo e restituisce (), ossia nessuno stato significativo *)
  let set (st : state) : unit t = fun _ -> (st, ())

  (* Recupera lo stato attuale senza modificarlo *)
  let get : state t = fun st -> (st, st)

  (* Esegue il calcolo e ottenendo un nuovo stato state' e un valore a.*)
  (* Passa alla funzione next che produce un nuovo calcolo basato su a e lo stato aggiornato. *)
  let bind (e : 'a t) (next : 'a -> 'b t) : 'b t =
   fun st -> let st', a = e st in next a st'

  (* Mappa let$ per indicare che si stanno eseguendo operazioni monadiche con stati *) 
  let ( let$ ) = bind
end

(* Rende disponibili le operazioni di WithState *)
open WithState



(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

(* Prende un'expr e restituisce una versione ridotta con la small step semantic *)
(* Sfrutta il calcolo monadico di WithState leggendo e modificando lo stato del programma *)
let rec trace1_expr (e : expr) : expr WithState.t =
  (* print_endline (string_of_expr e); *)  (* ABILITA PER DEBUG *)
  match e with

  (* Espressioni restituite così come sono senza cambiare stato *)
  | True -> return True
  | False -> return False 
  | Const(num) -> return (Const(num))

  | Var(var) -> 
      let$ state = get in  (* Recupera lo stato globale con un'operazione monadica *)
      return (Const (apply state var)) (* Ottiene il valore di var attraverso apply nello stato attuale *)

  | Not(True) -> return False
  | Not(False) -> return True
  | Not(e) -> let$ e' = trace1_expr e in 
      return (Not(e')) (* Riduzione ricorsiva creando un'espressione diversa e' *)

  | And(False,_) -> return False  (* caso base *)
  | And(True,e2) -> trace1_expr e2  (* riduzione secondo termine*)
  | And(e1, e2) -> let$ e1' = trace1_expr e1 in (* riduzione primo termine *)
      return (And(e1',e2))

  | Or(True,_) -> return False
  | Or(False,e2) -> trace1_expr e2
  | Or(e1, e2) -> let$ e1' = trace1_expr e1 in
      return (Or(e1',e2))

  | Add(Const(n1),Const(n2)) -> return (Const(n1+n2))
  | Add((Const _ as e1), e2) -> let$ e2' = trace1_expr e2 in
      return (Add(e1,e2'))
  | Add (e1, e2) -> let$ e1' = trace1_expr e1 in
      return (Add(e1',e2))

  | Sub(Const(n1),Const(n2)) -> return (Const(n1-n2))
  | Sub((Const _ as e1), e2) -> let$ e2' = trace1_expr e2 in
      return (Sub(e1,e2'))
  | Sub (e1, e2) -> let$ e1' = trace1_expr e1 in
      return (Sub(e1',e2))

  | Mul(Const(n1),Const(n2)) -> return (Const(n1*n2))
  | Mul((Const _ as e1), e2) -> let$ e2' = trace1_expr e2 in
      return (Mul(e1,e2'))
  | Mul (e1, e2) -> let$ e1' = trace1_expr e1 in
      return (Mul(e1',e2))

  | Eq(Const(n1),Const(n2)) -> return (if Int.equal n1 n2 then True else False)
  | Eq((Const _ as e1), e2) -> let$ e2' = trace1_expr e2 in 
      return (Eq(e1,e2'))
  | Eq(e1,e2) -> let$ e1' = trace1_expr e1 in 
      return (Eq(e1',e2))

  | Leq(Const(n1),Const(n2)) -> return (if Int.compare n1 n2 <= 0 then True else False)
  | Leq((Const _ as e1), e2) -> let$ e2' = trace1_expr e2 in 
      return (Leq(e1,e2'))
  | Leq(e1,e2) -> let$ e1' = trace1_expr e1 in 
      return (Leq(e1',e2))

  (* Chiamata diretta dal programma con argomento costante, foo(n) *)    
  | Call(foo,Const(num)) -> 
      let$ state = get in  (* recupera lo stato corrente che contiene env, mem e firstloc *)
      let param, body, retexpr = apply_fun state foo in  (* cerca foo nell'ambiente toplevel e restituisce il nome del parametro, il corpo dei comandi e l'espressione da restituire *)
      let loc = getloc state in   (* Calcola una nuova locazione recuperando il primo indirizzo libero *)
      let env' = bind_env (topenv state) param (IVar loc) in (* associa param alla nuova locazione di memoria nell'ambiente top-level corrente *)
      let mem' = bind_mem (getmem state) loc num in  (* associa loc al valore num nella memoria dello stato *)
      let$ _ = set  (* aggiona l'envstack pushando un nuovo env e crea un nuovo stato con un nuovo envstack, mem e firstloc+1 *)
      (make_state (pushenv state env') mem' (loc+1)) in
      return (CallExec(body,retexpr))   (* restituisce un'expr che avrà come compito solo la valutazione della funzione *)

  (* Chiamata con argomento generico, foo(n+1)  *)  
  | Call(foo,e) -> let$ e' = trace1_expr e in  (* Riduzione dell'argomento ricorsiva *)
      return (Call(foo,e'))

  (* Valutazione dei comandi della funzione chiamata *)  
  | CallExec(body,retexpr) -> ( 
      let$ state = get in (* si recupera lo stato attuale del programma*)
      match trace1_cmd (Cmd(body,state)) with  (* si valuta passo-passo il comando da eseguire e si confronta con un pattern matching *)
      | St state' -> let$ _ = set state' in  (* la riduzione del comando è uno stato: computazione finita *)
          return (CallRet retexpr)  (* dpbbimo esclusivamente ritornare il valore di expr, se ne occuperà CallRet *)
      | Cmd(body',state') -> let$ _ = set state' in  (* la riduzione dà vita a un nuovo comando: ripeti *)
          return (CallExec (body',retexpr)))
  
  (* Otteniamo un valore di ritorno pulito: restituiamo *)  
  | CallRet(Const(num)) -> 
      let$ state = get in  (* ottiene lo stato attuale *)
      let$ _ = set (setenv state (popenv state)) in (* rimuove l'env temporaneo (funzione conclusa) *)
      return (Const num) (* restituisce *)

  (* Otteniamo un valore di ritorno composto *)  
  | CallRet(e) -> let$ e' = trace1_expr e in (* riduce ricorsivamente l'espressione di ritorno*)
      return (CallRet e')

  (* NON SI HANNO ALTRI CASI | _ -> raise NoRuleApplies *)


(* Implementa la small-step per i comandi. *)
(* I comandi sono nella forma conf, ossia stato finale (St) o da ridurre (Cmd) *)
and trace1_cmd : conf -> conf = function
  (* Stato finale*)
  | St _ -> raise NoRuleApplies

  (* Stato da ridurre*)
  | Cmd(cmd,state) -> ( match cmd with
      | Skip -> St state (* non fa niente e restituisce lo stato corrente *)
      | Assign(var,e) ->  (* matcha l'assegnazione di una variabile *)
        (match trace1_expr e state with
          | state', Const(num) -> (* l'expr è ridotta a un numero *)
              let state'' = bind_ivar state' var num in St state'' (* aggiorna lo stato e viene restituito *)
          | state', e' -> Cmd(Assign(var,e'), state') ) (* altrimenti riduci l'espressione *)

      | Seq(c1,c2) -> (* Gestisce una sequenza di comandi *)
        (match trace1_cmd @@ Cmd(c1,state) with (* valuta preventivamente c1 *)
          | St state' -> Cmd(c2,state') (* se c1 è completamente valutato, passa a c2 col nuovo stato *)
          | Cmd(c1',state') -> Cmd(Seq(c1',c2),state')) (* se c1 non è valutato, esegue un passo su c1 con un nuovo stato e ripete *)

      | If(e_cond,c_then,c_else) -> 
        ( match trace1_expr e_cond state with  (* valuta l'espressione condizionale *)
          | state', True -> Cmd(c_then,state')
          | state', False -> Cmd(c_else,state')
          | state', e_cond' -> Cmd (If (e_cond',c_then,c_else), state')) (* aggiorna stato e condizione e ripeti *)

      | While(e_cond,cbody) -> 
          Cmd (If (e_cond, Seq (cbody, cmd), Skip), state)) (* workaround:*)
          (* se e_cond è true esegue cbody e ripete il ciclo *)
          (* se e_cond è false passa al comando skip*)



(**********************************************************************
 trace : int -> prog -> conf list

 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

(* Traccia passo-passo l'esecuzione di un programma *)
(* Prende un numero max di passi da eseguire (n_steps) e una rappresentazione di un programma *)
(* Restituisce una lista di configurazioni, ossia gli stati del programma in ogni configurazione *)
let trace (n_steps : int) (Prog (dl, c) : prog) =
  (* Analizza lo stato globale e importa le dichiarazioni *)
  let state0 = exec_decl state0 dl in
  (* Associa il comando principale c con lo stato appena inizializzato *)
  let conf0 = Cmd(c, state0) in
  (* Itera da i a n_steps per computare i comandi *)
  let rec go (i : int) (conf : conf) =
    if i >= 0 then (
      try conf :: go (i - 1) (trace1_cmd conf) (* prova ad andare avanti di un passo *)
      with NoRuleApplies ->
        [ conf ])
    else [] (* termina se il numero di passi è finito *)
  in
  go n_steps conf0 (* avvia con il max numero di passi e la configurazione iniziale *)

  