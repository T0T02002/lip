open Ast
open Types


(* apply è dichiarata in types *)

(* Prende una stringa in input rappresentante il comando da analizzare "x:=0" in una 
 * rappresentazione strutturata di tipo cmd ( ad esempio (Assign of string * expr)) *)
let parse (s : string) : cmd =  
  (* trasforma s in un buffer lessicale utilizzato dal lexer per leggere incrementalmente *) 
  let lexbuf = Lexing.from_string s in  
  (* Lexer.read è il lexer che scansiona il buffer lexbuf e lo dà in pasto al parser Parser.prog *)
  (* Parser.prog riceve i token (es. TRUE) dal lexer, li analizza e li trasforma in AST (NOT;e0=expr;{Not(e0)}) *)
  let ast = Parser.prog Lexer.read lexbuf in  
  ast (* Il risultato è in ast *)

(* eccezioni dichiarate in types *)

(* botenv e botmem si chiamano bottom_env e bottom_mem e si trovano in types *)

(* per bind esiste un bind_mem e un bind_env in types. Esiste anche bind_fun ma non so cosa svolga *)

(* CHECK, SEMBRA INUTILIZZATA *)
let is_val = function
    True -> true
  | False -> true
  | Const _ -> true
  | _ -> false

(* BIG STEP SEMANTIC, restituisce senza passaggi intermedi un risultato *)
(* Prende in input state (funzione che mappa una variabile un valore in un certo stato)
 * e expr (espressione da valutare) 
 let rec eval_expr (state : state) (expr : expr) : memval =
  match expr with
  | True -> Bool true
  | False -> Bool false

  | And (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> failwith "I parametri di And devono essere booleani")

  | Or (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> failwith "I parametri di Or devono essere booleani")

  | Not e0 -> (
      match eval_expr state e0 with
      | Bool b -> Bool (not b)
      | _ -> failwith "Il parametro di Not deve essere booleano")

  | Add (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> failwith "I parametri di Add devono essere numeri")

  | Sub (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> failwith "I parametri di Sub devono essere numeri")

  | Mul (e0, e1) -> (
      match (eval_expr state e0, eval_expr state e1) with
      | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> failwith "I parametri di Mul devono essere numeri")

  | Eq (a, b) -> (
      match (eval_expr state a, eval_expr state b) with
      | Bool a, Bool b -> Bool (a = b)
      | Int a, Int b -> Bool (a = b)
      | _ -> failwith "I valori di Eq devono essere dello stesso tipo")

  | Leq (a, b) -> (
      match (eval_expr state a, eval_expr state b) with
      | Int a, Int b -> Bool (a <= b)
      | _ -> failwith "I valori di Leq devono essere entrambi numerici")

  | Const num -> Int num

  | Var var -> (
      let env = topenv state in
      match env var with
      | BVar loc
      | IVar loc -> (getmem state) loc
    )
  

  let eval_decl (state : state) (decl_list : decl list) : state =
    let (env, loc) =
      List.fold_left
        (fun (env, loc) decl ->
           match decl with
           | IntVar var -> (bind_env env var (IVar loc), loc + 1)
           | BoolVar var -> (bind_env env var (BVar loc), loc + 1))
        (topenv state, getloc state)
        decl_list in
    let envstack = getenv state in
    make_state (env :: envstack) (getmem state) loc *)

      



(* SMALL STEP SEMANTIC: l'espressione viene valutata passo dopo passo usando 
una regola alla volta; visualizza gli stati intermedi

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

let rec trace1_expr state = function 
  | Var var -> (Const (apply state var), state)

  | Not(True) -> (False,state)
  | Not(False) -> (True,state)
  | Not(e0) -> let (e',state') = trace1_expr state e0 in (Not(e'),state')

  | And(True,e) -> (e,state)
  | And(False,_) -> (False,state)
  | And(e1,e2) -> let (e1',state') = trace1_expr state e1 in (And(e1',e2),state')

  | Or(True,_) -> (True,state)
  | Or(False,e) -> (e,state)
  | Or(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Or(e1',e2),state')

  | Add(Const(n1),Const(n2)) -> (Const(n1+n2),state)
  | Add(Const(n1),e) -> let (e',state') = trace1_expr state e in (Add(Const(n1),e'),state')
  | Add(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Add(e1',e2),state')
  
  | Sub(Const(n1),Const(n2)) -> (Const(n1-n2),state)
  | Sub(Const(n1),e) -> let (e',state') = trace1_expr state e in (Sub(Const(n1),e'),state')
  | Sub(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Sub(e1',e2),state')

  | Mul(Const(n1),Const(n2)) -> (Const(n1*n2),state)
  | Mul(Const(n1),e) -> let (e',state') = trace1_expr state e in (Mul(Const(n1),e'),state')
  | Mul(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Mul(e1',e2),state')

  | Eq(Const(n1),Const(n2)) -> if n1=n2 then (True,state) else (False,state)
  | Eq(Const(n1),e) -> let (e',state') = trace1_expr state e in (Eq(Const(n1),e'),state')
  | Eq(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Eq(e1',e2),state')

  | Leq(Const(n1),Const(n2)) -> if n1<=n2 then (True,state) else (False,state)
  | Leq(Const(n1),e) -> let (e',state') = trace1_expr state e in (Leq(Const(n1),e'),state')
  | Leq(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Leq(e1',e2),state')

  | Call(f,Const(n)) -> ( match (topenv state) f with 
      IFun(x,cmd,expr) -> 
        let loc = getloc state in
        let env' = bind_env (topenv state) x (IVar l) in (* CHECK bind_env, potrebbe essere chiamata in modo errato *)
        let mem' = bind_mem (getmem state) loc n in      (* CHECK *)
        let state' = (env'::(getenv state), mem', loc+1) in 
        (CallExec(cmd,expr),state')
      | _ -> raise (TypeError "Stai chiamando una funzione non esistente"))
  | Call(f,expr) -> let (expr',state') = trace1_expr state expr in (Call(f,expr'),state')
  
  | CallExec(cmd,expr) -> (match trace1_cmd (Cmd(cmd,state)) with
      St state' -> (CallRet(expr),state')
    | Cmd(cmd',state') -> (CallExec(cmd',expr),state'))

  | CallRet(Const(n)) -> let state' = (popenv state, getmem state, getloc state) in (Const(n),state')
  | CallRet(e) -> let (e',state') = trace1_expr state e in (CallRet(e'),state')
  
  | _ -> raise NoRuleApplies


and trace1_cmd = function
    St _ -> raise NoRuleApplies

  | Cmd(cmd,state) ->  match cmd with 
    | Skip -> St state 

    | Assign(x,Const(n)) -> ( match topenv state x with 
      | IVar loc -> St (getenv state, bind_ivar (getmem state) loc n, getloc state) (* CHECK *)
      | _ -> failwith " todo error message ")
    | Assign(x,expr) -> let (expr',state') = trace1_expr state expr in Cmd(Assign(x,expr'),state')

    | Seq(c1,c2) -> ( match trace1_cmd (Cmd(c1,st)) with 
      | St state1 -> Cmd(c2,state1)
      | Cmd(c1',state1) -> Cmd(Seq(c1',c2),state1))
    
    | If(True,c1,_) -> Cmd(c1,state)
    | If(False,_,c2) -> Cmd(c2,state)
    | If(expr,c1,c2) -> let (e',state') = trace1_expr state expr in Cmd(If(e',c1,c2),state')

    | While(expr,cmd) -> Cmd(If(expr,Seq(cmd,While(expr,cmd)),Skip),state)


(* CHECK TUTTA *)
let rec sem_decl (e,l) = function
  | EmptyDecl -> (e,l)
  | IntVar(x) ->  let e' = bind e x (IVar l) in (e',l+1)
  | Fun(f,x,c,er) -> let e' = bind e f (IFun(x,c,er)) in (e',l)
  | DSeq(d1,d2) -> let (e',l') = sem_decl (e,l) d1 in sem_decl (e',l') d2


(* CHECK TUTTA *)
let rec trace_rec n t =
  if n<=0 then [t]
  else try
    let t' = trace1_cmd t
    in t::(trace_rec (n-1) t')
  with NoRuleApplies -> [t]  

 
