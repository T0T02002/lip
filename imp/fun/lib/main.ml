open Ast
open Types


(* prende una stringa, la passa al lexer per dividerla in token, i token son letti dal 
   parser il quale costruisce l'AST.   *)
let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
(*  parse "int x; x:=1";;  *)
(* - : prog = Prog (DSeq (IntVar "x", EmptyDecl), Assign ("x", Const 1))   *)


(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
    
let bind f x v = fun y -> if y=x then v else f y


let rec trace1_expr state = function
  | Var x -> (Const(apply state x), state)

  | Not(True) -> (False,state)
  | Not(False) -> (True,state)
  | Not(e) -> let (e',state') = trace1_expr state e in (Not(e'),state')

  | And(True,e) -> (e,state)
  | And(False,_) -> (False,state)
  | And(e1,e2) -> let (e1',state') = trace1_expr state e1 in (And(e1',e2),state')

  | Or(True,_) -> (True,state)
  | Or(False,e) -> (e,state)
  | Or(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Or(e1',e2),state')

  | Add(Const(n1),Const(n2)) -> (Const(n1+n2),state)
  | Add(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Add(Const(n1),e2'),state')
  | Add(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Add(e1',e2),state')

  | Sub(Const(n1),Const(n2)) -> (Const(n1-n2),state)
  | Sub(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Sub(Const(n1),e2'),state')
  | Sub(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Sub(e1',e2),state')

  | Mul(Const(n1),Const(n2)) -> (Const(n1*n2),state)
  | Mul(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Mul(Const(n1),e2'),state')
  | Mul(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Mul(e1',e2),state')

  | Eq(Const(n1),Const(n2)) -> if n1=n2 then (True,state) else (False,state)
  | Eq(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Eq(Const(n1),e2'),state')
  | Eq(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Eq(e1',e2),state')

  | Leq(Const(n1),Const(n2)) -> if n1<=n2 then (True,state) else (False,state)
  | Leq(Const(n1),e2) -> let (e2',state') = trace1_expr state e2 in (Leq(Const(n1),e2'),state')
  | Leq(e1,e2) -> let (e1',state') = trace1_expr state e1 in (Leq(e1',e2),state')

  | Call(f,Const(n)) -> (match (topenv state) f with
        IFun(x,c,er) -> 
        let l = getloc state in
        let env' = bind (topenv state) x (IVar l) in
        let mem' = bind (getmem state) l n in
        let state' = (env'::(getenv state), mem', l+1) in
        (CallExec(c,er),state')
      | _ -> raise (TypeError "Call of a non-function"))
  | Call(f,e) -> let (e',state') = trace1_expr state e in (Call(f,e'),state')

  | CallExec(c,e) -> (match trace1_cmd (Cmd(c,state)) with
    | St state' -> (CallRet(e),state')
    | Cmd(c',state') -> (CallExec(c',e),state'))

  | CallRet(Const(n)) -> let state' = (popenv state, getmem state, getloc state) in (Const(n),state')
  | CallRet(e) -> let (e',state') = trace1_expr state e in (CallRet(e'),state')

  | _ -> raise NoRuleApplies

and trace1_cmd = function
  | St _ -> raise NoRuleApplies

  | Cmd(c,state) -> match c with
    | Skip -> St state

    | Assign(x,Const(n)) -> (match topenv state x with
      | IVar l -> St (getenv state, bind (getmem state) l n, getloc state)
      | _ -> failwith "improve err msg")
    | Assign(x,e) -> let (e',state') = trace1_expr state e in Cmd(Assign(x,e'),state') 

    | Seq(c1,c2) -> (match trace1_cmd (Cmd(c1,state)) with
        | St st1 -> Cmd(c2,st1)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))

    | If(True,c1,_) -> Cmd(c1,state)
    | If(False,_,c2) -> Cmd(c2,state)
    | If(e,c1,c2) -> let (e',state') = trace1_expr state e in Cmd(If(e',c1,c2),state')

    | While(e,c) -> Cmd(If(e,Seq(c,While(e,c)),Skip),state)

let rec sem_decl (e,l) = function
  | EmptyDecl -> (e,l)
  | IntVar(x) ->  let e' = bind e x (IVar l) in (e',l+1)
  | Fun(f,x,c,er) -> let e' = bind e f (IFun(x,c,er)) in (e',l)
  | DSeq(d1,d2) -> let (e',l') = sem_decl (e,l) d1 in sem_decl (e',l') d2


let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]


(**********************************************************************
 trace : int -> prog -> conf list

 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

let trace n (Prog(d,c)) =
  let (e,l) = sem_decl (botenv,0) d
  in trace_rec n (Cmd(c,([e],botmem,l)))