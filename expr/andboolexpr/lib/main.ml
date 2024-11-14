open Ast
 
(* Trasforma una boolexpr in una stringa letterale *)
let rec string_of_boolexpr = function
    True -> "True" 
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Not (e0) -> "not "^(string_of_boolexpr e0)
  | And (e0,e1) -> (string_of_boolexpr e0) ^ "and" ^ (string_of_boolexpr e1)
  | Or (e0,e1) -> (string_of_boolexpr e0) ^ "or" ^ (string_of_boolexpr e1)

(* Esegue le regole di parsing su una stringa *)
let parse (s : string) : boolExpr = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

(* SMALL STEP SEMANTIC: l'espressione viene valutata passo dopo passo usando una regola alla volta, visualizza gli stati intermedi*)
(* Applica una regola di valutazione all'espressione booleana, con contempla l'if nativo di ocaml *)
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0, e1, e2) -> If(trace1 e0,e1,e2)

  | Not(False) -> True
  | Not(True) -> False
  | Not(e0) -> Not(trace1 e0)

  | And(True,True) -> True
  | And(_,False)
  | And(False,_) -> False  (* qualsiasi espressione con uno dei due falso sarà falsa *)
  | And(e0,e1) -> And(trace1 e0,e1) (* altrimenti riduce l'albero (serve una virgola)*)

  | Or(True,_) 
  | Or(_,True) -> True    (* qualsiasi espressione con uno dei due vero sarà vera *)
  | Or(False,False) -> False
  | Or(e0,e1) -> Or(trace1 e0,e1) (* altrimenti riduce l'albero *)

  | _ -> raise NoRuleApplies

(* Rende una lista di boolxepr ogni trace *)
let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


(* BIG STEP SEMANTIC: l'espressione viene valutata senza passi intermedi, ricorsione che riduce l'espressione in un singolo passo*)
(* Valuta una boolExpr per ottenere true o false, contempla l'if nativo di ocaml*)
let rec eval = function
    True -> true
  | False -> false
  | If(e0,e1,e2) -> if (eval e0) then (eval e1) else (eval e2)
  | Not(e0) -> not (eval e0)
  | And(e0,e1) -> (eval e0) && (eval e1)
  | Or(e0,e1) -> (eval e0) || (eval e1) 
 

(* Prende in input una stringa, ne fa il parsing, le rende una boolExpr con trace e poi ne conta gli elementi*)
let count_trace_elements str  =
  let traced = parse str |> trace  in
  List.length traced
;;