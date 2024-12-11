open Ast

(* Eccezioni *)
exception NoRuleApplies
exception TypeError of string

(* Trasforma un'espressione in una stringa letterale, serve per gli errori *)
let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "if " ^ (string_of_expr e0) ^ " then " ^ (string_of_expr e1) ^ " else " ^ (string_of_expr e2)
  | Not(e0) -> "not " ^ (string_of_expr e0)
  | And(e0, e1) -> (string_of_expr e0) ^ " and " ^ (string_of_expr e1)
  | Or(e0, e1) -> (string_of_expr e0) ^ " or " ^ (string_of_expr e1)
  
  | Zero -> "0"
  | Succ(e0) -> "succ(" ^ (string_of_expr e0) ^ ")"
  | Pred(e0) -> "pred(" ^ (string_of_expr e0) ^ ")"
  | IsZero(e0) -> "iszero(" ^ (string_of_expr e0) ^ ")"


(* NEW Converte il tipo in una stringa *)
let string_of_type = function
  | BoolT -> "Bool"
  | NatT -> "Nat"


(* Prende un expr e ne determina il tipo exprtype *)
(* MIGLIORABILE cambiando messaggio di errore per ogni casistica *)
let rec typecheck = function 
    True | False -> BoolT

  | If(e0, e1, e2) -> ( (* condizione, ramo then, ramo else *)
    match typecheck e0, typecheck e1, typecheck e2 with (* valuta i tipi e gli assegna un valore exprtype *)
    | NatT,_,_ -> raise (TypeError "error") (* se la condizione è NatT, esce*)
    | BoolT, typethen, typeelse ->  if typethen = typeelse then typethen (* se then e else hanno lo stesso tipo, allora il tipo è valido e si valuta la funzione in then *)
                                    else raise (TypeError (string_of_expr e2 ^ " has type "^ string_of_type typeelse))) (* Errore se i tipi non coincidono *)
  
  | Not(e0) -> (
    match typecheck e0 with 
    | BoolT -> BoolT
    | NatT -> raise (TypeError "error")) (* messaggio di errore generico *)

  | And(e0,e1)
  | Or(e0,e1) -> (
    match typecheck e0, typecheck e1 with 
    | BoolT, BoolT -> BoolT 
    | NatT, _ -> raise (TypeError "error")
    | _, NatT -> raise (TypeError "error"))

  | Zero -> NatT

  | Succ(e0) -> (
    match typecheck e0 with
    | NatT -> NatT (* messaggio di errore specifico *)
    | BoolT -> raise (TypeError ("succ(" ^ (string_of_expr e0) ^ ") has type Bool, but type Nat was expected")))
  
  | Pred(e0) -> (
      match e0 with (* errore specifico per Pred(Zero) *)
      | Zero -> raise (TypeError "Pred(Zero) is not valid") (* Non riduce dopo Zero *)
      | _ -> (
          match typecheck e0 with
          | NatT -> NatT
          | BoolT -> raise (TypeError ("pred(" ^ (string_of_expr e0) ^ ") has type Bool, but type Nat was expected"))))

  | IsZero(e0) -> (
    match typecheck e0 with 
    | NatT -> BoolT (* se l'argomento di isZero è un int allora il risultato è bool *)
    | BoolT -> raise (TypeError "error")) (* altrimenti non ha senso *)
  
  
(* Converte exprval in stringa, serve per leggere un valore *)
let string_of_val : exprval -> string = function
  | Bool true -> "True"
  | Bool false -> "False"
  | Nat n -> string_of_int n 


(* Esegue le regole di parsing su una stringa *)
let parse (s : string) : expr =  
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Dà true se l'espressione è o Zero o Succ, false in tutti gli altri casi *)  
let rec is_nv = function 
  Zero -> true 
| Succ e1 ->is_nv e1 
| _ -> false


(* exception NoRuleApplies *)

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
  | And(False,_) -> False  
  | And (e0, True) -> And (trace1 e0, True) 
  | And (True, e0) -> And (True, trace1 e0) 
  | And(e0,e1) -> And(trace1 e0,e1)

  | Or(True,_) 
  | Or(_,True) -> True
  | Or(False,False) -> False
  | Or(e0, False) -> Or (trace1 e0, False)
  | Or(False, e0) -> Or (False, trace1 e0)
  | Or(e0,e1) -> Or(trace1 e0,e1)

  | Succ e0 -> Succ (trace1 e0) 

  | Pred Zero -> raise NoRuleApplies (* Non riduce dopo Zero *)
  | Pred (Succ e0) -> e0 
  | Pred e0 -> Pred (trace1 e0) 

  | IsZero Zero -> True
  | IsZero (Succ _ ) ->  False
  | IsZero e0 -> IsZero (trace1 e0)

  | _ -> raise NoRuleApplies

  
(* Rende una lista di boolxepr ogni trace *)
let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


(* BIG STEP SEMANTIC: l'espressione viene valutata senza passi intermedi, ricorsione che riduce l'espressione in un singolo passo*)
(* Valuta una boolExpr per ottenere true o false, contempla l'if nativo di ocaml*)
let rec eval = function
    True -> Bool true

  | False -> Bool false

  | If(e0,e1,e2) -> 
    (match eval e0 with 
    | Bool true -> eval e1 
    | Bool false -> eval e2
    | _ -> failwith "If valuta solo su Bool")

  | Not(e0) ->  
    (match eval e0 with
    | Bool e -> Bool (not e)
    | _ -> failwith "Not valuta solo su Bool")

  | And(e0,e1) -> 
    (match eval e0, eval e1 with
    | Bool a, Bool b -> Bool (a && b)
    | _ -> failwith "And valuta solo su Bool")
  
  | Or(e0,e1) -> 
    (match eval e0, eval e1 with
    | Bool a, Bool b -> Bool (a || b)
    | _ -> failwith "Or valuta solo su Bool")

  | Zero -> Nat 0

  | Succ(e0) -> 
    (match eval e0 with 
    | Nat n -> Nat (n+1)
    | _ -> failwith "Succ lavora su Nat" )

  | Pred(e0) -> 
    (match eval e0 with 
    | Nat n -> if n>0 then Nat (n-1) else failwith "Non puoi togliere da 0"
    | _ -> failwith "Pred lavora su Nat" )
  
  | IsZero(e0) ->
    (match eval e0 with 
    | Nat 0 -> Bool true 
    | Nat n -> if n>0 then Bool false else failwith "Non esiste isZero di negativi" 
    | _ -> failwith "IsZero lavora sui Nat")

 

(* Prende in input una stringa, ne fa il parsing, le rende una boolExpr con trace e poi ne conta gli elementi*)
let count_trace_elements str  =
  let traced = parse str |> trace  in
  List.length traced
;;