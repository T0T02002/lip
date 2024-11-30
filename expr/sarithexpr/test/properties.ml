open SarithexprLib.Ast
open SarithexprLib.Main 
open QCheck

(* ################################################
   Helpers for building expr values
   ################################################ *)

(* Converte un valore booleano OCaml (true o false) in un'espressione booleana del tipo expr (True o False) *)
let expr_of_bool b = if b then True else False

(* Converte un numero naturale OCaml (int) in un'expr che è  o Zero o Succ(e) *)
let rec expr_of_nat = function
  | 0 -> Zero
  | n -> Succ (expr_of_nat (n - 1))

(* Prendono i parametri necessari e li trasformano nella expr corrispondente *)
let epred e = Pred e
let esucc e = Succ e
let enot e = Not e
let eiszero e = IsZero e
let eand a b = And (a,b)
let eor a b = Or (a,b)
let eif a b c = If (a,b,c)

(* ################################################
   Formalization of progress and preservation properties
   ################################################ *)

(* Determina se un'espressione è un valore numerico *)   
let rec is_nv : expr -> bool = function
  | Zero -> true
  | Succ e -> is_nv e
  | _ -> false

(* Determina se un'espressione è di tipo Booleana *)  
let is_bv = function
  | True | False -> true
  | _ -> false

(* Determina se l'espressione è un valore (sia bool che natur) *)  
let is_value e = is_bv e || is_nv e

(* Determina se un'espressione può fare un passo di riduzione (eseguire regola di riduzione) *)
let can_take_step e =
  try
    ignore (trace1 e); true (* usa trace e dà true se il passo è possibile, false altrimenti *)
  with NoRuleApplies -> false

(* Controlla se un'espressione ha un tipo (Se supera typecheck) *)  
let typechecks e =
  try
    ignore (typecheck e); true
  with _ -> false

(* Formalizza progress; ogni expr ben tipata è un valore o può fare un passo di valutazione *)  
let progress e =
  typechecks e ==>
  (is_value e || can_take_step e)

(* Formalizza preservation; se un'espressione ben tipata effettua un passo di valutazione, il tipo rimane lo stesso *)  
let preservation e =
  try
    let ty1 = typecheck e in
    let e' = trace1 e in
    let ty2 = typecheck e' in
    ty1 = ty2
  with _ -> true

(* ################################################
   QCheck setup & test running
   Note: progress will fail!
   ################################################ *)

(* Genera espressioni casuali *)   
let expr_gen = 
  QCheck.Gen.(
    sized @@ fix (fun self n -> match n with
    | 0 -> map expr_of_bool bool
    | 1 -> map expr_of_nat nat
    | n ->
      frequency [
        1, map epred (self (n/2));
        1, map esucc (self (n/2));
        1, map eiszero (self (n/2));
        1, map enot (self (n/2));
        1, map2 eand (self (n/2)) (self (n/2));
        1, map2 eor (self (n/2)) (self (n/2));
        1, map3 eif (self (n/3)) (self (n/3)) (self (n/3));
      ]
    ));;

(* trasforma in stringa l'espressione generata *)    
let arbitrary_expr =
  QCheck.make expr_gen ~print:string_of_expr

(* Crea i test per progress e preservation *)  
let test_progress =
  QCheck.Test.make ~name:"test_progress" arbitrary_expr progress

let test_preservation =
  QCheck.Test.make ~name:"test_preservation" arbitrary_expr preservation
;;

(* Esegue i test *)
QCheck_runner.run_tests [test_progress; test_preservation];;