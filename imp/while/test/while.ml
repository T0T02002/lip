open WhileLib.Ast
open WhileLib.Types
open WhileLib.Prettyprint       
open WhileLib.Main

(* open WhileLib.Ast;;open WhileLib.Types;;open WhileLib.Prettyprint;;open WhileLib.Main;;open WhileLib__Main;; *)

(**********************************************************************
 parse test : (variable, term, expected result)
 **********************************************************************)

(* Prende in input una stringa cmd e un valore atteso, li confronta e restituisce un booleano se sono uguali *)
let test_parse_og cmd exp_result = cmd |> parse |> fun c -> c = exp_result 
let test_parse cmd exp_result = 
  (* parse "x:=0" diventa Assign("x", Const 0) *)
  let parsed_cmd = parse cmd in   
  (* confronto tra l'AST calcolato e quello fornito *)
  if parsed_cmd = exp_result then true else false 
 


(* custom test con un nome di variabile compatibile con C *)
let%test "test_parse0" = test_parse "aA_125_:=0" (Assign("aA_125_",Const(0)))

let%test "test_parse1" = test_parse "x:=0" (Assign("x",Const(0)))

let%test "test_parse2" = test_parse
    "x:=0; y:=x+1" (Seq(Assign("x",Const(0)),Assign("y",Add(Var("x"),Const(1)))))    

let%test "test_parse3" = test_parse
    "x:=0; if x=0 then y:=1 else y:=0" (Seq(Assign("x",Const(0)),If(Eq(Var("x"),Const(0)),Assign("y",Const(1)),Assign("y",Const(0)))))

let%test "test_parse4" = test_parse
    "x:=0; if x=0 then y:=1 else y:=0; x:=2" (Seq(Seq(Assign("x",Const(0)),If(Eq(Var("x"),Const(0)),Assign("y",Const(1)),Assign("y",Const(0)))),Assign("x",Const(2))))
    
let%test "test_parse5" = test_parse
    "x:=3; while x<=0 do x:=x-1; y:=0" (Seq(Seq(Assign("x",Const(3)),While(Leq(Var "x",Const 0),Assign("x",Sub(Var "x",Const 1)))),Assign("y",Const(0))))


(**********************************************************************
 trace test : (command, n_steps, variable, expected value after n_steps)
 **********************************************************************)

(* test_trace prende: 
 * cmd (una stringa con il comando del linguaggio),
 * n_steps (un intero, il numero massimo di passaggi da tracciare),
 * var (una stringa contentente il nome di una variabile di cui vogliamo verificare il valore finale),
 * exp_val (un valore di tipo exprval contenente il valore (TIPO,VALUE) della variabile alla fine (Nat,0)).
 * Restituisce true o false se il risultato atteso con i parametri assegnati è corretto *)
let test_trace (cmd,n_steps,var,exp_val) = cmd
  (* parsing del comando, ossia stringa trasformata da "x:=0" a Assign("x",Const(0)) *)
  |> parse 
  (* esegue il comando c per n_steps, last restituisce l'ultima configurazione di tipo conf (state | (command,state))*)
  |> fun c -> last (trace n_steps c) 
  (* se la configurazione finale è di tipo St s il programma termina. Confronta se è uguale al valore atteso exprval *)
  (* se nell'ultima configurazione rimane un comando da eseguire Cmd(_,s), si confronta lo stesso var con exprval *)
  |> fun t -> match t with
      St s -> s var = exp_val
    | Cmd(_,s) -> s var = exp_val



(* Comando: "x:=0" ossia l'assegnamento di 0 a x *)
(* Passi: ci si aspetta 1 passo solo, dal momento che Assign("x",Const(0)) è un solo step *)
(* Variabile: si vuole verificare il valore di 'x' *)
(* Exprval: il valore di x deve essere di tipo (Tipo,Value), in questo caso (Nat,0) *)
let%test "test_trace1" = test_trace
    ("x:=0", 1, "x", Nat 0)

let%test "test_trace2" = test_trace
    ("x:=0; x:=x+1", 2, "x", Nat 1) (* il numero di steps equivale al numero di SEQ ';' *)

let%test "test_trace3" = test_trace
    ("x:=0; y:=x+1; x:=y+1", 3, "x", Nat 2)
    
let%test "test_trace4" = test_trace
    ("x:=0; if x=0 then y:=1 else y:=2", 3, "y", Nat 1)

let%test "test_trace5" = test_trace
    ("x:=1; if x=0 then y:=1 else y:=2", 3, "y", Nat 2)

let%test "test_trace6" = test_trace
  ("x:=3; y:=2; r:=0; while 1<=y do (r:=r+x; y:=y-1)", 10, "r", Nat 6)

let%test "test_trace7" = test_trace
  ("x:=3; while 0<=x and not 0=x do x:=x-1; x:=5", 10, "x", Nat 5)

let%test "test_trace8" = test_trace
  ("x:=5; y:=3; if x<=y then min:=x else min:=y", 10, "min", Nat 3)

let%test "test_trace9" = test_trace
  ("x:=1; y:=2; z:=3; if x<=y and x<=z then min:=x else (if y<=z then min:=y else min:=z)", 10, "min", Nat 1)

let%test "test_trace10" = test_trace
    ("x:=2; y:=1; z:=3; if x<=y and x<=z then min:=x else (if y<=z then min:=y else min:=z)", 10, "min", Nat 1)

let%test "test_trace11" = test_trace
    ("x:=2; y:=3; z:=1; if x<=y and x<=z then min:=x else (if y<=z then min:=y else min:=z)", 10, "min", Nat 1)


(* EOF *) 
let%test_unit _ =
  print_endline "END TEST WHILE"



