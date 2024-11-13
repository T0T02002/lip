open BoolexprLib.Main

(*
let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = failwith "TODO"

(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = failwith "TODO" *)

let%test _ = parse "true" = True;;
let%test _ = parse "false" = False;;
let%test _ = parse "if true then false else true" = If (True, False, True);;

let%test _ = parse "if true then false else true" |> eval = false;;
let%test _ = parse "if false then false else true" |> eval = true;;
let%test _ = parse "if false then false else false" |> eval = false;;
let%test _ = parse "if if true then true else false then true else false" |> eval = true;;
let%test _ = parse "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" |> eval = false;;

let%test _ = parse "if (if true then false else true) then true else false" |> trace = [If (If (True, False, True), True, False);
If (False, True, False);
False];;


let parsed = parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" |> trace ;;
let foo acc x = match x with _ -> acc+1 ;;  
  

let%test _ = List.fold_left foo 0 parsed <= 10;; 

let%test _ = parse "if true || true then false else true" |> eval = false;;
