open BoolexprLib.Main


let%test _ = parse "true" = True;; 
let%test _ = parse "false" = False;;
let%test _ = parse "if true then false else true" = If (True, False, True);;


(* Task 4 *)
let%test _ = parse "if true then false else true" |> eval = false;;
let%test _ = parse "if false then false else true" |> eval = true;;
let%test _ = parse "if false then false else false" |> eval = false;;
let%test _ = parse "if if true then true else false then true else false" |> eval = true;;
let%test _ = parse "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" |> eval = false;;


(* Task 5 *)
let%test _ = parse "if (if true then false else true) then true else false" |> trace = [If (If (True, False, True), True, False);
If (False, True, False);
False];;

let task5_string = "if (if false then false else false) then (if false then true else false) else (if true then false else true)";;
let%test _ = count_trace_elements task5_string <= 10;; 


(* Task 6 *)
let%test _ = parse "true && false" |> eval = false;; (* if a then b else false *)
let%test _ = parse "true || false" |> eval = true;;  (* if a then true else b  *)
