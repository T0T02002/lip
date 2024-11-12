open Ast 

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

(* Tipo custom: è un'istanza di Result*)
(* Result è una tag union tra i costruttori Ok e Error*)
type int_or_err = (int, string) Result.t

(* Operatore per matchare detto bind *)
let ( ==> ) (res : int_or_err) (f : int -> int_or_err) : int_or_err =
  match res with
  | Ok value -> f value
  | Error msg -> Error msg

let string_of_intorerr : int_or_err -> string = function
  | Ok n -> string_of_int n
  | Error msg -> msg

(* eval : ast -> result    (REFACTORED TASK 6)*)
let rec eval : ast -> int_or_err = function

  | Const n -> Ok n

  | Sub (e1,e2) ->
    eval e1 ==> fun v1 ->
    eval e2 ==> fun v2 ->
    Ok (v1 - v2)

  | Neg (e1) ->
    eval e1 ==> fun v1 ->
    Ok (-v1)

  | Add (e1,e2) ->
    eval e1 ==> fun v1 ->
    eval e2 ==> fun v2 ->
    Ok (v1 + v2)

  | Mul (e1,e2) ->
    eval e1 ==> fun v1 ->
    eval e2 ==> fun v2 ->
    Ok (v1 * v2)

  | Div (e1,e2) ->
    eval e1 ==> fun v1 ->
    eval e2 ==> fun v2 ->
    if v2 = 0 then Error "Error: tried to divide by zero"
    else Ok (v1 / v2);;
    

  ;; 
    
(*  eval PRE-TASK 6

let rec eval : ast -> int_or_err = function

  | Const n -> Ok n

  | Sub (e1,e2) ->
    let res1 = eval e1 in
    let res2 = eval e2 in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 - v2) end

  | Neg (e1) ->
    let res1 = eval e1 in
    begin
    match res1 with
    | Error err1 -> Error err1
    | Ok v1 -> Ok (-v1) end  

  | Add (e1,e2) ->
    let res1 = eval e1 in
    let res2 = eval e2 in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 + v2) end

  | Mul (e1,e2) ->
    let res1 = eval e1 in
    let res2 = eval e2 in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 * v2) end

  | Div (e1,e2) ->
    let res1 = eval e1 in
    let res2 = eval e2 in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> if(v2=0) then Error "Error: tried to divide by zero" else Ok (v1 / v2) end
    
  ;; 

*)