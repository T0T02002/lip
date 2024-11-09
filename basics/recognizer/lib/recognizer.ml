(* [01]+ *)
let rec lang1 (l : char list) = match l with
  [] -> false
  |['1'] -> true
  |['0'] -> true
  | x::tail -> if(x=='0'||x=='1') then lang1 tail else false
;;


(* 0?1* *)
(* riconosce solo 1 (o nessuno) *)
let rec lang2_aux l = match l with
  [] -> true                  (* vuoto, sottinteso come caso true *)
| ['1'] -> true       (* solo un 1 *)
| '1'::tail -> lang2_aux tail
| _ -> false
;;
(* riconosce l'eventuale presenza di uno 0 *)
let lang2 l = match l with
  [] -> true (* nessun 0 e nessun 1 *)
| '0'::tail
| '1'::tail -> lang2_aux tail  (* 0 o 1 a inizio parola*)
| _ -> false
;;


(* 0[01]*0 *)
(* la stringa non può concludersi con 1 ma solo con 0, sequenze di 0 o 1 sono accettate*)
let rec lang3_aux l = match l with
  ['0'] -> true
| '0'::tail
| '1'::tail -> lang3_aux tail
| _ -> false
;;
(* riconosce se la parola inizia con 0 *)
let lang3 l = match l with
  '0'::tail -> lang3_aux tail
| _ -> false
;;


(* 0*10*10* *)
let rec lang4_2 (l : char list) = match l with
  [] -> false
  |['0'] -> true
  | a::tail -> if(a=='0') then lang4_2 tail else false;;
let rec lang4_1 (l : char list) = match l with
  [] -> false
  | a::tail -> if(a=='1') then lang4_2 tail else if(a=='0') then lang4_1 tail else false;;
let rec lang4 (l : char list) = match l with
  [] -> false
  | a::tail -> if(a=='1') then lang4_1 tail else if(a=='0') then lang4 tail else false
;;


(* (00|11)+ *)
let rec lang5 (l : char list) = match l with
  [] -> false
  | a :: tail -> if(a=='0') then lang5_0 tail else if (a=='1') then lang5_1 tail else false
and lang5_0 (l : char list) = match l with
  [] -> false
  | a :: tail -> if(a=='0') then lang5_2 tail else false
and lang5_1 (l : char list) = match l with
  [] -> false
  | a :: tail -> if(a=='1') then lang5_2 tail else false
and lang5_2 (l : char list) = match l with
  [] -> true
  | a :: tail -> if(a=='0') then lang5_0 tail else if(a=='1') then lang5_1 tail else false
;;
    


let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
