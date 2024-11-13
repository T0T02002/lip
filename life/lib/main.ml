module T = ANSITerminal
open Printf

(* let rec range a b = if b<a then [] else a::(range (a+1) b) *)


(* zeroes prende una lista n e ne sostituisce ogni elemento con zeri*)
let rec zeroes n = (* int -> bool list *)
  match n with
  | 0 -> []
  | n -> false :: (zeroes (n - 1))


(* converte una lista di bool w1 in una stringa, dove true è codificato con quadrato mentre falso con blank*)
let string_of_world1 w1 = (*  bool list -> string  *)
  List.fold_left 
  (fun s x -> s ^ (if x then "█" else " ")) "" w1
  (* funzione accumulatrice, accumulatore iniziale, lista su cui iterare*)
  (* fun è una funzione anonima che prende 2 parametri: s (il valore accumulato sino ad ora) e x (l'elemento corrente, un bool)*)
  (* s sfrutta ^ per concatenare, dopo vi è una espressione condizionale che cambia il carattere*)
  (* string_of_world1 [true;true;false] = ;; - : string = "██ "                   *)

(* converte una matrice (lista di lista) di bool w in una stringa, basandosi su string_of_world1*)
let string_of_world w = (* bool list list -> string *)
  List.fold_left 
  (fun s x -> s ^ "\n" ^ string_of_world1 x) "" w
  (* è identica alla scorsa ma opera con liste di liste *)
  (* w è una lista di liste, accoda un terminatore di stringa per ogni stringa letta*)
  (* si può riscrivere dando un nome alla funzione anonima fun (lascio l'implem. originale) *)




(* Crea una lista di bool di lunghezza n dove ogni elemento ha una probabilità p di essere true*)
(* p in [0,100] is the probability of 1 *)
let rec rnd_world1 p n = (* (int -> int) -> bool list *)
  match n with
  | 0 -> []         (* senza elementi la lista è vuota *)
  | _ -> (Random.int 100 < p) :: rnd_world1 p (n - 1) (* per n>1 genera un elemento casuale
    in questa posizione (che ha p probabilità di essere true) e poi procede ricorsivamente a creare
    gli altri elementi della lista *)

let rec rnd_world p m = function
    0 -> []
  | n -> (rnd_world1 p m) :: (rnd_world p m (n-1))


let init_w = rnd_world 20 30 60

let neighbours w i j = 
  let n = List.length w in
  let l0 = List.nth w ((i+n-1) mod n) 
  and l1 = List.nth w (i mod n) 
  and l2 = List.nth w ((i+1) mod n) 
  in 
  let m = List.length l0
  in  
  assert (List.length l1 = m && List.length l2 = m);
  (
    List.nth l1 (j mod m),
    [
      [List.nth l0 ((j+m-1) mod m); List.nth l0 (j mod m); List.nth l0 ((j+1) mod m)]; 
      [List.nth l1 ((j+m-1) mod m); false; List.nth l1 ((j+1) mod m)]; 
      [List.nth l2 ((j+m-1) mod m); List.nth l2 (j mod m); List.nth l2 ((j+1) mod m)]
    ])

let count1 l = List.fold_left (fun s x -> s + (if x then 1 else 0)) 0 l

let count w = List.fold_left (fun s x -> s + count1 x) 0 w

let alive w i j =
  let (cell,nb) = neighbours w i j in
  let alive_nb = count nb in

  if cell then (* cell is alive *)
    (* cell survives? *)
    alive_nb = 2 || alive_nb = 3
  else (* cell is dead *)
    (* cell is born? *)
    alive_nb = 3


(* step1 prende una matrice e un indice i*)
let step1 w i =
  let n = List.length w in      (* n è il numero di *)
  List.mapi (fun j _ -> alive w i j) (zeroes n)

let step w =
  let n = List.length w in
  List.mapi (fun i _ -> step1 w i) (zeroes n)

(* let step w = List.map step1 w *)
(* let step w = w *)

(* display prende come parametro una matrice w e utilizza le funzioni dell'ANSI T
   per mostrare a schermo il nuovo aspetto del terminale *)
let display w =
  T.erase T.Screen;         (* Cancella  il contenuto del terminale*)
  T.set_cursor 1 1;         (* Imposta il cursore in basso a sx *)
  (* T.print_string [] (string_of_world w); *)
  printf "%s\n%!" (string_of_world w);  (* Stampa il carattere indicato da string_of_world*)
  Unix.sleepf 5.0;;         (* Float sleep in secondi *)


(* loop prende la matrice  w e il numero di iterazioni n *)
let rec loop w n =
  if n=0 then (display w; w) (* 0 è il numero di ripetizioni rimaste*)
  else (display w; loop (step w) (n-1))
;;


