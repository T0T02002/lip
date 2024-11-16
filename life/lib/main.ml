(* by nicololuca *)

module T = ANSITerminal (* import ANSITERMINAL as T *)
open Printf             (* consente formattazioni per la stampa a schermo *)


(* Prende una stringa s la trasforma in una regola Rule.rule *)
let parse (s : string) : Rule.rule =
  (* crea un buffer di token *)
  let lexbuf = Lexing.from_string s in   
  (* restituisce rule, ossia la stringa interpretata col parser e con il lexer *)           
  let rule = Parser.prog Lexer.read_token lexbuf in
  rule


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

(* Genera una lista di n liste di lunghezza m con p probabilità di essere true *)
let rec rnd_world p m = function
    0 -> []
  | n -> (rnd_world1 p m) :: (rnd_world p m (n-1))


(* Genera una matrice 60x30 con la probabilità p=20% *)  
let init_w = rnd_world 20 30 60


(* Prende: una lista di liste (w), l'indice della riga (i) e l'indice della colonna (j) *)
let neighbours w i j = 
  (* n = numero righe, ossia la lunghezza di w *)
  let n = List.length w in
  (* l0 è la riga precedente a i (con coda circolare grazie a mod) *)
  let l0 = List.nth w ((i+n-1) mod n) 
  (* l1 è la riga corrente *)
  and l1 = List.nth w (i mod n) 
  (* l2 è la riga successiva a i*)
  and l2 = List.nth w ((i+1) mod n)  in 
  (* assegna ad m il numero di colonne della matrice, ossia il numero di liste dentro una lista *)
  let m = List.length l0 in  
  (* assert assicura che l1 e l2 abbiano la stessa lunghezza di m (quindi di l0); fallisce altrimenti *)
  assert (List.length l1 = m && List.length l2 = m);
  (
    List.nth l1 (j mod m), (* restituisce il valore dell'elemento nella riga corrente l1 di colonna j *)
    [
      (* sottolista 3x3 delle celle vicine: calcola per ogni cella adiacente che valore booleano settare *)
      [List.nth l0 ((j+m-1) mod m); List.nth l0 (j mod m); List.nth l0 ((j+1) mod m)]; 
      [List.nth l1 ((j+m-1) mod m); false; List.nth l1 ((j+1) mod m)]; 
      [List.nth l2 ((j+m-1) mod m); List.nth l2 (j mod m); List.nth l2 ((j+1) mod m)]
    ])


(* conta il numero di true in una lista di bool *)
let count1 l =  (* s accumulatore, x elemento della lista *)
  List.fold_left (fun s x -> s + (if x then 1 else 0)) 0 l

(* conta il numero di true in una lista di liste di bool *)
let count w = 
  List.fold_left (fun s x -> s + count1 x) 0 w


(* Alive accetta: una lista di liste (w), l'indice di riga (i), l'indice di colonna (j), 
   un valore di rule, ossia una coppia (int list, int list) *)
let alive w i j (rule: Rule.rule) =
  (* chiama neighbours e la applica. Il risultato è una coppia (cell,nb) 
     cell indica se la cella (1,j) è true o false; neigh_matrix è una matrice 3x3 che indica gli stati dei vicini *)
  let (current_cell,neigh_matrix) = neighbours w i j in
  (* alive_matrix_neigh conta quanti vicini di (i,j) sono a true *)
  let alive_matrix_neigh = count neigh_matrix in
  (* pattern matching su rule *)

  let rule_match = match rule with
    Rule (s,[0]) -> (s,[]) (* se la lista ha solo false, viene trattata come vuota *)
  | Rule (s,b) -> (s,b)    (* in tutti gli altri casi, restituisce la lista così com'è *)
  | _ -> failwith "Error rule" in (* eccezione se Rule non funziona *)
  (* Scompone rule per fornire le regole della partita *)
  let (s,b) = rule_match in (* s=lista di vicini vivi per mantenere (i,j) in vita; b=num di vicini vivi per far nascere una nuova cella *)
  
  (* se la cella corrente è true (viva), e il numero di vicini vivi appare nella lista s, allora la cella rimane viva *)
  if current_cell then 
    List.exists (fun x -> x=alive_matrix_neigh) s
  (* se la cella corrente è false (morta), e il numero di vicini vivi appare nella lista b, allora la cella rinasce *)
  else  
    List.exists (fun x -> x=alive_matrix_neigh) b

(* ESEMPIO 
  let w = [
    [false; true; false];
    [true; true; false];
    [false; false; false]
  ]

  let rule = Rule ([2;3], [3])   
  
  let result = alive w 1 1 rule

  La cella centrale (1,1) è viva e ha 2 vicini vivi. 
  Secondo la regola, con 2 o 3 vicini rimane viva.
  Quindi result sarà 'true' perché 2 appare nella lista s, ossia [2;3]. 
 *)


(* aggiorna lo stato di tutte le celle di una riga i in base alla regola di alive *)
let step1 w i rule =
  let n = List.length w in      (* n è il numero di *)
  List.mapi (fun j _ -> alive w i j rule) (zeroes n)
(* aggiorna l'intera matrice w *)
let step w rule =
  let n = List.length w in
  List.mapi (fun i _ -> step1 w i rule) (zeroes n)
(* let step w = List.map step1 w *)
(* let step w = w *)


(* display prende come parametro una matrice w e utilizza le funzioni dell'ANSI T
   per mostrare a schermo il nuovo aspetto del terminale *)
let display w =
  T.erase T.Screen;   (* Cancella  il contenuto del terminale*)
  T.set_cursor 1 1;   (* Imposta il cursore in basso a sx *)
  (* T.print_string [] (string_of_world w); *)
  printf "%s\n%!" (string_of_world w);  (* Stampa il carattere indicato da string_of_world*)
  Unix.sleepf 2.5;;   (* Float sleep in secondi *)


(* esegue un ciclo su w ogni n volte secondo la regola rule *)
let rec loop w n rule =
  if n=0 then (display w; w) (* 0 è il numero di ripetizioni rimaste*)
  else (display w; loop (step w rule) (n-1) rule)
;;


