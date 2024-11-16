module T = ANSITerminal (* Modulo ANSITerminal rinominato T *)
open Life.Main          (* Apertura modulo Life.Main (in modo da non dover precedere le funzioni con Life.Main.qualcosa)*)

(* Funzione main; conta il numero di parametri di argv, ossia i parametri passati a riga di comando*)
let _ = match Array.length(Sys.argv) with
  (* si aspetta 3 parametri, quindi si indica che fare nel caso vengano trovati*)
  3 -> 
    (* L'argomento (0) è il nome del programma e non viene conteggiato.
       Assegna l'argomento di indice (1) a rule *)
    let rule = Sys.argv.(1) in 
    (* Converte l'argomento di indice (2) a int e lo assegna a k *)
    let k = int_of_string (Sys.argv.(2)) in
    (* Cancella lo schermo *)
    T.erase T.Screen;
    (* Salva la posizione attuale del cursore per poterla ripristinare a fine esecuzione *)
    T.save_cursor();
    (* Inizializza rand con un seed *)
    Random.self_init();
    (* Chiama parse per interpretare la stringa rule e convertirla in ??? *)
    let parsed_rule = parse rule in
    (* Invia a loop una condizione iniziale (init_w), un numero di iterazioni e una regola. Assegna il risultato a world *)
    let world = loop init_w k parsed_rule in
    (* Mostra la configurazione a schermo *)
    display world;
    (* ignore(read_line());  (permetteva di premere invio per chiudere) *)
    T.restore_cursor();      (* riposiziona il cursore *)
    print_newline();         (* aggiunge una riga (per ordinare) *)
    T.erase T.Screen;        (* pulisce *) 

  (* se non sono 3 i parametri allora fallisce *)
  | _ -> failwith "Usage: dune exec life n_rounds"
