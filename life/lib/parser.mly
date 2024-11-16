// copyright -> nicololuca

%{
open Rule     (* import tipo rule*)
%}

%token E   // prefisso regole estese
%token S   // survive
%token B   // born
%token <string> SEQ         // sequenza di numeri        "123"
%token <string> RANGE_SEQ   // sequenza di intervalli    "0..5,7"
%token SLASH
%token EOF

%start <rule> prog

%%

// parsing dell'input con expr, restituisce e
prog:
  | e = expr; EOF { e }
;

// 
expr:
  // "S/B" di formato Rule(e1,e2) dove e1 e e2 sono due liste
  | S; e1 = sequence; SLASH; B; e2 = sequence { Rule(e1,e2) }
  // "ES/B" di formato Rule(e1,e2) dove e1 e e2 sono due liste
  | E; S; e1 = ext_sequence; SLASH; B; e2 = ext_sequence { Rule(e1,e2) }
  // "E" di formato Rule(e1,e2) dove e1 e e2 sono due liste
  | E; e1 = ext_sequence; SLASH; e2 = ext_sequence { Rule(e1,e2) }
;

// Accetta un token SEQ con una stringa di cifre, converte ogni carattere in int
sequence:
  | seq = SEQ { List.map 
  (* Converte la stringa in una sequenza, poi in una lista e poi trasforma ogni char in int con sottraendo l'ASCII *)
  (fun x -> (int_of_char x) - 48) (seq |> String.to_seq |> List.of_seq) }
;

// Disingue la sequenza separata da virgole (SEQ) dalla sequenza di intervallo (RANGE_SEQ)
ext_sequence:
  // Divide la stringa alla primma occorrenza di ',' e converte ogni elemento in int
  | seq = SEQ { (seq |> String.split_on_char ',' 
    |> List.map (fun x -> int_of_string x)) }
  // Divide la stringa appena trova ',' e poi ulteriormente quando trova '.' per gestire gli intervalli
  // converte poi i sotto-elementi in int
  | seq = RANGE_SEQ { 
      seq 
      |> String.split_on_char ',' 
      |> List.map (fun x -> String.split_on_char '.' x)
      |> List.map (fun x -> List.filter (fun y -> not(y="")) x)
      |> List.map (fun x -> List.map (fun y -> int_of_string y) x)
      |> List.map (fun x -> match x with
          [] -> [0]
        | [a;b] -> List.init (b-a+1) (fun x -> a+x) (* genera i numeri nell'intervallo *)
        | _ -> failwith "Wrong format"
      )
      |> List.flatten (* unisce tutte le liste in una *)
  }
;