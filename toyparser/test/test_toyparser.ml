open Toyparser.Main
 
let%test _ = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9
let%test _ = parse "1 + 2 + 3 + 1 + 2" |> eval = Ok 9
let%test _ = parse "5 * 2 + 2" |> eval = Ok 12
let%test _ = parse "(5 * 2) + 2" |> eval = Ok 12
let%test _ = parse "5 * (2 + 2)" |> eval = Ok 20
let%test _ = parse "2 + 5 * 2" |> eval = Ok 12
let%test _ = parse "5 * 6 / 1" |> eval = Ok 30
let%test _ = parse "5 / 1 * 6" |> eval = Ok 30
let%test _ = parse "-1 - 2 - -3" |> eval = Ok 0
let%test _ = parse "0x01 + 2" |> eval = Ok 3
let%test _ = parse "0x01 + 0X01" |> eval = Ok 2
let%test _ = parse "0xA + 0" |> eval = Ok 10

(* Test di conferma *)
let%test_unit "all tests passed" =
  print_endline "Tutti i test sono passati con successo!"