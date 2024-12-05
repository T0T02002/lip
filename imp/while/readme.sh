# while

cd imp
make while

# per testare dune utop
cd imp/while
dune utop
open WhileLib.Ast;;open WhileLib.Types;;open WhileLib.Prettyprint;;open WhileLib.Main;;open WhileLib__Main;;
open WhileLib__Main;;   
open WhileLib;;

# capisci che hai aperto il modulo corretto quando con tab vedi la funzione che ti serve
# per testare parse (ide -> cmd)
parse "x:=0" ;;

# riesco a testare trace o trace1 per il momento

