# sei in /lip
cd toylexer
dune build 
dune utop
# frequency 3 [ID("x"); ASSIGN; ID("y"); SEQ; ID("x"); ASSIGN; ID("x"); PLUS; CONST("1")];;
# CTRL + Z       per uscire da utop

# per quanto riguarda dune exec
cd toylexer
dune exec toylexer freq <n>  #n param
#x+x=y           


#per l'esercizio 2