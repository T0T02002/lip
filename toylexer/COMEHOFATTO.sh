# sei in /lip
cd toylexer
dune build 
dune utop
# frequency 3 [ID("x"); ASSIGN; ID("y"); SEQ; ID("x"); ASSIGN; ID("x"); PLUS; CONST("1")];;