# boolexpr

cd expr
dune init proj boolexpr
# dà un problema 
#File "lib/dune", lines 4-5, characters 0-26:
#4 | (menhir
#5 |  (modules parser))
#Error: 'menhir' is available only when menhir is enabled in the dune-project
#file. You must enable it using (using menhir 3.0) in your dune-project file.

dune build # se compila hai risolto la task 1

# per testare eval e trace
dune utop lib
open BoolexprLib.Main;;
open BoolexprLib.Ast;;


#########################################################

# andboolexpr

cd expr

# NON ESISTE IL FILE dune-project, copia da quello di boolexpr
touch dune-project
# copia incolla
cd andboolexpr
dune init proj andboolexpr
# OUTPUT DESIDERATO:
#       Entering directory '/home/t0t02002/lip/expr/expr'
#       Success: initialized project component named andbool

dune build

# capire perché:
#       Warning: one state has shift/reduce conflicts.
#       Warning: 2 shift/reduce conflicts were arbitrarily resolved.

# nel dubbio ho creato un .ocamlinit file con open
cd lib
touch .ocamlinit
# copia incolla:
# open AndboolexprLib.Ast
# open AndboolexprLib.Main 

# per far funzionare dune test ho cancellato l'open di Ast da test/andboolexpr.ml
dune test

# non ho testato dune lib utop

####################################

# arithexpr

cd expr
make arithexpr
# output:
#dune init proj arithexpr ; \
#        cd arithexpr ; \
#        echo '(using menhir 2.1)' >> dune-project ; \
#        echo '\n(menhir (modules parser))\n\n(ocamllex lexer)' >> lib/dune ; \
#        rm test/test_arithexpr.ml ; \
#        echo "\
#(library\n\
# (name arithexprTest)\n\
# (inline_tests)\n\
# (preprocess (pps ppx_inline_test))\n\
# (libraries arithexpr))" > test/dune
#Entering directory '/home/toto/lip/expr/arithexpr'
#Warning: File bin/main.ml was not created because it already exists
#Success: initialized project component named arithexpr

dune test
# errore in test 
# 1 | open ArithexprLib.Ast
#         ^^^^^^^^^^^^^^^^
#Error: Unbound module ArithexprLib
#Hint: Did you mean Arithexpr?

# apri test/dune
# cambia la riga 5 in  (libraries arithexprLib)) 
# apri lib/dune
# cambia la riga 2 in  (name arithexprLib))
# (questo è perché il make non funziona)

# cambia e commenta open ArithexprLib.Ast  e open ArithexprLib.Main a cazzo di cane finché non va

# Il test smallstep6 mi ha dato parecchi problemi, ha funzionato solo cambiato la logica dell'or e dell'and in parser