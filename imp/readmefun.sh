
# fun

cd lib/imp
make fun

# output:
dune init proj fun ; \
        cd fun ; \
        sed -i 's/name fun/name funLib/' lib/dune ; \
        sed -i 's/libraries fun/libraries funLib/' bin/dune ; \
        echo '(using menhir 2.1)' >> dune-project ; \
        echo '\n(menhir (modules parser))\n\n(ocamllex lexer)' >> lib/dune ; \
        rm test/test_fun.ml ; \
        echo "\
(library\n\
 (name funTest)\n\
 (inline_tests)\n\
 (preprocess (pps ppx_inline_test))\n\
 (libraries funLib))" > test/dune
Entering directory '/home/t0t02002/lip/imp/fun'
Warning: File bin/main.ml was not created because it already exists
Success: initialized project component named fun


# build and run test in real time:
cd fun
dune test --watch


# aprire dune utop:
cd imp/fun
dune utop

open Fun;;
open FunLib;; 
open FunLib.Ast;; 
open FunLib.Types;; 
open FunLib.Lexer;; 
open FunLib.Parser;; 
open FunLib.Prettyprint;;
open FunLib.Main;;  

parse "int x; x:=51";;