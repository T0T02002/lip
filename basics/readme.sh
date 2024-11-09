# adder (test passano sia con portatile che con fisso)

# inizializza progetto
cd lip/basics
dune init project adder

# test
cd basics # ogni tanto qua ogni tanto la
dune utop lib

# in utop
open Adder;;
addlist [1;2;3];;

# CTRL+Z per uscire da utop
dune test

# elimina le righe inutili da adder/test/dune

# per funzionare deve esistere un solo _build e un solo /adder
# in adder scrivi
dune test

#################################################

# recognizer (funziona con fisso)

# inizializza progetto
cd lip/basics
dune init project recognizer

# test con utop
dune utop lib
open Recognizer;;
lang1 [];; # o altri test

# per testare la parola intera (e sapere a quali linguaggi appartiene)
cd recognizer
dune exec recognizer

# alcuni errori dell'ide (VScode) spariscono se commentiamo
# o aggiungiamo tab in alto nel documento

# per creare un dune test
# vai nel file test_recognizer, apri il progetto con Open Recognizer
# incolla i test come in adder
# nel file 'dune' della stessa cartella incolla

(library
 (name test_recognizer) # nome file .ml
 (inline_tests) 
 (preprocess (pps ppx_inline_test))
 (libraries recognizer)) # libreria 

 # esegui in /recognizer con
dune test

######################################################

