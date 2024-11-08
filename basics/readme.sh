# adder

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

