# toylexer

# crea il progetto con
cd lip
dune init project toylexer
cd toylexer
echo '(ocamllex lexer)' >> lib/dune

# per testare frequency
dune exec toylexer freq n   # con n un numero intero a piacere

# cancella ogni riga superflua da test/dune
dune test

