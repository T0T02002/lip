# life (TO TEST)

# installa l'ansiterminal 
cd lip
opam install ANSITerminal

# in lip
dune init project life

cd life
sed -i '2i   (libraries ANSITerminal)' lib/dune

# per eseguire il progetto
dune exec life <n>   # con n il numero di turni

cd life
echo '(using menhir 2.1)' >> dune-project
echo -e '(menhir (modules parser))\n(ocamllex lexer)' >> lib/dune

# per eseguirlo con le regole
dune exec life E2,3/3 <n>

# Il progetto è interamente da testare con i casi della Task3
# parser e main by nicololuca
