# life

# installa l'ansiterminal 
cd lip
opam install ANSITerminal

# in lip
dune init project life

cd life
sed -i '2i   (libraries ANSITerminal)' lib/dune

