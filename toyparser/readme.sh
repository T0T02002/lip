# toyparser (fisso)

# inizializza
cd lip
dune init project toyparser
cd toyparser
echo '(using menhir 2.1)' >> dune-project
echo -e '(menhir (modules parser))\n(ocamllex lexer)' >> lib/dune

# per eseguirlo
cd toyparser
dune exec toyparser

# per eseguire test
cd toyparser
dune test

# come per ogni cartella di progetto, eseguire dune build rimuoverà gli errori di comp su vscode
