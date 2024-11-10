# contextfree (testato da fisso senza docker)

# Inizializza il progetto con
cd lip/contextfree
dune init project contextfree

# accertati che non ci sia una cartella doppia di nome contextfree, 
# in caso elimina la più interna
cd lip/contextfree
dune build

# testa con
cd lip/contextfree
dune test

# funziona anche la generazione di parole di dalps con
dune exec contextfree