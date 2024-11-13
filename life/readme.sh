# life

# installa l'ansiterminal 
cd lip
opam install ANSITerminal

# in lip
dune init project life

cd life
sed -i '2i   (libraries ANSITerminal)' lib/dune



git remote -v #repository che sta ascoltando, nella clone, appare link 
git remote add unica https://github.com/ (link di lip)
git pull unica main
# se appaiono conflitti
git pull unica --no-rebase
# cerca file con punto esclamativo (conflitti da risolvere) (in generale a dx)
# da interfaccia, commit e sync changes