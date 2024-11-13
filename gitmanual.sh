# non sono ancora riuscito a utilizzare docker in entrambi i dispositivi
# aggiungi qui quando saprai spiegare il modo per farlo


# nella cartella padre, serve per raggruppare i file modificati
git add .

# serve per dare un commento alle task svolte
git commit -m "commento"

# serve per sincronizzare dalla repo locale a quella remota
git push


# per conflitti
git remote -v #repository che sta ascoltando, nella clone, appare link 
git remote add unica https://github.com/ (link di lip)
git pull unica main
# se appaiono conflitti
git pull unica --no-rebase
# cerca file con punto esclamativo (conflitti da risolvere) (in generale a dx)
# da interfaccia, commit e sync changes