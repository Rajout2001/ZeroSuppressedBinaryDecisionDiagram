reset 
set terminal png
set output "img/distributionTaillesZDD.png"
set xrange [0:256]
set yrange [0:0.1]
set ylabel "probabilité"
set xlabel "taille (2*n bits)'"
plot "distribution.txt" title "DistributionTaillesZDD"

reset 
set terminal png
set output "img/ComparaisonVitesseExe.png"
set xlabel "Taille"
set ylabel "Temps (en seconde)"
plot "comparaison_vitesse.txt" using 1:2 with lines title "compressionParArbre", \
     "comparaison_vitesse.txt" using 1:3 with lines title "compressionParListe"

reset 
set terminal png
set output "img/TailleMoy.png"
set xlabel "Taille"
set ylabel "Taille compressé (en noeud)"
plot "taille_moy.txt" using 1:2 with lines title "compressionParArbre", \
     "taille_moy.txt" using 1:3 with lines title "compressionParListe"

