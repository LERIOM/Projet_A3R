# Crée les histogrammes
hist(df$sog,
     main = "Histogramme des vitesses des bateaux",
     xlab = "Vitesse (sog)",
     col = "skyblue",
     border = "white",
     breaks = 50)
hist(df$vessel_type,
     main = "Graphique des bateaux en fonction de leurs types",
     xlab = "Type des bateaux (vessel_type)",
     col = "skyblue",
     border = 'white',
     breaks = 50)

