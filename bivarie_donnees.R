install.packages("vcd")
install.packages("grid")
df <- read.csv("vessel-total-clean-final.csv", stringsAsFactors = FALSE)

# Conversion en facteurs pour les variables qualitatives
df$vessel_type <- as.factor(df$vessel_type)
df$cargo <- as.factor(df$cargo)
df$transceiver_class <- as.factor(df$transceiver_class)

# Analyse bivariée 1 : boîte à moustaches de la vitesse (sog) par type de bateau
boxplot(sog ~ vessel_type, data = df,
        main = "Vitesse des bateaux par type",
        xlab = "Type de bateau",
        ylab = "Vitesse (sog)",
        col = "lightblue")

# Analyse bivariée 2 : corrélation entre longueur et largeur
plot(df$length, df$width,
     main = "Corrélation entre longueur et largeur des navires",
     xlab = "Longueur (m)",
     ylab = "Largeur (m)",
     pch = 19, col = rgb(0.2, 0.4, 0.6, 0.6))
abline(lm(width ~ length, data = df), col = "red")

# Analyse bivariée 3 : tableau croisé et test du chi2 entre vessel_type et transceiver_class
table_vt_tc <- table(df$vessel_type, df$transceiver_class)
print(table_vt_tc)
chisq.test(table_vt_tc)

# Visualisation du tableau croisé
library(vcd)  # pour le mosaicplot
mosaicplot(table_vt_tc, main = "Répartition Type de Bateau vs Classe Transceiver", color = TRUE)

