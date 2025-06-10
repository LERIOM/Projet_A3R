file<-read.csv("DossierdedonnéesBigDataNantes.csv") 



 


summary(file)
str(file)
head(file)


install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("janitor")

library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readr)

# 1. Charger les données
df <- vessel.total.clean
# Liste des valeurs à convertir en NA
valeurs_na <- c("", "NA", "\\N", "na", "n")

# Appliquer le remplacement sur toutes les colonnes de type caractère ou facteur
df <- as.data.frame(lapply(df, function(col) {
  if (is.character(col) || is.factor(col)) {
    col[col %in% valeurs_na] <- NA
  }
  return(col)
}))

# 2. Nettoyer les noms de colonnes
df <- df |> clean_names()

# 3. Convertir les colonnes numériques (length, width, draft) de caractères → num
df <- df |>
  mutate(
    length = as.numeric(length),
    width = as.numeric(width),
    draft = as.numeric(draft)
  )

# 4. Supprimer les doublons (identiques sur toutes les colonnes)
df <- df |> distinct()




# (optionnel) Supprimer les lignes trop incomplètes
df <- df |> drop_na(lat, lon, base_date_time, mmsi)

# 6. Supprimer les valeurs aberrantes (exemples simples)
df <- df |> 
  filter(
    lat >= 20 & lat <= 30,
    lon >= -98 & lon <= -78,
    sog <= 30,
    cog <= 360.0,
    )
df <- subset(df, !(length == 0 | width == 0) | is.na(length) | is.na(width))

# 5. Identifier les lignes avec valeurs manquantes
missing_summary <- df |> summarise(across(everything(), ~sum(is.na(.))))

# 7. Exporter la base propre
write_csv(df, "vessel-total-clean-final.csv")

sum <- summary(vessel.total.clean)
summary_df <- data.frame(valeur = sum)
write_csv(summary_df, "summary.csv")

