file<-read.csv("DossierdedonnéesBigDataNantes.csv") 



summary(file)
str(file)
head(file)
summary(vessel.total.clean)
str(vessel.total.clean)
head(vessel.total.clean)

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
df <- file
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

# 5. Identifier les lignes avec valeurs manquantes
missing_summary <- df |> summarise(across(everything(), ~sum(is.na(.))))

# (optionnel) Supprimer les lignes trop incomplètes
df <- df |> drop_na(lat, lon, base_date_time, mmsi)

# 6. Supprimer les valeurs aberrantes (exemples simples)
df <- df |> 
  filter(
    lat >= 20 & lat <= 30,
    lon >= -98 & lon <= -78,
  )

# 7. Exporter la base propre
write_csv(df, "vessel-total-clean-final.csv")


