file<-read.csv("DossierdedonnéesBigDataNantes.csv") 


summary(file)
str(file)
head(file)


install.packages("nnet")



install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("janitor")


library(nnet)
library(corrplot)

library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readr)

# Charge les données

df <- file
# Liste des valeurs à convertir en NA
valeurs_na <- c("", "NA", "\\N", "na", "n")

# Applique le remplacement sur toutes les colonnes de type caractère ou facteur
df <- as.data.frame(lapply(df, function(col) {
  if (is.character(col) || is.factor(col)) {
    col[col %in% valeurs_na] <- NA
  }
  return(col)
}))

# Nettoie les noms de colonnes
df <- df |> clean_names()

# 3. Convertit les colonnes numériques (length, width, draft) de caractères → num
df <- df |>
  mutate(
    length = as.numeric(length),
    width = as.numeric(width),
    draft = as.numeric(draft)
  )

# Supprime les doublons (identiques sur toutes les colonnes)
df <- df |> distinct()



# Supprime les valeurs aberrantes (exemples simples)
df <- df |> 
  filter(
    lat >= 20 & lat <= 30,
    lon >= -98 & lon <= -78,
    sog <= 30,
    )

# Met l'angle à 0 si la vitesse est nulle
df$cog[df$sog == 0] <- 0

# Met l'angle à NA s'il est supérieur à 360
df$cog[df$cog >= 360] <- NA
df$heading[df$heading >= 360] <- NA

df$cargo[df$vessel_type == 60 & (df$cargo == 0 | df$cargo == 99)] <- NA

df <- subset(df, !(length <= 10 | width <= 3 | draft <= 0.5) | is.na(draft) | is.na(length) | is.na(width))

# Remplace les valeurs Na par la moyenne correspondante par type 
mean_drafts <- aggregate(draft ~ vessel_type, data = df, FUN = function(x) mean(x, na.rm = TRUE))
df$draft <- ifelse(is.na(df$draft), 
                   mean_drafts$draft[match(df$vessel_type, mean_drafts$vessel_type)], 
                   df$draft)

mean_length <- aggregate(length ~ vessel_type, data = df, FUN = function(x) mean(x, na.rm = TRUE))
df$length <- ifelse(is.na(df$length), 
                   mean_length$length[match(df$vessel_type, mean_length$vessel_type)], 
                   df$length)

mean_width <- aggregate(width ~ vessel_type, data = df, FUN = function(x) mean(x, na.rm = TRUE))
df$width <- ifelse(is.na(df$width), 
                   mean_width$width[match(df$vessel_type, mean_width$vessel_type)], 
                   df$width)

# Identifie les lignes avec valeurs manquantes
missing_summary <- df |> summarise(across(everything(), ~sum(is.na(.))))

# Exporte la base propre
write_csv(df, "vessel-total-clean-final.csv")

sum <- summary(vessel.total.clean)
summary_df <- data.frame(valeur = sum)
write_csv(summary_df, "summary.csv")





