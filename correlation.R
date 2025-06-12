#matrice de corélation 
df_num <- df |>
  select(where(is.numeric)) |>
  drop_na()  # Optionnel : supprime les lignes avec NA

# Calculer la matrice de corrélation
cor_matrix <- cor(df_num, use = "complete.obs")

# Afficher la matrice
print(cor_matrix)

# Visualisation avec corrplot
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")
df <- df|>  
  drop_na(vessel_type, width, length, draft, cargo)
