df <- df|>
  drop_na(sog, width, length, draft)
# Créer un échantillon d'entraînement (80%) et test (20%)
set.seed(123)  # pour reproductibilité
index <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_data <- df[index, ]
test_data <- df[-index, ]

# Entraînement du modèle multinomial
model <- multinom(vessel_type ~ width + length + draft, data = train_data)

# Prédiction sur le jeu de test
test_data$predicted <- predict(model, newdata = test_data)

# Évaluer la précision
accuracy <- mean(test_data$predicted == test_data$vessel_type)
cat("Taux de bonne classification (test) :", round(accuracy * 100, 2), "%\n")

# Matrice de confusion
print(table(Prédit = test_data$predicted, Réel = test_data$vessel_type))

