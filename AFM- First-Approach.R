set.seed(123)

data <- data.frame(
  product_quality = sample(1:10, 200, replace = TRUE),
  product_durability = sample(1:10, 200, replace = TRUE),
  preference_taste = sample(1:10, 200, replace = TRUE),
  preference_price = sample(1:10, 200, replace = TRUE),
  age = sample(18:70, 200, replace = TRUE),
  income = sample(10000:150000, 200, replace = TRUE),
  gender = as.factor(sample(c("Male", "Female"), 200, replace = TRUE)),
  education_level = as.factor(sample(c("High School", "Bachelor’s", "Master’s", "PhD"), 200, replace = TRUE))
)

data


mfa_result <- MFA(data, 
                  group = c(2, 2, 2, 1, 1),
                  type = c("c", "c", "c" ,"n", "n"),
                  name.group = c("Perception", "Preferences", "Client Caracteristics", "Gender", "Education Level"),
                  num.group.sup = c(4, 5),
                  graph = FALSE)

# Resumen del análisis MFA
summary(mfa_result)


# Graficar las coordenadas factoriales de las variables
fviz_mfa_var(mfa_result, 
             geom.var = "arrow", 
             col.var = "red", 
             title = "Coordenadas Factoriales de las Variables")

# Varianza explicada por dimensión
variance_explained <- mfa_result$eig[, 2]
variance_explained_pct <- round(variance_explained * 100, 2)
print(variance_explained_pct)

# Gráfico de la varianza explicada
barplot(variance_explained_pct, 
        main = "Varianza Explicada por Dimensión",
        xlab = "Dimensiones", ylab = "Varianza (%)", 
        col = "lightblue")

