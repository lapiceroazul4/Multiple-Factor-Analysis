install.packages("readxl")
install.packages("FactoMineR")
install.packages("factoextra")

library(readxl)
library(FactoMineR)
library(factoextra)
library(ggplot2)

set.seed(123)

# Usamos los datos generados anteriormente
data <- data.frame(
  product_quality = sample(1:10, 200, replace = TRUE),
  product_durability = sample(1:10, 200, replace = TRUE),
  preference_taste = sample(1:10, 200, replace = TRUE),
  preference_price = sample(1:10, 200, replace = TRUE),
  age = sample(18:70, 200, replace = TRUE),
  income = sample(10000:150000, 200, replace = TRUE),
  gender = sample(c("Male", "Female"), 200, replace = TRUE),
  education_level = sample(c("High School", "Bachelor’s", "Master’s", "PhD"), 200, replace = TRUE)
)

### data <- read_excel("df_consumer_preferences.xlsx")

# Estandarización de variables cuantitativas
X_num <- as.matrix(data[, c("product_quality", "product_durability", "preference_taste", "preference_price")])
X_num_centered <- scale(X_num, center = TRUE, scale = TRUE)


## NOTE!!! 
# Pilas aqui, al parecer si hacemos el proceso a pie necesitamos volver las variables dummies
# Cuando lo hacemos haciendo uso de la libreria, basta con tener las variables como factores
# Cuando convertimos a factores la cantidad de dummies sería los niveles del factor

# Convertir a variables dummy las cualitativas
#X_gender <- model.matrix(~ gender - 1, data = data)
#X_education <- model.matrix(~ education_level - 1, data = data)

#ncol(X_gender)
#ncol(X_education)
#ncol(X_num_centered)

#group_sizes <- c(ncol(X_num_centered), ncol(X_gender) + ncol(X_education)) 

# Unir las dos matrices en una sola
#X_cat <- cbind(X_gender, X_education)
#X_cat_centered <- scale(X_cat, center = TRUE, scale = FALSE)
#X_total <- cbind(X_num_centered, X_cat_centered)

data$gender <- as.factor(data$gender)
data$education_level <- as.factor(data$education_level)

ncol(X_gender)
levels(data$gender) 
levels(data$education_level)

group_sizes <- c(ncol(X_num_centered), 1, length(levels(data$gender)), length(levels(data$education_level)))
group_sizes


# Realizar el análisis factorial múltiple (MFA)
mfa_result <- MFA(data, 
                  group = c(6,2), # Número de variables en cada bloque (cuantitativas y cualitativas)
                  type = c("n", "c"), # Tipo de variables (cuantitativas y cualitativas)
                  graph = FALSE)

# Resumen del análisis MFA
summary(mfa_result)

# Graficar las coordenadas factoriales de los individuos
fviz_mfa_ind(mfa_result, 
             geom.ind = "point", 
             col.ind = "blue", 
             addEllipses = TRUE, 
             title = "Coordenadas Factoriales de los Individuos")

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
