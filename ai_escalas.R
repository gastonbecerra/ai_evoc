# Cargar las librerías necesarias
library(tidyverse)
library(psych)

# Cargar los datos
data <- read_csv('data/ai_evoc.csv')

# Filtrar las columnas de interés para las escalas de uso y personalidad
uso_columns <- data %>% select(starts_with("uso_"))
minipip_columns <- data %>% select(starts_with("minipip_"))

# Calcular el Alfa de Cronbach para las escalas
uso_alpha <- psych::alpha(uso_columns)$total$raw_alpha
minipip_alpha <- psych::alpha(minipip_columns)$total$raw_alpha

# Mostrar los resultados
uso_alpha
minipip_alpha



library(corrplot)

uso_corr <- cor(uso_columns, use = "complete.obs")
minipip_corr <- cor(minipip_columns, use = "complete.obs")

# Visualizar las correlaciones con mapas de calor
# Escalas de uso
corrplot(uso_corr, method = "color", tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.7, title = "Correlaciones de Escalas de Uso")

# Escalas MINIPIP
corrplot(minipip_corr, method = "color", tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.7, title = "Correlaciones de Escalas MINIPIP")

