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
