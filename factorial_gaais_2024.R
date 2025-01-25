# FACTORIAL DE GAAIS
# Y PALABRAS MAS FRECUENTES DE LOS DE ACTITUD POSITIVA Y NEGATIVA

library(tidyverse)

evoc <- read.csv(file = 'data/evoc.csv')
data2021 <- read.csv(file = 'data/data2021.csv')
data2024 <- read.csv(file = 'data/data2024.csv')
proto2021 <- readRDS(file = 'data/proto2021_5.rds')
proto2024 <- readRDS(file = 'data/proto2024_5.rds')


# palabras mas/menos del 2021 ---------------------


polar2021 = evoc %>% filter(muestra==2021) %>%
  group_by(lemma) %>%
  summarise(
    n=n(),
    mean_val=mean(valoracion)
  ) %>%
  filter(n>2) 

summary(polar2021)

library(wordcloud2)
wordcloud2::wordcloud2(polar2021 %>% filter(mean_val > 5) %>% select(-mean_val))
wordcloud2::wordcloud2(polar2021 %>% filter(mean_val < 5) %>% select(-mean_val))

# palabras mas/menos del 2024 ---------------------

glimpse(data2024)

library(psych)
library(corrplot)

gaais_data <- data2024 %>%
  select(starts_with("gaais_")) %>%
  drop_na()

fa_result <- fa(gaais_data, nfactors = 2, rotate = "varimax", fm = "ml")
fa_result
corrplot(fa_result$loadings, is.corr = FALSE, tl.col = "black", tl.cex = 0.8)
rm(fa_result)

dimension1 <- gaais_data %>% select(gaais_2n, gaais_3n, gaais_4n, gaais_6n, gaais_8n)
dimension2 <- gaais_data %>% select(gaais_1p, gaais_3p, gaais_4p, gaais_5p, gaais_8p)
alpha_dimension1 <- psych::alpha(dimension1)$total$std.alpha
alpha_dimension2 <- psych::alpha(dimension2)$total$std.alpha
list(negativa = alpha_dimension1, positiva = alpha_dimension2)

rm(dimension1, dimension2, alpha_dimension1, alpha_dimension2)

gaais_data <- gaais_data %>%
  mutate(
    negativa = rowMeans(select(., gaais_2n, gaais_3n, gaais_4n, gaais_6n, gaais_8n), na.rm = TRUE),
    positiva = rowMeans(select(., gaais_1p, gaais_3p, gaais_4p, gaais_5p, gaais_8p), na.rm = TRUE)
  ) %>%
  select(negativa,positiva)

glimpse(data2024)
x <- data2024 %>% select(id) %>% cbind(gaais_data)

evoc %>% filter(muestra==2024) %>%
  select(id,lemma) %>%
  mutate(id = as.numeric(id)) %>%
  filter(id %in% (x %>% filter(negativa > 3) %>% pull(id))) %>% 
  count(lemma) %>% filter(n>2) %>%
  wordcloud2(.)

evoc %>% filter(muestra==2024) %>%
  select(id,lemma) %>%
  mutate(id = as.numeric(id)) %>%
  filter(id %in% (x %>% filter(positiva > 3) %>% pull(id))) %>% 
  count(lemma) %>% filter(n>2) %>%
  wordcloud2(.)
