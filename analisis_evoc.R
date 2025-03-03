library(tidyverse)

evoc <- read.csv(file = 'data/evoc.csv')
data2021 <- read.csv(file = 'data/data2021.csv')
data2024 <- read.csv(file = 'data/data2024.csv')
proto2021 <- readRDS(file = 'data/proto2021_5.rds')
proto2024 <- readRDS(file = 'data/proto2024_5.rds')


glimpse(evoc)
glimpse(data2021)
glimpse(data2024)




# SOCIODEMOGRAFICOS ----------------------------------------------------------------


data2021 %>% 
  select(edad, sexo, carrera2) %>%
  summarise(
    N = n(),
    `% Géneros` = paste0(names(table(sexo)), ": ", round(100 * prop.table(table(sexo)), 1), "%", collapse = ", "),
    Edad_mean = mean(edad, na.rm = TRUE),
    Edad_sd = sd(edad, na.rm = TRUE),
    `% Carreras` = paste0(names(table(carrera2)), ": ", round(100 * prop.table(table(carrera2)), 1), "%", collapse = ", ")
  )

data2024 %>% 
  summarise(
    N = n(),
    `% Géneros` = paste0(names(table(genero)), ": ", round(100 * prop.table(table(genero)), 1), "%", collapse = ", "),
    Edad_mean = mean(edad, na.rm = TRUE),
    Edad_sd = sd(edad, na.rm = TRUE),
    `% Estudios` = paste0(names(table(estudio)), ": ", round(100 * prop.table(table(estudio)), 1), "%", collapse = ", "),
    `% Ocupaciones` = paste0(names(table(ocupacion)), ": ", round(100 * prop.table(table(ocupacion)), 1), "%", collapse = ", ")
  )



# TERMINOS Y VOCABULARIO ------------------------------

before_2021 <- evoc %>% filter(muestra == 2021) %>%
  count(palabra, name = "freq") %>%
  summarize(
    unique_words = n_distinct(palabra),
    most_frequent = max(freq)
  )

before_2024 <- evoc %>% filter(muestra == 2024) %>%
  count(palabra, name = "freq") %>%
  summarize(
    unique_words = n_distinct(palabra),
    most_frequent = max(freq)
  )

after_2021 <- evoc %>% filter(muestra == 2021) %>%
  count(lemma, name = "freq") %>%
  summarize(
    unique_lemmas = n_distinct(lemma),
    most_frequent = max(freq),
    mean_freq = mean(freq)
  )

after_2024 <- evoc %>% filter(muestra == 2024) %>%
  count(lemma, name = "freq") %>%
  summarize(
    unique_lemmas = n_distinct(lemma),
    most_frequent = max(freq),
    mean_freq = mean(freq)
  )

comparison <- tibble(
  Muestra = c("2021", "2024"),
  `Palabras Únicas Antes` = c(before_2021$unique_words, before_2024$unique_words),
  `Frecuencia Máxima Antes` = c(before_2021$most_frequent, before_2024$most_frequent),
  `Lemmas Distintos Después` = c(after_2021$unique_lemmas, after_2024$unique_lemmas),
  `Frecuencia Máxima Después` = c(after_2021$most_frequent, after_2024$most_frequent),
  `Frecuencia Media Después` = c(after_2021$mean_freq, after_2024$mean_freq)
)

# view(comparison)
# comparison %>% clipr::write_clip()

rm(before_2021, before_2024, after_2021, after_2024, comparison)

proto2021$parameters
proto2024$parameters



# DISTRIBUCION Y ZIPF ----------------------------------------------------------------


evoc %>%
  group_by(muestra) %>%
  count(lemma) %>%
  filter(n > 3) %>%
  ggplot(aes(x=n,y=lemma)) + 
  geom_col() +
  facet_wrap(~muestra, scales = "free_y") + 
  theme_minimal()

zipf2021 <- proto2021$data %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(rank = row_number()) # usamos otro ranking
zipf_model <- lm(log(freq) ~ log(rank), data = zipf2021)
zipf2021$zipf_line = exp(predict(zipf_model, newdata = zipf2021))
ggplot(zipf2021, aes(x = reorder(lemma, -freq), y = freq, group = 1)) +
  geom_point() +
  geom_line(aes(y = zipf_line), color = "blue", size = 1) +
  labs(x = "Palabra",
       y = "Frecuencia") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

zipf2024 <- proto2024$data %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(rank = row_number()) # usamos otro ranking
zipf_model <- lm(log(freq) ~ log(rank), data = zipf2024)
zipf2024$zipf_line = exp(predict(zipf_model, newdata = zipf2024))
ggplot(zipf2024, aes(x = reorder(lemma, -freq), y = freq, group = 1)) +
  geom_point() +
  geom_line(aes(y = zipf_line), color = "blue", size = 1) +
  labs(x = "Palabra",
       y = "Frecuencia") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

rm(zipf_model,zipf2021,zipf2024)


# RQ1) EVOC GENERAL -------------------------


proto2021$data %>% 
  ggplot(aes(x=freq,y=rank,label=lemma)) + 
  geom_hline(yintercept = proto2021$parameters$rank_cut, linetype = 2) +
  geom_vline(xintercept = proto2021$parameters$freq_cut, linetype = 2) +
  geom_text( size=5 ) +
  labs(y="Orden de evocación", x = "Frecuencia") +
  theme_minimal()

proto2024$data %>% 
  ggplot(aes(x=freq,y=rank,label=lemma)) + 
  geom_hline(yintercept = proto2024$parameters$rank_cut, linetype = 2) +
  geom_vline(xintercept = proto2024$parameters$freq_cut, linetype = 2) +
  geom_text( size=5 ) +
  labs(y="Orden de evocación", x = "Frecuencia") +
  theme_minimal()

proto2024$data$muestra = '2024'
proto2021$data$muestra = '2021'

rbind(proto2021$data, proto2024$data) %>%
  mutate(
    lemma = if_else(muestra == 2021, str_to_upper(lemma), lemma)
  ) %>%
  ggplot(aes(x = freq, y = rank, label = lemma, color = factor(muestra))) +
  scale_x_continuous(trans = 'log') +
  scale_color_manual(values = c("2021" = "blue", "2024" = "red")) +
  geom_hline(yintercept = proto2021$parameters$rank_cut, linetype = 2, color = "blue") +
  geom_vline(xintercept = proto2021$parameters$freq_cut, linetype = 2, color = "blue") +
  geom_hline(yintercept = proto2024$parameters$rank_cut, linetype = 2, color = "red") +
  geom_vline(xintercept = proto2024$parameters$freq_cut, linetype = 2, color = "red") +
  geom_text( size=5 ) +
  labs(y = "Orden de evocación", x = "Frecuencia (log2)", color = NULL) +
  theme_minimal()


# tabla comparativa 

comparativa <- full_join(
  proto2024$data %>%
    select(lemma, freq, q) %>%
    arrange(q,desc(freq)) %>%
    ungroup() %>%
    mutate(pos_2024 = row_number()),
  proto2021$data %>%
    select(lemma, freq, q) %>%
    arrange(q,desc(freq)) %>%
    ungroup() %>%
    mutate(pos_2021 = row_number()),
  by = "lemma",
  suffix = c("_2021", "_2024")
) %>%
  mutate(diferencia = pos_2021 - pos_2024) %>%
  select(lemma, p24=pos_2024, p21=pos_2021, dif=diferencia)

comparativa

# comparativa %>% clipr::write_clip()

# RQ2) EVOCS CORRELACIONADAS -------------------------

library(ggraph)
library(igraph)
library(tidygraph)
library(widyr)

grafo2021 <- evoc %>%
  filter(muestra == 2021) %>%
  count(lemma) %>%
  filter(n > 4) %>%
  select(lemma, n) %>%
  inner_join(evoc %>% filter(muestra == 2021), by = "lemma") %>%
  select(id, lemma, n) %>%
  widyr::pairwise_cor(item = lemma, feature = id) %>%
  rename(weight = correlation) %>%
  filter(weight > 0.1) %>%
  graph_from_data_frame(directed = FALSE) %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_louvain())) %>%
  left_join(evoc %>% filter(muestra == 2021) %>% count(lemma) %>% select(name = lemma, n), by = "name")

grafo2021 %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = abs(weight), edge_width = abs(weight), color = weight), show.legend = FALSE) +
  geom_node_label(aes(label = name, size = n, color = community)) +
  scale_edge_width(range = c(0.1, 3)) +
  scale_edge_alpha(range = c(0.2, 0.8)) +
  scale_edge_color_gradient(low = "lightblue", high = "darkblue") +
  scale_size(range = c(2, 10)) +
  theme_void() +
  theme(legend.position = "none")

grafo2024 <- evoc %>%
  filter(muestra == 2024) %>%
  count(lemma) %>%
  filter(n > 4) %>%
  select(lemma, n) %>%
  inner_join(evoc %>% filter(muestra == 2024), by = "lemma") %>%
  select(id, lemma, n) %>%
  widyr::pairwise_cor(item = lemma, feature = id) %>%
  rename(weight = correlation) %>%
  filter(weight > 0.1) %>%
  graph_from_data_frame(directed = FALSE) %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_louvain())) %>%
  left_join(evoc %>% filter(muestra == 2024) %>% count(lemma) %>% select(name = lemma, n), by = "name") 

grafo2024 %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = abs(weight), edge_width = abs(weight), color = weight), show.legend = FALSE) +
  geom_node_label(aes(label = name, size = n, color = community)) +
  scale_edge_width(range = c(0.1, 3)) +
  scale_edge_alpha(range = c(0.2, 0.8)) +
  scale_edge_color_gradient(low = "lightblue", high = "darkblue") +
  scale_size(range = c(2, 10)) +
  theme_void() +
  theme(legend.position = "none")

compare_graphs <- function(graph1, graph2) {
  tibble(
    Métrica = c(
      "Número de nodos",
      "Número de aristas",
      "Densidad del grafo",
      "Número de comunidades"
    ),
    `2021` = c(
      vcount(graph1),
      ecount(graph1),
      edge_density(graph1),
      length(unique(V(graph1)$community))
    ),
    `2024` = c(
      vcount(graph2),
      ecount(graph2),
      edge_density(graph2),
      length(unique(V(graph2)$community))
    )
  )
}

comparison_table <- compare_graphs(grafo2021, grafo2024)
comparison_table

grafo2021 %>%
  activate(nodes) %>%
  as_tibble() %>%
  group_by(community) %>%
  summarise(
    Nodos = n(),
    Integración = mean(strength(grafo2021, vids = which(community == cur_group_id()), mode = "all"))
  )

grafo2024 %>%
  activate(nodes) %>%
  as_tibble() %>%
  group_by(community) %>%
  summarise(
    Nodos = n(),
    Integración = mean(strength(grafo2021, vids = which(community == cur_group_id()), mode = "all"))
  )

rm(compare_graphs, comparison_table)

# grafo2021 %>% activate(nodes) %>% as_tibble() %>% view()
# grafo2024 %>% activate(nodes) %>% as_tibble() %>% view()


# RQ3) VALORACIONES Y ACTITUDES ----------------------

library(wordcloud2)
library(psych)
library(corrplot)
library(ggrepel)

polar2021 <- evoc %>%
  filter(muestra == 2021) %>%
  group_by(lemma) %>%
  summarise(n = n(), mean_val = mean(valoracion)) %>%
  filter(n > 2) %>%
  mutate(categoria = case_when(
    mean_val > 5 ~ "positivo",
    mean_val < 5 ~ "negativo",
    TRUE ~ "neutral"
  ))

summary(polar2021)

evoc %>%
  filter(muestra == 2021) %>%
  group_by(lemma) %>%
  summarise(n = n(), mean_val = mean(valoracion)) %>%
  mutate(
    categoria = case_when(
      mean_val > 5 ~ "positivo",
      mean_val < 5 ~ "negativo",
      TRUE ~ "neutral"
    )
  ) %>%
  ggplot(aes(x = categoria, fill = categoria)) +
  geom_bar() +
  theme_minimal()

ggplot(polar2021, aes(x = mean_val, y = log10(n), label = lemma, color = categoria)) +
  geom_text_repel(size = 3, alpha = 0.7, max.overlaps = 30) +
  scale_color_manual(values = c("red", "green", "blue"), guide = "none") +
  labs(x = "Valoración Media",
       y = "Frecuencia Relativa (log10)") +
  theme_minimal()

polar2021 %>%
  count(categoria) %>%
  mutate(perc = n / sum(n) * 100)

polar2021 %>% slice_max(mean_val, n = 20)

wordcloud2::wordcloud2(polar2021 %>% filter(mean_val > 5) %>% select(-mean_val))
wordcloud2::wordcloud2(polar2021 %>% filter(mean_val < 5) %>% select(-mean_val))

## palabras mas/menos del 2024

glimpse(data2024)

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

x <- data2024 %>% select(id) %>% cbind(gaais_data) %>% mutate(id=as.numeric(id))

rm(gaais_data)

summary(x)
glimpse(x)

positivas <- evoc %>%
  filter(muestra == 2024) %>%
  select(id, lemma) %>%
  mutate(id = as.numeric(id)) %>%
  filter(id %in% (x %>% filter(positiva > 3) %>% pull(id))) %>%
  count(lemma, sort = TRUE) %>%
  filter(n > 2) %>%
  head(30)

negativas <- evoc %>%
  filter(muestra == 2024) %>%
  select(id, lemma) %>%
  mutate(id = as.numeric(id)) %>%
  filter(id %in% (x %>% filter(negativa > 3) %>% pull(id))) %>%
  count(lemma, sort = TRUE) %>%
  filter(n > 2) %>%
  head(30)

terminos_compartidos <- intersect(positivas$lemma, negativas$lemma)
terminos_distintos <- setdiff(positivas$lemma, negativas$lemma)

cbind(
  positivas %>%
  mutate(compartido = lemma %in% terminos_compartidos) ,
  negativas %>%
  mutate(compartido = lemma %in% terminos_compartidos)
)

full_join(positivas, negativas, by = "lemma", suffix = c("_pos", "_neg")) %>%
  replace_na(list(n_pos = 0, n_neg = 0)) %>%
  mutate(diferencia = n_pos - n_neg, suma = n_pos+n_neg) %>%
  # slice_max(abs(diferencia), n = 20) %>%  # Selecciona los 20 términos con mayor diferencia absoluta
  # slice_max(suma, n = 30) %>%  # selecciona los 20 terminos mas mencionados
  pivot_longer(cols = c(n_pos, n_neg), names_to = "grupo", values_to = "frecuencia") %>%
  mutate(grupo = recode(grupo, "n_pos" = "positivo", "n_neg" = "negativo")) %>%
  ggplot(., aes(x = reorder(lemma, frecuencia), 
                          y = ifelse(grupo == "positivo", frecuencia, -frecuencia), fill = grupo)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("positivo" = "blue", "negativo" = "red"), guide = "none") +
  scale_y_continuous(labels = abs) +
  labs(x = element_blank(), y = element_blank()) +
  theme_minimal()

rm(x)
rm(positivas, negativas, compartidas, compartidas_long, polar2021, terminos_compartidos, terminos_distintos)


# RQ4) FACTORES -------------------------


library(Factoshiny)
library(FactoMineR)
library(factoextra)

evoc_table <- evoc %>%
  filter(muestra == 2021) %>%
  count(lemma) %>%
  filter(n > 2) %>%
  select(lemma) %>%
  inner_join(evoc %>% filter(muestra == 2021), by = "lemma") %>%
  inner_join(data2021, by = "id") %>%
  count(carrera2, lemma) %>%
  pivot_wider(names_from = lemma, values_from = n, values_fill = 0) %>%
  column_to_rownames("carrera2")
# Factoshiny(evoc_table)

## carrera/ocupacion
## borramos los pocos casos

table(data2021$carrera2)
table(data2024$ocupacion)

facto2021 <- evoc %>%
  filter(muestra == 2021) %>%
  left_join(evoc %>% filter(muestra == 2021) %>% count(lemma), by = "lemma") %>%
  filter(n > 3) %>%
  left_join(data2021, by = "id") %>%
  group_by(carrera2) %>% mutate(x=n()) %>% filter(x > 10) %>% ungroup() %>%
  count(lemma, carrera2) %>%
  pivot_wider(names_from = carrera2, values_from = n, values_fill = 0) %>%
  column_to_rownames(var = "lemma") %>%
  as.matrix() %>%
  FactoMineR::CA(graph = FALSE)
summary(facto2021)
fviz_ca_biplot(facto2021, repel = TRUE)

facto2024 <- evoc %>%
  filter(muestra == 2024) %>%
  left_join(evoc %>% filter(muestra == 2024) %>% count(lemma), by = "lemma") %>%
  filter(n > 3) %>%
  left_join(data2024 %>% mutate(id=as.character(id)), by="id") %>%
  left_join(data2024 %>% count(ocupacion, name = "n_ocupacion"), by="ocupacion" ) %>%
  filter(n_ocupacion>15) %>%  
  count(lemma, ocupacion) %>%
  pivot_wider(names_from = ocupacion, values_from = n, values_fill = 0) %>%
  column_to_rownames(var = "lemma") %>%
  as.matrix() %>%
  FactoMineR::CA(graph = FALSE)
summary(facto2024)
fviz_ca_biplot(facto2024, repel = TRUE)



# 2do: distinguir factores y actitudes por genero

# 2do: agrupar y sintetizar ocupacion

facto2024 <- evoc %>%
  filter(muestra == 2024) %>%
  left_join(evoc %>% filter(muestra == 2024) %>% count(lemma), by = "lemma") %>%
  filter(n > 3) %>%
  left_join(data2024 %>% mutate(id=as.character(id)), by = "id") %>%
  group_by(lemma, estudio) %>%
  summarize(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = estudio, values_from = n, values_fill = 0) %>%
  column_to_rownames(var = "lemma") %>%
  as.matrix() %>%
  FactoMineR::CA(graph = FALSE)
fviz_ca_biplot(facto2024, repel = TRUE)
summary(facto2024)

