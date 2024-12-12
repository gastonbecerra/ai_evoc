library(tidyverse)
library(googlesheets4)

# LEEMOS DATOS Y ARMAMOS TABLA EVOC ------------------

url <- "https://docs.google.com/spreadsheets/d/1Zj15LkmCBlCIojAjHH-2wDJ3YKZXPXISOWN880ncgSc/edit?resourcekey#gid=68034389"
data <- read_sheet(url) %>% 
  janitor::clean_names() %>%
  select(
    palabra_1 = 2, palabra_2 = 3, palabra_3 = 4, palabra_4 = 5, palabra_5 = 6,
    gaais_1n = 7, gaais_2n = 8, gaais_3n = 10, gaais_4n = 11, gaais_5n = 12, gaais_6n = 15, gaais_7n = 18, gaais_8n = 19, gaais_9n = 21,  
    gaais_1p = 9, gaais_2p = 13, gaais_3p = 14, gaais_4p = 16, gaais_5p = 17, gaais_6p = 20, gaais_7p = 22, gaais_8p = 23,
    conocimiento = 24, experiencia = 25,
    edad = edad_anos, genero, estudio = maximo_nivel_de_estudio_completo_o_en_curso, ocupacion  = 55,
    uso_redes = 26, uso_plataformas = 27, uso_apps = 28, uso_ias = 29, uso_asistentes = 30, uso_robots = 31,
    minipip_1 = 32, minipip_2 = 33, minipip_3 = 34, minipip_4 = 35, minipip_5 = 36, minipip_6r = 37, minipip_7r = 38, minipip_8r = 39, minipip_9r = 40, 
    minipip_10r = 41, minipip_11 = 42, minipip_12 = 43, minipip_13 = 44, minipip_14 = 45, minipip_15r = 46, minipip_16r = 47, minipip_17r = 48,
    minipip_18r = 49, minipip_19r = 50, minipip_20r = 51,
    timestamp = 1
  ) %>%
  mutate(
    id = row_number()
  )

write.csv(x = data, file = './data/ai_evoc.csv')

glimpse(data)

evoc <- data %>%
  select(id, 1:5) %>%
  pivot_longer(cols = -1, names_to = "orden", values_to = "palabra") %>%
  mutate(orden = str_extract(orden, "\\d+") %>% as.numeric(.), 
         evoc_id = paste(id, orden, sep = "-"),
         palabra = str_to_lower(palabra) %>% str_trim(side = "both")
         )
  

glimpse(evoc)

# PREPROCESAMIENTO ------------------

library(udpipe)
# modelo <- udpipe_download_model(language = "spanish", model_dir = '../dix/')
# ud_model <- udpipe_load_model(modelo$file_model)

ud_model <- udpipe_load_model(file = '../dix/spanish-gsd-ud-2.5-191206.udpipe')

lemma <- udpipe_annotate(ud_model, 
                              x = evoc$palabra, doc_id = evoc$evoc_id,
                              trace = 100, ) %>% as_tibble() %>%
  filter(upos %in% c("NOUN", "ADJ", "VERB", "DET", "ADV")) %>%
  group_by(doc_id) %>%
  summarise(lemma = paste(lemma, collapse = " ")) 

# 2DO: meter reemplazo x chatgpt

evoc_lemma <- evoc %>% 
  left_join(lemma, by = c("evoc_id"="doc_id")) %>%
  filter(!is.na(lemma))

# CREAMOS EVOC ------------------

evoc_lemma %>%
  count(lemma, sort = TRUE)


crear_evok <- function( tabla, palabra, orden, frecuencia_minima = 2) {
  
  stopifnot(is.data.frame(tabla))
  stopifnot(is.numeric(frecuencia_minima))
  
  palabra_column = enquo(arg = palabra)
  orden_column = enquo(arg = orden)

  # frequency x rank table 
  freq_x_rank <- tabla %>% 
    group_by(!!palabra_column) %>% summarise(
      freq=n(), # frecuencia
      rank=mean(!!orden_column), # media de orden de evocacion
      .groups = 'drop_last'
    ) 
  
  # calculamos una frecuencia minima
  freq_x_rank <- freq_x_rank %>%
    group_by(!!palabra_column) %>%
    filter( freq >= frecuencia_minima)
  
  freq_cut <- mean(freq_x_rank$freq) # cut-off x frecuencias de evocacion
  rank_cut <- mean(freq_x_rank$rank) # cut-off x rango de evocacion
  message("frequency cut = ", freq_cut)
  message("rank cut = ", rank_cut)
  
  freq_x_rank <- freq_x_rank %>% mutate( q = case_when(
    freq >= freq_cut & rank < rank_cut ~ 1,
    freq >= freq_cut & rank >= rank_cut ~ 2,
    freq < freq_cut & rank < rank_cut ~ 3,
    freq < freq_cut & rank >= rank_cut ~ 4 
  )
  ) %>% arrange(q,desc(freq))
  
  return(
    list(
      data = freq_x_rank ,
      parameters = list(
        "freq_cut" = freq_cut ,
        "rank_cut" = rank_cut ,
        "min_cut" = frecuencia_minima ,
        "dix_length" = nrow(freq_x_rank) ,
        "q1_length" = sum(freq_x_rank$q == 1) ,
        "q2_length" = sum(freq_x_rank$q == 2) ,
        "q3_length" = sum(freq_x_rank$q == 3) ,
        "q4_length" = sum(freq_x_rank$q == 4) 
      )
    )
  )
}


zipf_model <- lm(log(freq) ~ log(rank), data = frecuencias_palabras)
frecuencias_palabras <- frecuencias_palabras %>%
  mutate(zipf_line = exp(predict(zipf_model, newdata = frecuencias_palabras)))


evok = crear_evok(tabla = evoc_lemma, 
           palabra = lemma, orden = orden, 
           frecuencia_minima = 3
           )

glimpse(evok$data)

# RQ1) EVOC GENERAL -------------------------

frecuencias_palabras <- evok$data %>%
  ungroup() %>%
  arrange(desc(freq)) %>%
  mutate(rank = row_number())

zipf_model <- lm(log(freq) ~ log(rank), data = frecuencias_palabras)
frecuencias_palabras <- frecuencias_palabras %>%
  mutate(zipf_line = exp(predict(zipf_model, newdata = frecuencias_palabras)))

ggplot(frecuencias_palabras, aes(x = reorder(lemma, -freq), y = freq, group = 1)) +
  geom_point() +
  geom_line(aes(y = zipf_line), color = "blue", size = 1) +
  labs(title = "Curva de Zipf de Frecuencia de Lemmas",
       x = "Palabra",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


evok$data %>% ggplot(aes(x=freq,y=rank,label=lemma)) + # terminos x n x ofe
  scale_x_continuous(trans='log') +
  geom_hline(yintercept = evok$parameters$rank_cut, linetype = 2) +
  geom_vline(xintercept = evok$parameters$freq_cut, linetype = 2) +
  geom_point(, show.legend = FALSE) +
  geom_text( aes(size=20), fontface = "bold",
             show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) +
  labs(y="Orden de evocación", x = "Frecuencia (log2)") +
  theme_minimal()

# RQ2) EVOC X ACTITUD -------------------------

# 2DO: HAY QUE CORTAR LA TABLA POR ACTITUD

# RQ3) EVOC CORRELACIONADAS -------------------------

library(widyr) # para grafos
library(ggraph) # para grafos
library(igraph) # para grafos
library(tidygraph) # para grafos


evoc_lemma %>%
  inner_join(evok$data) %>%
  filter(q<4) %>%
  select(id,lemma) %>%
  widyr::pairwise_cor(item = lemma, feature = id) %>%
  graph_from_data_frame( directed = FALSE ) %>% as_tbl_graph() %>%
  activate(edges) %>%
  filter(correlation>0) %>%
  activate(nodes) %>%
  left_join( evok$data %>% rename(name=lemma) ) %>%
  mutate(community=as.factor(group_leading_eigen())) %>%
  ggraph(layout = "fr" ) +  #drl, fr, dh
  geom_edge_link(aes(width = correlation, alpha = correlation), color="gray", show.legend = TRUE )+
  geom_node_point(aes(color=community), show.legend = TRUE) +
  geom_node_text(aes(label = name ), repel = TRUE, show.legend = FALSE)+
  theme_graph()


# RQ4) FACTORIALES -------------------------

# 2DO: hay que hacer una tabla de features
# seguir esta onda: 
# https://github.com/gastonbecerra/trs-estructural/blob/main/analisis.R

# tabla de fuentes consultadas por carrera
carrera_medios_bd %>% 
  dplyr::rename_all(funs(
    stringr::str_replace_all( ., "[[:punct:]]", "_" ) %>%
      stringr::str_replace_all( ., "medios_", "" )
  )) %>%
  inner_join(carrera_medios_ai %>% 
               dplyr::rename_all(funs(
                 stringr::str_replace_all( ., "[[:punct:]]", "_" ) %>%
                   stringr::str_replace_all( ., "medios_", "" ))), by ="carrera2")

# factoriales
carrera_medios_ai <- sociodemograficos %>% filter(estimulo == "Inteligencia artificial") %>% 
  group_by(carrera2) %>%
  summarise(n=n(), p=n() / nrow(sociodemograficos %>% filter(estimulo == "Inteligencia artificial"))) %>% 
  inner_join(
    sociodemograficos %>% filter(estimulo == "Inteligencia artificial") %>% 
      group_by(carrera2) %>%
      summarise_if( .predicate = is.logical, .funs = function(x) sum(x) / n() ) , by="carrera2") %>%
  select(-n,-p) 

facto_ai <- terminos %>%
  filter(estimulo == "Inteligencia artificial",
         palabra %in% (evok_ai$data %>% filter(q<5) %>% pull(palabra))) %>%
  inner_join(sociodemograficos) %>%
  group_by(palabra,carrera2) %>% summarize(n=n()) %>%
  pivot_wider(names_from = carrera2, values_from = n, values_fill = 0) %>%
  column_to_rownames(var = "palabra") %>%
  as.matrix()

facto_ai2 <- FactoMineR::CA(X = facto_ai, graph = FALSE)
ellipseCA(facto_ai2,ellipse=c('col'),cex=0.9,cex.main=0.9,cex.axis=0.9,title="Inteligencia artificial")

