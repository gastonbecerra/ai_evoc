library(tidyverse)
library(googlesheets4)

# CARGA 2021 ------------------------------------------------------

data2021 <- readr::read_csv("./data2021/estimulos_todos.csv") %>% 
  filter(estimulo == "Inteligencia artificial") %>%
  left_join(readr::read_csv("./data2021/sociodemograficos_todos.csv"), by="id") %>%
  mutate(
    carrera2 = case_when(
      carrera %in% c("Matemáticas", "Computación e Informática", "Física", "Química", "Medio ambiente", "Biología", "Otras Naturales y Exactas", "Ing. Civil", "Ing. Eléctrica y de la Información", "Ing. Mecánica", "Ing. Química", "Ing. Médica", "Ing. del Medio Ambiente", "Biotecnología", "Nanotecnología", "Otras Ingenierías") ~ "Física, comput. e ingeniería" , 
      carrera %in% c("Psicología", "Medicina", "Ciencias de la Salud", "Biotecnología", "Otras Médicas y de la Salud") ~ "Psicología y medicina" ,
      carrera %in% c("Agricultura", "Producción Animal", "Veterinarias", "Biotecnología Agropecuaria", "Otras de Ciencias Agrícolas") ~ "Agrícolas" ,
      carrera %in% c("Economía, Negocios y Administración", "Educación", "Sociología y Política", "Urbanismo, Geografía y Arquitectura", "Comunicación y Medios", "Turismo, Eventos y Gastronomía", "Derecho", "Otras Ciencias Sociales y empresariales") ~ "Sociales y empresariales" ,
      carrera %in% c("Historia y Antropología", "Lengua y Literatura", "Filosofía y Religión", "Arte", "Otras Humanidades") ~ "Humanidades"
    )
  ) 

evoc2021 <- readr::read_csv("./data2021/terminos_todos.csv") %>% tibble() %>% 
  filter(id %in% data2021$id ) %>% 
  mutate( doc_id = row_number() ) %>%
  select(-valoracion)

glimpse(data2021)
glimpse(evoc2021)



# CARGA 2024 ---------------------------------------------------------------------

url <- "https://docs.google.com/spreadsheets/d/1IKDGy83iT6s1-mAxAFrmKJt3SLTJ9yFWYGOUmrnOEeM/edit?gid=68034389#gid=68034389"
googlesheets4::gs4_deauth()
data2024 <- read_sheet(url) %>% 
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

rm(url)

glimpse(data2024)

evoc2024 <- data2024 %>%
  select(id, 1:5) %>%
  pivot_longer(cols = -1, names_to = "orden", values_to = "palabra") %>%
  mutate(orden = str_extract(orden, "\\d+") %>% as.numeric(.), 
         evoc_id = paste(id, orden, sep = "-"),
         palabra = str_to_lower(palabra) %>% str_trim(side = "both")
  ) %>% select(-evoc_id) %>% 
  mutate( doc_id = row_number() )

glimpse(data2024)
glimpse(evoc2024)



# LIMPIEZA -------------------------------------------------


library(udpipe)
ud_model <- udpipe_load_model(file = '../dix/spanish-gsd-ud-2.5-191206.udpipe')

lemma2021 <- udpipe_annotate(ud_model, 
                             x = evoc2021$palabra, doc_id = evoc2021$doc_id,
                             trace = 100, ) %>% as_tibble() %>%
  group_by(doc_id) %>%
  summarise(lemma = paste(lemma, collapse = " ")) %>%
  mutate(lemma = str_to_lower(lemma)) %>%
  mutate(lemma = chartr("áéíóúÁÉÍÓÚ", "aeiouaeiou", lemma),
         lemma = str_replace_all(lemma, "\\s+", ""),
         ) %>%
  mutate(doc_id=as.integer(doc_id)) %>%
  arrange(doc_id)

lemma2021

evoc2021 <- evoc2021 %>% 
  left_join(lemma2021) %>%
  filter(!is.na(lemma))

evoc2021

lemma2024 <- udpipe_annotate(ud_model, 
                             x = evoc2024$palabra, doc_id = evoc2024$doc_id,
                             trace = 100) %>% 
  as_tibble() %>%
  group_by(doc_id) %>%
  summarise(lemma = paste(lemma, collapse = " ")) %>%
  mutate(lemma = str_to_lower(lemma)) %>%
  mutate(lemma = chartr("áéíóúÁÉÍÓÚ", "aeiouaeiou", lemma),
         lemma = str_replace_all(lemma, "\\s+", "")) %>%
  mutate(doc_id = as.integer(doc_id)) %>%
  arrange(doc_id)

lemma2024

evoc2024 <- evoc2024 %>% 
  left_join(lemma2024) %>%
  filter(!is.na(lemma))

rm(lemma2021, lemma2024, ud_model)


evoc2021$muestra = 2021
evoc2024$muestra = 2024

evoc = rbind(evoc2021, evoc2024)

evoc = evoc %>%
  mutate(
    lemma = str_replace_all(lemma, fixed("tecnologir"),fixed("tecnologia")),
    lemma = str_replace_all(lemma, fixed("herramientar"),fixed("herramienta")),
    lemma = str_replace_all(lemma, fixed("robotica"),fixed("robot")),
    lemma = str_replace_all(lemma, fixed("bigdatar"),fixed("big data")),
    lemma = str_replace_all(lemma, fixed("avances"),fixed("avance")),
    lemma = str_replace_all(lemma, fixed("algoritr"),fixed("algoritmo")),
    lemma = str_replace_all(lemma, fixed("algoritmos"),fixed("algoritmo")),
    lemma = str_replace_all(lemma, fixed("."),fixed("")),    
  ) %>%
  filter(lemma != "")


glimpse(evoc)



# EXPORTA -------------------------------------------------

data2021 %>% write.csv('./data/data2021.csv')
data2024 %>% write.csv('./data/data2024.csv')
evoc %>% write.csv('./data/evoc.csv')




# ANALISIS PROTOTIPICO ----------------------------------------------------------------

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

proto2021 = crear_evok(tabla = evoc %>% filter(muestra==2021), 
                       palabra = lemma, orden = orden, 
                       frecuencia_minima = 5
)

proto2024 = crear_evok(tabla = evoc %>% filter(muestra==2024), 
                       palabra = lemma, orden = orden, 
                       frecuencia_minima = 5
)



proto2021 %>% saveRDS(file = 'data/proto2021_5.rds')
proto2024 %>% saveRDS(file = 'data/proto2024_5.rds')
