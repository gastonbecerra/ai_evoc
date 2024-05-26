library(tidyverse)
library(googlesheets4)

url <- "https://docs.google.com/spreadsheets/d/1Zj15LkmCBlCIojAjHH-2wDJ3YKZXPXISOWN880ncgSc/edit?resourcekey#gid=68034389"
data <- read_sheet(url) %>% 
  janitor::clean_names() %>%
  select(
    primera_palabra, segunda_palabra, tercera_palabra, cuarta_palabra, quinta_palabra,
    gaais_1n = 7, gaais_2n = 8, gaais_3n = 10, gaais_4n = 11, gaais_5n = 12, gaais_6n = 15, gaais_7n = 18, gaais_8n = 19, gaais_9n = 21,  
    gaais_1p = 9, gaais_2p = 13, gaais_3p = 14, gaais_4p = 16, gaais_5p = 17, gaais_6p = 20, gaais_7p = 22, gaais_8p = 23,
    conocimiento = 24, experiencia = 25,
    edad = edad_anos, genero, estudio = maximo_nivel_de_estudio_completo_o_en_curso, ocupacion  = 55,
    uso_redes = 26, uso_plataformas = 27, uso_apps = 28, uso_ias = 29, uso_asistentes = 30, uso_robots = 31,
    minipip1 = 32, minipip2 = 33, minipip3 = 34, minipip4 = 35, minipip5 = 36, minipip6r = 37, minipip7r = 38, minipip8r = 39, minipip9r = 40, 
    minipip10r = 41, minipip11 = 42, minipip12 = 43, minipip13 = 44, minipip14 = 45, minipip15r = 46, minipip16r = 47, minipip17r = 48,
    minipip18r = 49, minipip19r = 50, minipip20r = 51
  ) %>%
  mutate(
    id = row_number()
  )%>%
  glimpse()

write.csv(x = data, file = './data/ai_evoc.csv')


