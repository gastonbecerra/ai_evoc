library(tidyverse)
library(googlesheets4)

url <- "https://docs.google.com/spreadsheets/d/1Zj15LkmCBlCIojAjHH-2wDJ3YKZXPXISOWN880ncgSc/edit?resourcekey#gid=68034389"
data <- read_sheet(url) %>% janitor::clean_names()

glimpse(data)
names(data)

data %>%
  select(
    primera_palabra, segunda_palabra, tercera_palabra, cuarta_palabra, quinta_palabra,
    gaais_1n = 7, gaais_2n = 8, gaais_3n = 10, gaais_4n = 11, gaais_5n = 12, gaais_6n = 15, gaais_7n = 18, gaais_8n = 19, gaais_9n = 21,  
    gaais_1p = 9, gaais_2p = 13, gaais_3p = 14, gaais_4p = 16, gaais_5p = 17, gaais_6p = 20, gaais_7p = 22, gaais_8p = 23,
    conocimiento = 24, experiencia = 25,
    uso_redes = 26, uso_plataformas = 27, uso_apps = 28, uso_ias = 29, uso_asistentes = 30, uso_robots = 31,
    
  ) %>%
  mutate(
    id = row_number()
  )%>%
  glimpse()
