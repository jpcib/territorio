#Limpieza de datos de visitas
#Territorio
library(data.table)
library(tidyverse)
library(readxl)
source("script/fn_xl2env.R")

#Read Data####
file <- "data/solicitudes_clasificadas_16-07_12-09.xlsx"
xl2env(file)

desplegable <- read_xlsx("data/desplegable_app_2022_10.xlsx") %>% 
  janitor::clean_names() %>% 
  glimpse()

area <- desplegable %>% 
  filter(sub_area == "0") %>%
  select(-sub_area) %>% 
  print()

sub_area <- desplegable %>% 
  filter(sub_area != 0) %>% 
  print()

desplegable_clean <- left_join(area,sub_area, by = "area_responsable")
write.csv(desplegable_clean, "data/desplegable_clean.csv")
#Me quedo con el general
general_raw <- general_21_07_al_12_09_14
rm(general_21_07_al_12_09_14)


#Check col vars
unique(general_clean$area)
unique(general_raw$area)
unique(general_raw$subarea)
unique(general_raw$asunto)

general_clean <- general_raw %>%
  select(-c(columna1)) %>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  rename(area2 = area, 
         area = area_responsable) %>% 
  #####
  mutate(area_id = case_when(area == "tecnología educativa y sustentabilidad (sstes)" ~ 1,
                             area == "coordinación pedagógica y equidad educativa (sscpee)" ~ 2, 
                             area == "administración de recursos (ssgefyar)" ~ 3,
                             area == "infraestructura escolar y entornos de esc. (ssgefyar)" ~ 4,
                             area == "servicios a las esc. (ssgefyar)" ~ 5,
                             area == "asuntos legales (dgclei)" ~ 7,
                             area == "comuna" ~ 8,
                             area == "seguridad" ~ 8,
                             area == "limpieza y portería (sscdoc)" ~ 9,
                             area == "recursos humanos (sscdoc)" ~9,
                             area == "carrera docente (sscd)" ~ 9,
                             area == "mantenimiento" ~ 10, 
                             area == "mobiliario" ~ 11,
                             TRUE ~ 99
                             
                             )) %>% 
  mutate(area_id = case_when(area_id == 99 & str_detect(area2, "bandera|escudo") ~ 3, 
                             area_id == 99 & str_detect(area2, "telefonia") ~ 3, 
                             area_id == 99 & str_detect(asunto, "reja") ~ 10,
                             TRUE ~ area_id)) %>% 
  #####
  # filter(area_id == 99) %>%
  # select(asunto:area_id) %>% 
  glimpse()

write_csv(general_clean,"data/solicitudes_clasificadas_16-07_12-09_clean.csv")
