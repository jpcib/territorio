#Normalizo encuestas de funcionarios de territorio
##test package #1####
# install.packages("xlsx")
# library(xlsx)
library(readxl)
library(tidyverse)

###

path <- "data/encuesta/Arretino Juan.xlsx"
files <- list.files(path = "data/encuesta", pattern = ".xlsx")

files
raw <- files %>% 
  exc

#esto que sigue se lo tengo que aplicar a cada uno de los archivos en files.
raw_sheets <-
  path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = files)

clean_sheets <- 
  raw_sheets %>% 
  map_dfr(~mutate(.,across(everything(), as.character))) %>% 
  mutate(across(everything(), ~as.character(.))) %>% 
  filter(if_any(everything(), ~ str_detect(., "\\w\\d\\)"))) %>% 
  print()


####

basepath <- "data/encuesta/"

raw <- read_xlsx(paste0(basepath,"Arretino Juan.xlsx"))
raw2 <- read_xlsx(paste0(basepath,"Diaz Bettina.xlsx"))

clean <- raw %>% 
  mutate(across(everything(), ~as.character(.))) %>% 
  filter(if_any(everything(), ~ str_detect(., "\\w\\d\\)"))) %>% 
  print()

# transform_raw <- map(lista, ~mutate(.,across(everything(), as.character)))
# 
# 
#   map_dfr(., ~mutate(across(everything(), ~as.character(.))))
#   mutate(across(everything(), ~as.character(.))) %>% 
#   filter(if_any(everything(), ~ str_detect(., "\\w\\d\\)"))) %>% 
#   glimpse()
#   bind_rows(.id = "escuela")
#   
