#Test t2v v2 - Con live data

#Librerías###
library(tidyverse)
library(text2vec)
library(readxl)
#Limpieza####

raw <- readxl::read_xlsx("data/solicitudes_clasificadas_16-07_12-09.xlsx") %>% 
  janitor::clean_names() %>% 
  # mutate(across(everything(), ~ as.character(.))) %>% 
  glimpse()

read_excel("data/solicitudes_clasificadas_16-07_12-09.xlsx")


df <- read.delim("data/solicitudes_clasificadas_16-07_12-09.xlsx")
dim(df);
