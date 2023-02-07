# scipt exploratorio 
# Datos de visitas
library(data.table)
library(tidyverse)
library(readxl)
library(writexl)

sol_clas_raw <- read_xlsx("data/solicitudes_clasificadas_16-07_12-09.xlsx", sheet = 3) %>% 
  print()

excel_sheets(path = "data/solicitudes_clasificadas_16-07_12-09.xlsx")

file <- "data/solicitudes_clasificadas_16-07_12-09.xlsx"

# hojas <- map(excel_sheets(path), read_excel, path = path) 

#write R function that reads an excell file with multiple sheets and exports each one to a df object in the global enviroment, with the name of each sheet as the object name
# read_excel_sheets <- function(file){
#   library(readxl)
#   sheets <- excel_sheets(file)
#   for (i in 1:length(sheets)){
#     assign(sheets[i], read_excel(file, sheet = sheets[i]), envir = .GlobalEnv)
#   }
# }

#Add to the previous function an operation that cleans the names of the sheets as the clean_names function does with column names in the janitor package

read_excel_sheets <- function(file){
  library(readxl)
  library(janitor)
  sheets <- excel_sheets(file)
  for (i in 1:length(sheets)){
    assign(make_clean_names(sheets[i]), read_excel(file, sheet = sheets[i]), envir = .GlobalEnv)
  }
}


#modify the previous function to make use of the purrr package and add a function that cleans the column names of all the sheets in the original excell


read_excel_sheets_purrr <- function(file){
  library(readxl)
  library(janitor)
  library(purrr)
  sheets <- excel_sheets(file)
  for (i in 1:length(sheets)){
    assign(make_clean_names(sheets[i]), read_excel(file, sheet = sheets[i]), envir = .GlobalEnv)
  }
}

clean_column_names <- function(file){
  library(readxl)
  library(janitor)
  library(purrr)
  sheets <- excel_sheets(file)
  for (i in 1:length(sheets)){
    assign(make_clean_names(sheets[i]), read_excel(file, sheet = sheets[i]), envir = .GlobalEnv)
    assign(make_clean_names(sheets[i]), clean_names(get(make_clean_names(sheets[i]))), envir = .GlobalEnv)
  }
}
#
# read_excel_sheets(file)
clean_column_names(file)

# install.packages("arsenal")
# library(arsenal)
compare_df_cols(mget(ls()))

 mget(ls())
ls()
map_df(, compare_df_cols)
lapply(ls(), )
objects()
lapply(list(), compare_df_cols)

list2DF()
#
##
####old####
xl_sheet_2env <- function(path) {
  path %>% 
    excel_sheets() %>% 
    set_names() %>% #Unfinished
}

bind_excel <- function(.x) {
  .x %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map(read_excel, path = .x, skip = 6) %>% 
    map_df(~mutate(.,across(everything(), as.character)),
           .id = "esc")
}

df <- path %>%
  set_names() %>% 
  map(#.x = files,
    .f = bind_excel)






plani_clean %>%
  #Separo por estado> 
  #en_obra,en_proyecto,finalizado,fracaso,licitacion,planificado,desconocido
  split(f = .$estado %>% 
          snakecase::to_any_case(.)) %>% 
  #Envio al ambiente como df
  list2env(.GlobalEnv)
