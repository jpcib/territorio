#Normalizo encuestas de funcionarios de territorio
##test package #1####
# install.packages("xlsx")
# library(xlsx)
library(readxl)
library(tidyverse)

###

# path <- "data/encuesta/Arretino Juan.xlsx"
# files <- list.files(path = "data/encuesta", pattern = ".xlsx")
files <- list.files(path = "data/encuesta", pattern = ".xlsx", full.names = T)

files
# raw <- files %>% 
#   exc

bind_excel <- function(.x) {
    .x %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map(read_excel, path = .x, skip = 6) %>% 
    map_df(~mutate(.,across(everything(), as.character)),
           .id = "esc")
}

df <- files %>%
  set_names() %>% 
  map(#.x = files,
      .f = bind_excel)

df2 <- df %>% 
  bind_rows(.id = "source") %>% 
  rename(cui = ...2, id = ...3, texto = ...4) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), ~as.character(.))) %>% 
  mutate(id = case_when(is.na(id) ~ str_extract(cui, "\\w\\d"),
                        TRUE ~ id
                           ),
         texto = case_when(is.na(texto) ~ cui, 
                           TRUE ~ texto),
         cui = case_when(str_detect(cui,"\\d{6}") ~ cui,
                         TRUE ~ as.character(NA))) %>%
  # filter(if_any(everything(), ~ str_detect(., "\\w\\d\\)"))) %>% 
  filter(str_detect(texto, "\\w\\d\\)")) %>% 
  mutate(across(x1:x10, ~str_to_upper(.))) %>% 
  mutate(across(x1:x10, ~case_when(str_detect(.,"X") ~ cur_column(), 
                                   TRUE ~ .))) %>% 
  # filter(if_any(x1:x10, ~str_detect(.,"x")| is.na(.))) %>%
  glimpse()


df_clean <- df2 %>% 
    # group_by(source,esc,cui,id) %>% 
    pivot_longer(cols = starts_with("x"), 
               names_to = "id2",
               names_prefix = "x",
               values_to = "ans",
               values_drop_na = TRUE
               ) %>% 
  
  select(-id2) %>% 
  select(-texto) %>% 
  mutate(ans = str_remove(ans, "x")) %>% 
  mutate(ans = as.numeric(ans)) %>% 
  # mutate(id2 = id) %>% 
  glimpse()

write_csv(df_clean, "data/encuesta_limpia_v1.csv")
writexl::write_xlsx(df_clean,"encuesta_limpia_v1.xlsx")

df_clean_v2 <- 
  df_clean %>% 
  pivot_wider(id_cols = c(source,esc,cui),
               names_from = id,
               values_from = ans) %>% 
  glimpse()


df_clean %>% 
dplyr::group_by(source, esc, cui, id) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

write_csv(df_clean_v2, "data/encuesta_limpia_v2.csv")
writexl::write_xlsx(df_clean_v2,"data/encuesta_limpia_v2.xlsx")



# 
# ###
# #esto que sigue se lo tengo que aplicar a cada uno de los archivos en files.
# raw_sheets <-
#   path %>% 
#   excel_sheets() %>% 
#   set_names() %>% 
#   map(read_excel, path = path)
# 
# clean_sheets <- 
#   raw_sheets %>% 
#   map_dfr(~mutate(.,across(everything(), as.character))) %>% 
#   # select(where(~is.numeric(.x) && any(.x == 9)))
#   mutate(across(everything(), ~as.character(.))) %>% 
#   # select(where(any(str_detect(.x,"\\d")))) %>% 
#   filter(if_any(everything(), ~ str_detect(., "\\w\\d\\)"))) %>% 
#   print()
# 
# ####
# 
# basepath <- "data/encuesta/"
# 
# raw <- read_xlsx(paste0(basepath,"Arretino Juan.xlsx"))
# raw2 <- read_xlsx(paste0(basepath,"Diaz Bettina.xlsx"))
# 
# clean <- raw %>% 
#   mutate(across(everything(), ~as.character(.))) %>% 
#   filter(if_any(everything(), ~ str_detect(., "\\w\\d\\)"))) %>% 
#   print()

# transform_raw <- map(lista, ~mutate(.,across(everything(), as.character)))
# 
# 
#   map_dfr(., ~mutate(across(everything(), ~as.character(.))))
#   mutate(across(everything(), ~as.character(.))) %>% 
#   filter(if_any(everything(), ~ str_detect(., "\\w\\d\\)"))) %>% 
#   glimpse()
#   bind_rows(.id = "escuela")
#   
