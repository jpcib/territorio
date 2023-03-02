#Snippet xl2en
#Lee todas las hojas de un libro de excel y las envía al ambiente de trabajo

xl2env <- function(file){
  library(readxl)
  library(janitor)
  library(purrr)
  sheets <- excel_sheets(file)
  for (i in 1:length(sheets)){
    assign(make_clean_names(sheets[i]), read_excel(file, sheet = sheets[i]), envir = .GlobalEnv)
    assign(make_clean_names(sheets[i]), clean_names(get(make_clean_names(sheets[i]))), envir = .GlobalEnv)
  }
}
