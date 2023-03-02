#Test t2v v2 - Con live data

#Librerías###
install.packages("tm")
library(tidyverse)
library(text2vec)
library(tm)
library(stringi)
library(data.table)
install.packages("tidytext")
library(tidytext)


#Limpieza####
browseVignettes(package = "text2vec")
general_clean <- fread("data/solicitudes_clasificadas_16-07_12-09_clean.csv") %>% 
  select(asunto,observaciones,area_id) %>% 
  mutate(text = paste(asunto, observaciones)) %>%
  # mutate(text = asunto) %>% 
  # mutate(text = observaciones) %>% 
  # mutate(area_id = case_when(area_id == 4 ~ 1, 
  #                   TRUE ~ 2)) %>% 
  select(text, area_id) %>% 
  glimpse()

reclamos <- 
  general_clean %>% 
  mutate(across(where(is.character), ~ iconv(.x ,from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
                  str_to_lower() %>% 
                  str_replace_all("\\d|[:punct:]"," ") %>% 
                  str_squish() %>% 
                  str_replace_all(" x ", " por ") %>% 
                  str_replace_all(" er ", " ")
  )
  )%>%
  mutate(id = row_number()) %>% 
  # select(-asunto) %>% 
  drop_na() %>% 
  glimpse()
tidytext::get_stopwords(language = "es")

stop_words = tidytext::get_stopwords(language = "es")
stop_words = stop_words$word 

setkey(reclamos, id)
set.seed(2017L)
all_ids = reclamos$id
train_ids = sample(all_ids, 400)
test_ids = setdiff(all_ids, train_ids)
train = reclamos[J(train_ids)]
test = reclamos[J(test_ids)]



#2
# define preprocessing function and tokenization function
prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$id, 
                  progressbar = FALSE)


# vocab = create_vocabulary(it_train, stopwords = stop_words)
# vocab = create_vocabulary(it_train)
vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 2L))

vocab
#3

vectorizer = vocab_vectorizer(vocab)
t1 = Sys.time()
dtm_train = create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

dim(dtm_train)


identical(rownames(dtm_train), train$id)

# install.packages("glmnet")
library(glmnet)
NFOLDS = 4
t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = dtm_train, y = train[['area_id']], 
                              # family = 'multinomial', 
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              # type.measure = "auc",
                              # 5-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-3,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))


# Note that most text2vec functions are pipe friendly!
  it_test = tok_fun(prep_fun(test$text))
# turn off progressbar because it won't look nice in rmd
it_test = itoken(it_test, ids = test$area_id, progressbar = FALSE)


dtm_test = create_dtm(it_test, vectorizer)

preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1]
glmnet:::auc(test$area_id, preds)

