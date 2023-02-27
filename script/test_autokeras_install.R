#Autokeras test
#Package install ####
install.packages("autokeras")
library(autokeras)
install_autokeras()
library(keras)
library(tidyverse)

#Test IMDB####

# Get IMDb dataset
imdb <- dataset_imdb(num_words = 1000)
c(x_train, y_train) %<-% imdb$train
c(x_test, y_test) %<-% imdb$test

# AutoKeras procceses each text data point as a character vector,
# i.e., x_train[[1]] "<START> this film was just brilliant casting..",
# so we need to transform the dataset.
word_index <- dataset_imdb_word_index()
word_index <- c(
  "<PAD>", "<START>", "<UNK>", "<UNUSED>",
  names(word_index)[order(unlist(word_index))]
)
x_train <- lapply(x_train, function(x) {
  paste(word_index[x + 1], collapse = " ")
})
x_test <- lapply(x_test, function(x) {
  paste(word_index[x + 1], collapse = " ")
})

x_train <- matrix(unlist(x_train), ncol = 1)
x_test <- matrix(unlist(x_test), ncol = 1)
y_train <- array(unlist(y_train))
y_test <- array(unlist(y_test))

library("autokeras")

# Create a text classifier, and train 10 different models
clf <- model_text_classifier(max_trials = 10) %>%
  fit(x_train, y_train)
