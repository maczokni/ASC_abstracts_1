library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(keras)
library(janeaustenr)
library(tokenizers)

abstracts <- read_csv("https://www.dropbox.com/s/5m3reylokvxcleo/asc_abstracts1419.csv?dl=1")

abstracts <- abstracts %>% 
  rename(id = X1, 
         abstract = `0`) %>% 
  mutate(abstract = gsub("<p>", "", abstract), 
         abstract = gsub("</p>", "", abstract))

max_length <- 40

text <- abstracts %>%
  pull(abstract) %>%
  str_c(collapse = " ") %>%
  tokenize_characters(lowercase = FALSE, strip_non_alphanum = FALSE, simplify = TRUE)

print(sprintf("Corpus length: %d", length(text)))
## [1] "Corpus length: 684767"
chars <- text %>%
  unique() %>%
  sort()

print(sprintf("Total characters: %d", length(chars)))

dataset <- map(
  seq(1, length(text) - max_length - 1, by = 3), 
  ~list(sentence = text[.x:(.x + max_length - 1)], 
        next_char = text[.x + max_length])
)

dataset <- transpose(dataset)


vectorize <- function(data, chars, max_length){
  x <- array(0, dim = c(length(data$sentence), max_length, length(chars)))
  y <- array(0, dim = c(length(data$sentence), length(chars)))
  
  for(i in 1:length(data$sentence)){
    x[i,,] <- sapply(chars, function(x){
      as.integer(x == data$sentence[[i]])
    })
    y[i,] <- as.integer(chars == data$next_char[[i]])
  }
  
  list(y = y,
       x = x)
}



vectors <- vectorize(dataset, chars, max_length)
