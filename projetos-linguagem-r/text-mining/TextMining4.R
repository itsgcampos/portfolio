#Quarto test - Poetas
library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("SnowballC")
#install.packages("stopwords")

dados_totais <- read.csv("portuguese-poems.csv")
poetas <- dados_totais[,1]
poetas <- as_tibble(poetas)

#Mudamos o encoding para ver os acentos
#Discutir o que é encoding: https://blog.caelum.com.br/entendendo-unicode-e-os-character-encodings/amp/
Encoding(poetas$value) <- "ASCII"

#vamos retirar os acentos
for (i in 1:nrow(poetas))
{
  poetas$value[i] <- iconv(poetas$value[i], to = "ASCII//TRANSLIT")
}

#Unnest tokens - vamos fazer n gramas de 2 = bigrama
poetas <- poetas %>%  unnest_tokens(word, value, token = "ngrams", n = 2) 

#VAMOS CONTAR
poetas <- poetas %>% count(word, sort = TRUE)

#Correlação entre palavras - widyr
#install.packages("widyr")
#install.packages("janeaustenr")
library("widyr")
library("janeaustenr")

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)
