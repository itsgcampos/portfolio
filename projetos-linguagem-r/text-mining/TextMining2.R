#Segundo test - Fianancial Sentiment Dataset
library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("SnowballC")
library("tibble")

# Vamos usar o Financial Sentiment Dataset
dados_totais <- read.csv("Financial Sentiment.csv")
dados_totais <- as_tibble(dados_totais)

dados <- dados_totais[,1]

dados_unnested <- dados %>%  unnest_tokens(word, Sentence)

#Vamos retirar números - pode ser qualquer coisa
dados_unnested <- dados %>%  unnest_tokens(word, Sentence) %>% filter(!grepl('[0-9]', word))

#Vamos nos livrar das stop words e obter os tokens
dados_unnested <- dados_unnested %>%  anti_join(stop_words)

#Contar as palavras mais comuns

common <- dados_unnested %>%  count(word, sort = TRUE)

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# word cloud
common %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

#Stemming - vamos aplicar 
dados_unnested_stem <-  dados_unnested %>%  mutate(stem = wordStem(word)) 

dados_unnested_stem_count <- dados_unnested_stem %>%  mutate(stem = wordStem(word)) %>% count(stem, sort = TRUE)
