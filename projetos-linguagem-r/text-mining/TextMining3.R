#Terceiro test - Poemas
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

#Unnest tokens
poetas <- poetas %>%  unnest_tokens(word, value) 

#Excluímos stop words em português - instalar stop words
poetas <- poetas %>% anti_join(get_stopwords(language = 'pt'))

#Word cloud - teste word e stem
livro_cont <- poetas %>% select(word) %>% count(word, sort = TRUE)

pal <- brewer.pal(8,"Dark2")
livro_cont %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))
