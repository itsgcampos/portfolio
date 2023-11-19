#PACKAGES
library("janeaustenr")
library("dplyr")
library("tidytext")
library("ggplot2")


textos <- austen_books()

#Vamos usar Othello e Contos do Norte
book_words <- textos %>%  unnest_tokens(word, text) %>% group_by(book) %>% count(book, word, sort = TRUE)

#Avalia Lei de ZIPF 
#A lei de Zipf afirma que a frequência com que uma palavra aparece é inversamente proporcional a sua classificação.

#TF-IDF
#Não vamos pré-processar, a ideia é mostrar papel do tf-idf
books_tf_idf <- book_words %>% bind_tf_idf(word, book, n)

#Separa para ver palavras mais importantes por texto
sense_tf_idf <- books_tf_idf %>% filter(book == 'Sense & Sensibility')
mansfield_tf_idf <- books_tf_idf %>% filter(book == 'Mansfield Park')

#Realiza gráfico com palavras mais importantes
books_graph <- books_tf_idf %>% 
  group_by(book) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) 

books_graph %>% ggplot(aes(tf_idf, word, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~book, ncol = 2, scales = "free")