library(tidyverse)


# starting with https://quanteda.io/articles/quickstart.html
library("quanteda")

a1 <- tolower(readtext::readtext(here::here("data/derived-data/abstracts-ocr/1973/txt_from_split_pages/abstract-text-1973.txt")))
a1c <- corpus(a1)
summary(a1c)

# keyword in context
kwic(a1c, pattern = "hypothesis")

# make a dfm, removing stopwords and applying stemming
a1c_dtm <- 
  dfm(a1c,
      remove = stopwords("english"),
      stem = TRUE, remove_punct = TRUE)

a1c_dtm[, 1:5]

textplot_wordcloud(a1c_dtm) 

topfeatures(a1c_dtm, 20) # 20 most frequent words

# Now continuing from https://quanteda.io/articles/pkgdown/replication/text2vec.html

a1_toks1 <- tokens(a1c, 
                  remove_punct = TRUE,
                  remove_symbols = TRUE,
                  remove_numbers = TRUE,
                  remove_url = FALSE,
                  remove_separators = TRUE,
                  split_hyphens = FALSE)

a1_feats <- dfm(a1_toks1, 
                tolower = TRUE,
                verbose = TRUE,
               # remove = c(stopwords("english")),
               # stem = TRUE, 
                remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 3) %>%
  featnames()

# leave the pads so that non-adjacent words will not become adjacent
a1_toks2 <- 
  tokens_select(a1_toks1, 
                a1_feats, 
                min_nchar = 3,
                padding = TRUE)

# Construct the feature co-occurrence matrix
a1_fcm <- fcm(a1_toks2, 
              context = "window", 
              tri = TRUE)

library("text2vec")

glove <- GlobalVectors$new(rank = 50, x_max = 10)
a1_main <- glove$fit_transform(a1_fcm, 
                               n_iter = 10,
                               convergence_tol = 0.01, 
                               n_threads = 8)

a1_main

a1_context <- glove$components
word_vectors <- a1_main + t(a1_context)

# words that we have
sort(row.names(word_vectors))

what <- 
  word_vectors["food", , drop = FALSE] - word_vectors["plant", , drop = FALSE]

cos_sim <- textstat_simil(x = as.dfm(word_vectors),
                          y = as.dfm(what),
                          method = "cosine")


head(sort(cos_sim[, 1], decreasing = TRUE), 10)

# how to visualise? Using code from 'Understanding the Embeddings' 
# https://blogs.rstudio.com/tensorflow/posts/2017-12-22-word-embeddings-with-keras/ 

embedding_matrix <- word_vectors

find_similar_words <- function(word, embedding_matrix, n = 5) {
  similarities <- 
    embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

find_similar_words("food", embedding_matrix)
find_similar_words("culture", embedding_matrix)

library(Rtsne)
library(ggplot2)
library(plotly)

tsne <- Rtsne(embedding_matrix[2:500,], perplexity = 50, pca = FALSE)

tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(embedding_matrix)[2:500]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 3)

tsne_plot
