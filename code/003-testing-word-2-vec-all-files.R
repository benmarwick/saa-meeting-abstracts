library(tidyverse)

# starting with https://quanteda.io/articles/quickstart.html
library("quanteda")

# get all txt files of abstracts
abstract_txt_files <- 
fs::dir_ls(here::here("data/derived-data/"), 
           recurse = TRUE, 
           regexp = '\\.txt$')

all_txts <- tolower(readtext::readtext(abstract_txt_files))
all_txts_c <- corpus(all_txts)

# check to see if we have them all, and they are all different
summary(all_txts_c)

# keyword in context
kwic(all_txts_c, pattern = "hypothesis")

# make a dfm, removing stopwords and applying stemming
all_txts_c_dtm <- 
  dfm(all_txts_c,
      remove = stopwords("english"),
      stem = TRUE, 
      remove_punct = TRUE)

all_txts_c_dtm[, 1:5]

textplot_wordcloud(all_txts_c_dtm) 

topfeatures(all_txts_c_dtm, 20) # 20 most frequent words

# Now continuing from https://quanteda.io/articles/pkgdown/replication/text2vec.html

all_txts_c_dtm_toks1 <- tokens(all_txts_c, 
                  remove_punct = TRUE,
                  remove_symbols = TRUE,
                  remove_numbers = TRUE,
                  remove_url = FALSE,
                  remove_separators = TRUE,
                  split_hyphens = FALSE)

all_txts_c_feats <- dfm(all_txts_c_dtm_toks1, 
                tolower = TRUE,
                verbose = TRUE,
               # remove = c(stopwords("english")),
               # stem = TRUE, 
                remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 3) %>%
  featnames()

# leave the pads so that non-adjacent words will not become adjacent
all_txts_c_dtm_toks2 <- 
  tokens_select(all_txts_c_dtm_toks1, 
                all_txts_c_feats, 
                min_nchar = 3,
                padding = TRUE)

# Construct the feature co-occurrence matrix
all_txts_c_fcm <- fcm(all_txts_c_dtm_toks2, 
              context = "window", 
              tri = TRUE)

library("text2vec")

glove <- GlobalVectors$new(rank = 50, x_max = 10)
all_txts_main <- glove$fit_transform(all_txts_c_fcm, 
                               n_iter = 10,
                               convergence_tol = 0.01, 
                               n_threads = 8)

all_txts_main

all_txts_context <- glove$components
word_vectors <- all_txts_main + t(all_txts_context)

# words that we have
sort(row.names(word_vectors))

what <- 
  word_vectors["food", , drop = FALSE] - word_vectors["meat", , drop = FALSE]

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

find_similar_words("theory", embedding_matrix, n = 10)
find_similar_words("hypothesis", embedding_matrix, n = 10)

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
