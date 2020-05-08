library(tidyverse)

# starting with https://quanteda.io/articles/quickstart.html
library("quanteda")

# get all txt files of abstracts
abstract_txt_files <- 
fs::dir_ls(here::here("data/derived-data/"), 
           recurse = TRUE, 
           regexp = '\\.txt$')

all_txts <- tolower(readtext::readtext(abstract_txt_files))
names(all_txts) <- map_chr(names(all_txts), ~str_match_all(.x, "\\d{4}")[[1]][1,1])

all_txts_c <- corpus(all_txts)

# make a dfm, removing stopwords and maybe applying stemming
all_txts_c_dtm <- 
  dfm(all_txts_c,
      remove = stopwords("english"),
      #stem = TRUE, 
      remove_punct = TRUE)

# investigate the target feature a bit

related_words <- c("theory", "theories", "theoretical")
map(related_words, ~colSums(all_txts_c_dtm[, .x ]))
# 'theory' is way more common, so use that

# in each text we want to replace the target feature (word) with a tagged feature,
# a time-specific token

year <- names(all_txts)
target_feature <- "theory"

time_specific_token <- paste0(target_feature, "_", year)

all_txts_updated <- vector("character", length = length(all_txts))
for(i in seq_len(length(all_txts))){
  
  tmp1 <- all_txts[i]
  tmp2 <- str_replace_all(tmp1, "-", "") 
  tmp3 <- time_specific_token[i]
  tmp4 <- str_c("\\b", target_feature, "\\b", collapse="|")
  tmp5 <- str_replace_all(tmp2, tmp4, tmp3)
  all_txts_updated[[i]] <- tmp5

}

all_txts_updated_c <- corpus(all_txts_updated)

# Now continuing from https://quanteda.io/articles/pkgdown/replication/text2vec.html

all_txts_c_dtm_toks1 <- tokens(all_txts_updated_c, 
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
                               n_iter = 20,
                               convergence_tol = -1, 
                               n_threads = 8)

all_txts_main

all_txts_context <- glove$components
word_vectors <- all_txts_main + t(all_txts_context)

# words that we have
sort(row.names(word_vectors))

# how to visualise? Using code from 'Understanding the Embeddings' 
# https://blogs.rstudio.com/tensorflow/posts/2017-12-22-word-embeddings-with-keras/ 

embedding_matrix <- word_vectors

find_similar_words <- function(word, embedding_matrix, n = 5) {
  similarities <- 
    embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

similar_words <- 
  names(find_similar_words("method", embedding_matrix, n = 100))

library(Rtsne)
library(ggplot2)
library(plotly)

plot_data <- embedding_matrix[row.names(embedding_matrix) %in% c(similar_words, 
                                                                 time_specific_token), ]
tsne <- Rtsne(plot_data, 
              perplexity = 10, 
              pca = FALSE)

tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(plot_data)) %>%
  ggplot(aes(x = V1, 
             y = V2, 
             label = word)) + 
  geom_text(size = 3) +
  theme_minimal()

tsne_plot
