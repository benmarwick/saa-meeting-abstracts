
library(quanteda)

# from https://quanteda.io/articles/pkgdown/replication/text2vec.html

# get a list of all words in all documents
all_words <-
  data_corpus_inaugural %>% 
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE) %>% 
  types()

# should expect this mean features in each set
length(all_words) # 10052

# these are our three sets that we want to compare, we want to project the
# change in a few key words on a fixed background of other words
corpus_1 <- data_corpus_inaugural[1:19]
corpus_2 <- data_corpus_inaugural[20:39]
corpus_3 <- data_corpus_inaugural[40:58]

my_tokens1 <- texts(corpus_1) %>%
  char_tolower() %>%
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE) 

my_tokens2 <- texts(corpus_2) %>%
  char_tolower() %>%
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE) 

my_tokens3 <- texts(corpus_3) %>%
  char_tolower() %>%
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE) 

my_feats1 <- #  10052
  dfm(my_tokens1, verbose = TRUE) %>%
  dfm_trim(min_termfreq = 5) %>% 
  dfm_match(all_words) %>% 
  featnames()

my_feats2 <- 
  dfm(my_tokens2, verbose = TRUE) %>%
  dfm_trim(min_termfreq = 5) %>%
  dfm_match(all_words) %>% 
  featnames()

my_feats3 <- 
  dfm(my_tokens3, verbose = TRUE) %>%
  dfm_trim(min_termfreq = 5) %>%
  dfm_match(all_words) %>% 
  featnames()

# leave the pads so that non-adjacent words will not become adjacent
my_toks1_2 <- tokens_select(my_tokens1, my_feats1, padding = TRUE)
my_toks2_2 <- tokens_select(my_tokens2, my_feats2, padding = TRUE)
my_toks3_2 <- tokens_select(my_tokens3, my_feats3, padding = TRUE)

# Construct the feature co-occurrence matrix
my_fcm1 <- fcm(my_toks1_2, context = "window", tri = TRUE)
my_fcm2 <- fcm(my_toks2_2, context = "window", tri = TRUE)
my_fcm3 <- fcm(my_toks3_2, context = "window", tri = TRUE)

# Fit word embedding model for each set
library("text2vec")

glove1 <- GlobalVectors$new(rank = 50, 
                            x_max = 10)

my_main1 <- glove1$fit_transform(my_fcm1, 
                               n_iter = 10,
                               convergence_tol = 0.01, 
                               n_threads = 8)

my_context1 <- glove1$components
word_vectors1 <- my_main1 + t(my_context1)

glove2 <- GlobalVectors$new(rank = 50, 
                            x_max = 10,
                            init = list(w_i = t(my_main1), 
                                        b_i = glove1$bias_i, 
                                        w_j = my_context1, 
                                        b_j = glove1$bias_j))

my_main2 <- glove2$fit_transform(my_fcm2, 
                                 n_iter = 10,
                                 convergence_tol = 0.01, 
                                 n_threads = 8)

my_context2 <- glove2$components
word_vectors2 <- my_main2 + t(my_context2)

glove3 <- GlobalVectors$new(rank = 50, 
                            x_max = 10)

my_main3 <- glove3$fit_transform(my_fcm3, 
                                 n_iter = 10,
                                 convergence_tol = 0.01, 
                                 n_threads = 8)

my_context3 <- glove3$components
word_vectors3 <- my_main3 + t(my_context3)



library(Rtsne)
library(ggplot2)

embedding_matrix <- word_vectors1

tsne <- Rtsne(embedding_matrix[2:500,], perplexity = 50, pca = FALSE)

tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(embedding_matrix)[2:500]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 3)
tsne_plot

