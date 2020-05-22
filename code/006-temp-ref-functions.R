
#-----------------------------------------------------------
read_in_the_abstracts_fn <- function(){

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

return(list(all_txts = all_txts,
            all_txts_c_dtm = all_txts_c_dtm))

}


#-----------------------------------------------------------

insert_time_specific_token_fn <- function(all_txts = all_txts,
                                          target_feature = "mechanisms",
                                          year_interval = 5){

# https://www.aclweb.org/anthology/P19-1044.pdf is our inspiration 
# in each text we want to replace the target feature (word) with a tagged feature,
# a time-specific token target word w ∈ Ct with a time-specific token wt
# E.g., in the corpus for 1920 we replace each occurrence of computer with the string computer1920

year <- names(all_txts)

floor_to_interval <-  function(value, interval){ return(value - value %% interval) }

# year_interval <- 5 # we can change and explore
interval <- floor_to_interval(as.numeric(year), year_interval)
# target_feature <- "mechanisms" # we can change this

time_specific_token <- 
  paste0(target_feature, "_", interval, "_", interval + year_interval, "")

# deal with the most recent one
time_specific_token[length(time_specific_token)] <- 
  str_replace(time_specific_token[length(time_specific_token)], "_\\d{4}$", "")

all_txts_updated <- vector("character", length = length(all_txts))
for(i in seq_len(length(all_txts))){
  
  tmp1 <- all_txts[i]
  tmp2 <- str_replace_all(tmp1, "-", "") 
  tmp2 <- str_replace_all(tmp2, "\n", " ")
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

return(list(all_txts_c_fcm = all_txts_c_fcm,
            time_specific_token = time_specific_token))

}

#-----------------------------------------------------------
 
generate_embedding_matrix_fn <- function(all_txts_c_fcm = all_txts_c_fcm,
                                         time_specific_token = time_specific_token,
                                         n_similar_words = 50,
                                         rank = 100){

library("text2vec")

glove <- GlobalVectors$new(rank = rank, x_max = 20)
all_txts_main <- glove$fit_transform(all_txts_c_fcm, 
                               n_iter = 20,
                               convergence_tol = -1, 
                               n_threads = 8)


all_txts_context <- glove$components
word_vectors <- all_txts_main + t(all_txts_context)

# words that we have
# sort(row.names(word_vectors))

# how to visualise? Using code from 'Understanding the Embeddings' 
# https://blogs.rstudio.com/tensorflow/posts/2017-12-22-word-embeddings-with-keras/ 

embedding_matrix <- word_vectors

find_similar_words <- function(word, embedding_matrix, n_similar_words) {
  similarities <- 
    embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n_similar_words)
}

find_similar_words_safe <- 
  safely(find_similar_words)

similar_words <- 
map(time_specific_token, 
    ~find_similar_words_safe(.x, 
                        embedding_matrix, 
                        n_similar_words = n_similar_words)) %>% # explore to change the context word quantity 
  transpose() %>%
  simplify_all() %>% 
  .$result %>% 
  unlist() %>% 
  names() %>% 
  unique()

# keep only the tokens found in an English syllables dictionary
myTokens <- 
  featnames(dfm(tokens_select(tokens(similar_words), 
                              names(data_int_syllables))))

return(list(embedding_matrix = embedding_matrix,
            myTokens = myTokens))

}

#-----------------------------------------------------------

temporal_referencing_plot_fn <- function(embedding_matrix = embedding_matrix,
                                         myTokens = myTokens,
                                         time_specific_token = time_specific_token,
                                         target_feature = target_feature,
                                         year_interval = 5,
                                         perplexity = 50){

library(Rtsne)
library(ggplot2)
library(ggrepel)

plot_data <- embedding_matrix[row.names(embedding_matrix) %in% c(myTokens, 
                                                                 time_specific_token), ] 
tsne <- Rtsne(plot_data, 
              perplexity = perplexity, # explore different values
              pca = FALSE)

tsne_plot_data <- 
  tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(plot_data)) %>% 
  filter(!word %in% time_specific_token)

tsne_plot_target_features <- 
  tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(plot_data)) %>% 
  filter(word %in% time_specific_token) %>% 
  mutate(word = str_replace(word, "_", " ")) %>% 
  mutate(word = str_replace(word, " ", " (")) %>% 
  mutate(word = str_replace(word, "_", "-")) %>% 
  mutate(word = str_c(word, ")")) 
  
tsne_plot <- 
  ggplot() + 
  geom_text(data = tsne_plot_data,
            aes(x = V1, 
                y = V2, 
                label = word), 
            size = 2,
            check_overlap = TRUE) +
  geom_segment(data = tsne_plot_target_features, 
               aes(x = V1, 
                   y = V2,
                   xend=c(tail(V1, n=-1), NA), 
                   yend=c(tail(V2, n=-1), NA)),
               arrow=arrow(length=unit(0.3,"cm"), 
                           type = "closed"),
               colour = "red") +
  geom_text_repel(data = tsne_plot_target_features, 
            aes(x = V1, 
                y = V2,
                label = word),
            size = 2,
            colour = "red",
            bg.color = "white", 
            bg.r = 0.15 ) +
  theme_minimal() +
  xlab("t-SNE dimension 1") +
  ylab("t-SNE dimension 2") 
  

tsne_plot +
  ggtitle(paste0("Semantic shifts in the word '",
                 target_feature,
  "' indicated by word embeddings\ntrained on SAA conference abstracts aggregated into ", 
                 year_interval, 
                 " year groupings ")) +
  labs(caption  = "Data and code online at https://github.com/benmarwick/saa-meeting-abstracts")

}


