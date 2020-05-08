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

# make a dictionary of all words
all_txts_c <- corpus(all_txts)
all_txts_c_toks <- tokens(
  all_txts_c,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = FALSE,
  remove_separators = TRUE,
  split_hyphens = FALSE
)
all_words <- types(all_txts_c_toks)

# split by decade
d_65_75 <- all_txts[as.numeric(names(all_txts)) %in% 1965:1975 ]
d_75_85 <- all_txts[as.numeric(names(all_txts)) %in% 1975:1985 ]
d_85_95 <- all_txts[as.numeric(names(all_txts)) %in% 1985:1995 ]
d_95_05 <- all_txts[as.numeric(names(all_txts)) %in% 1995:2005 ]
d_05_10 <- all_txts[as.numeric(names(all_txts)) %in% 2005:2010 ]
d_10_20 <- all_txts[as.numeric(names(all_txts)) %in% 2010:2020 ]

decades_txt <- list( d_65_75,
                     d_75_85,
                     d_85_95,
                     d_95_05,
                     d_05_10,
                     d_10_20)

# here's a function to create the word vectors for each decade
do_the_embedding <- function(x, w_i = NULL, w_j = NULL) {
  all_txts_c <- corpus(x)
  
  # Now continuing from https://quanteda.io/articles/pkgdown/replication/text2vec.html
  
  all_txts_c_dtm_toks1 <- tokens(
    all_txts_c,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = FALSE,
    remove_separators = TRUE,
    split_hyphens = FALSE
  )
  
  all_txts_c_feats <- dfm(
    all_txts_c_dtm_toks1,
    tolower = TRUE,
    verbose = TRUE,
    # remove = c(stopwords("english")),
    # stem = TRUE,
    remove_punct = TRUE
  )    %>%  
    dfm_match(all_words) %>% 
    dfm_trim(min_termfreq = 3) %>%
    featnames() 
  
  # leave the pads so that non-adjacent words will not become adjacent
  all_txts_c_dtm_toks2 <-
    tokens_select(
      all_txts_c_dtm_toks1,
      all_txts_c_feats,
      min_nchar = 3,
      padding = TRUE
    )
  
  # Construct the feature co-occurrence matrix
  all_txts_c_fcm <- fcm(all_txts_c_dtm_toks2,
                        context = "window",
                        tri = TRUE)
  
  library("text2vec")
  
  glove <- GlobalVectors$new(rank = 50, 
                             x_max = 10,
                             init = list(w_i = w_i, 
                                         b_i = NULL, 
                                         w_j = w_j, 
                                         b_j = NULL))
  all_txts_main <- glove$fit_transform(
    all_txts_c_fcm,
    n_iter = 10,
    convergence_tol = 0.01,
    n_threads = 8
  )
  
  all_txts_context <- glove$components
  word_vectors <- all_txts_main + t(all_txts_context)
  
  w_i <- all_txts_main
  w_j <- all_txts_context
  
  return(list(word_vectors = word_vectors,
              w_i = w_i,  
              w_j = w_j))
  
}

# generate them all, using initialisation from previous year
for(i in seq_len(length(decades_txt))){

  embedd1 <- do_the_embedding(decades_txt[[i]])
  # do we need to set a dictionary at the start to ensure every decade has the same words?
  embedd2 <- do_the_embedding(decades_txt[[i]], w_i = embedd1$w_i, w_j = embedd1$w_j )
  
}

decades_wv <- map(decades_txt, do_the_embedding)

# explore some words
similar_words <- function(x, word){
what <- x[word, , drop = FALSE] 
cos_sim <- textstat_simil(x = as.dfm(x),
                          y = as.dfm(what),
                          method = "cosine")


head(sort(cos_sim[, 1], decreasing = TRUE), 50)
}

similar_words_theory <-  map(decades_wv, ~similar_words(.x, "theory"))

all_similar_words_by_decade <- 
  sort(unique(unlist(map(similar_words_theory, ~names(.x)))))

most_recent_embedd <- 
decades_wv[[length(decades_wv)]][row.names(decades_wv[[length(decades_wv)]]) %in% all_similar_words_by_decade, ]

target_word_coords <- 
  map(decades_wv, ~.x[row.names(.x) %in% all_similar_words_by_decade, ])



# UMAP plot
library(umap)
umap_output = umap(most_recent_embedd)
umap_output_tbl <- 
  tibble(x = umap_output$layout[,1],
         y = umap_output$layout[,2],
         word =  row.names(umap_output$layout)
         )
                                       

ggplot(umap_output_tbl,
       aes(x, y, label = word)) +
  geom_text()

# alignment

library(MCMCpack)
plot_umap <- function(i){
pr <- procrustes(target_word_coords[[i]][1:50,], 
                 target_word_coords[[6]][1:50,], 
                 translation = TRUE)

umap_output = umap(pr$X.new)
umap_output_tbl <- 
  tibble(x = umap_output$layout[,1],
         y = umap_output$layout[,2],
         word =  row.names(umap_output$layout)
  )

return(umap_output_tbl)
}

umaps_tbls <- map(1:5, plot_umap)

ggplot() +
  theme_minimal() +
  geom_text(data = umaps_tbls[[1]], 
            aes(x, y, label = word), 
            colour = viridis::magma(10)[2]) +
  geom_text(data = umaps_tbls[[2]], 
            aes(x, y, label = word), 
            colour = viridis::magma(10)[3]) +
  geom_text(data = umaps_tbls[[3]], 
            aes(x, y, label = word), 
            colour = viridis::magma(10)[4]) 



