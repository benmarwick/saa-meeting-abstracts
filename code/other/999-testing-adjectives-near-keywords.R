
# looking at attitudes towards keywords

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


# get adjectives used around a keyword
kwiks <- map(all_txts_c, ~kwic(.x, "hypothesis",  window = 7))
adjectives <- vector("list", length(kwiks))

library(udpipe)
udmodel <- udpipe_download_model(language = "english")

for(i in seq_len(length(kwiks))){
  
  ki <- kwiks[[i]]
  anno <- data.frame(doc_id = ki$from, 
                     pre = ki$pre, 
                     post = ki$post)
  anno$text <- paste0(anno$pre, anno$post)


  x <- udpipe(anno, udmodel)
  
  adjectives[[i]] <- 
  x %>% 
    filter(upos == "ADJ")
  
}

x <-  map_chr(adjectives, ~paste0(.x$lemma, collapse = " "))
names(x) <- names(all_txts) 

dfm_x <- 
  dfm(x) %>% 
  dfm_trim(min_termfreq = 5, verbose = FALSE)
  


