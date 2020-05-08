

# Words per year

library(tidyverse)
library("quanteda")

# get all txt files of abstracts
abstract_txt_files <- 
  fs::dir_ls(here::here("data/derived-data/"), 
             recurse = TRUE, 
             regexp = '\\.txt$')

all_txts <- tolower(readtext::readtext(abstract_txt_files))
names(all_txts) <- map_chr(names(all_txts), ~str_match_all(.x, "\\d{4}")[[1]][1,1])

# count all words for each year
all_txts_c <- corpus(all_txts)
all_txts_c_summary <- 
  summary(all_txts_c) %>%   
  mutate(year = parse_number(Text))
  
# make a dfm
all_txts_c_dtm <- 
  dfm(all_txts_c,
      remove_punct = TRUE)

# Explore key words over time

#----- change these words
keywords <-
  c("agency",
    "phenomena",
    "action",
    "affect",
    "inference",
    "mechanism")
#----

dfm_keywords <- 
  dfm_select(all_txts_c_dtm, 
             pattern = keywords, 
             selection = "keep")

dfm_keywords_tbl <- 
  convert(dfm_keywords, to = "data.frame") %>% 
  pivot_longer(-document, 
               names_to = "keyword",
               values_to = "n") %>% 
  mutate(year = parse_number(document))

# compute proportion of all words per year
dfm_keywords_tbl_prop <- 
dfm_keywords_tbl %>% 
  left_join(all_txts_c_summary) %>% 
  mutate(prop = n / Tokens )

# plot of keywords as a proportion of all words per year
ggplot(data = dfm_keywords_tbl_prop, 
       aes(x = year , 
           y = prop)) +
  geom_col() +
  facet_wrap( ~ keyword, 
              ncol = 1, 
             scales = "free_y") +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)), 
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(name = "Proportion of all words per year",
                     labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) 

