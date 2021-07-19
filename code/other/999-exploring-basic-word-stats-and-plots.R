

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


##-------------------------------------------------------------------------

# Words per year

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

# word count checking
all_txts_word_count_per_year <- 
  map_int(all_txts, 
          stringi::stri_count_words) %>% 
  stack %>% 
  as_tibble() %>% 
  rename(words = values,
         year = ind) %>% 
  mutate(year = as.numeric(as.character(year)))

# count all words for each year
all_txts_c <- corpus(all_txts)
all_txts_c_summary <- 
  summary(all_txts_c) %>%   
  mutate(year = parse_number(Text))

# make a dfm
all_txts_c_dtm <- 
  dfm(all_txts_c,
      remove_punct = TRUE)

# plot of all words in each year
plot_all_words <- 
  ggplot(data = all_txts_c_summary, 
         aes(x = year , 
             y = Tokens / 1000, 
             group = 1)) +
  geom_col() +
  scale_x_continuous(labels = c(seq(1940, 2020, 2)), 
                     breaks = seq(1940, 2020, 2),
                     name = "Year") +
  scale_y_continuous(name = "Total word count per year (x 1,000)",
                     labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) 

# Our key words over time

keywords <- 
  c("identity", 
    "power", 
    "awareness", 
    "transformation", 
    "presentation", 
    "cognition")

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
  mutate(prop = n / Tokens ) %>% 
  group_by(keyword) %>% 
  mutate(sum_the_word = sum(n)) %>% 
  mutate(keyword_n = str_c(keyword, " (n = ", sum_the_word, ")"))

# plot of keywords as a proportion of all words per year
ggplot(data = dfm_keywords_tbl_prop, 
       aes(x = year , 
           y = prop)) +
  geom_point(alpha = 0.3) +
  geom_smooth(span = 0.475,
              se = FALSE) +
  facet_wrap( ~ keyword_n, 
              ncol = 1,
              scales = "free_y") +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)), 
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(name = "Proportion of all words in all abstracts per year",
                     labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) 

