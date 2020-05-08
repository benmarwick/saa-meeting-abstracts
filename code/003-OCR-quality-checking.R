
# check to see how badly we are affected by OCR errors in the text we are using
# I made https://github.com/benmarwick/rmgarbage to do this

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

prop_ocr_garbage_output <- vector("list", length = length(all_txts))
names(prop_ocr_garbage_output) <- names(all_txts)

# check for OCR garbage strings in each abstract and compute proportion of 
# garbage strings to total number of words (all word-like strings)
# this takes a few hours to complete!

for(i in seq_len(length(all_txts))){

print(paste0("Now checking OCR in ", names(all_txts)[[i]], "..."))
  
txt_chr_sp <- tolower(strsplit(all_txts[[i]], " |\n")[[1]])
txt_chr_sp <- txt_chr_sp[!txt_chr_sp == ""]

# problem with vowel/consonant ratio
gb <- vapply(txt_chr_sp, rmgarbage::rmgarbage, logical(1))

# get garbage only
gb_t <- gb[gb == TRUE]

# proportion of text that is OCR garbage
prop_ocr_garbage_output[[i]] <- sum(unname(gb_t), na.rm = TRUE) / length(gb)

}

# take a look at the results 
prop_ocr_garbage_output_tbl <- 
  prop_ocr_garbage_output %>% 
  as_tibble_col() %>% 
  unnest(value) %>% 
  mutate(year = as.numeric(names(prop_ocr_garbage_output)),
         `Proportion of text that is garbage` = value)

hist_plot <- 
ggplot(prop_ocr_garbage_output_tbl, 
       aes(x = `Proportion of text that is garbage`)) +
         geom_histogram()  +
  theme_bw(base_size = 10)

time_plot <- 
ggplot(prop_ocr_garbage_output_tbl,
       aes(year, 
           `Proportion of text that is garbage`)) +
  geom_col() +
  theme_minimal()

g1 <-  ggplotGrob(hist_plot)
both_plots <-  
  time_plot + 
  annotation_custom(grob = g1, 
                    xmin=1998, 
                    xmax=2020, 
                    ymin=0.03, 
                    ymax=0.05) 

both_plots +
  ggtitle("OCR text quality for SAA Abstracts")
