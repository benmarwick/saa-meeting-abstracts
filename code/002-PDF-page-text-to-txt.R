library(fs)
library(tidyverse)

# get the file names to text files
pdf_file_names <- dir_ls(here::here("data/raw-data/saa-abstracts-pdfs/"))

# metadata about abstract PDFs, done by hand
abstracts_tally <- readxl::read_excel(here::here("data/raw-data/saa-abstracts-tally.xlsx"))

# abstracts that are page text
abstracts_that_are_page_text <- 
abstracts_tally %>% 
  filter(image_or_text == "text") %>% 
  filter( year %in% 2004:2020)

# file names of PDFs that are page text
pdf_file_names_searchable_text <- 
  pdf_file_names[grepl(paste0(as.character(abstracts_that_are_page_text$year), collapse  = "|"), 
                 basename(pdf_file_names))]

dir.create(here::here("data/derived-data/abstracts-txt"))

# convert PDF that has searchable text to text file
for(i in seq_len(length(pdf_file_names_searchable_text))){
  
  year <- str_match_all(pdf_file_names_searchable_text[i], "\\d{4}")[[1]][2,1]
  dir.create(paste0(here::here("data/derived-data/abstracts-txt/"), year))
  
  # convert PDF to text
  txts <- pdftools::pdf_text(pdf_file_names_searchable_text[i])
  
  text_c <- paste0(txts, collapse = ", ") # combine into a single element

  # write out to text file
  write_lines(text_c, path = paste0(paste0(here::here("data/derived-data/abstracts-txt/"), 
                                           year, "/",
                                    basename(pdf_file_names_searchable_text[i]), 
                                    ".txt")))
  
}
