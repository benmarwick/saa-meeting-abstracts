# get PDFs of programs from saa.org
# https://www.saa.org/annual-meeting/annual-meeting-archives/program-archives

# get PDFs of abstracts from saa.org
# https://www.saa.org/annual-meeting/annual-meeting-archives/abstract-archives

# There are three pages of lists to archived abstracts:
link_to_list_of_abstract_links <- 
  c("https://www.saa.org/annual-meeting/annual-meeting-archives/abstract-archives",
    "https://www.saa.org/annual-meeting/annual-meeting-archives/abstract-archives/2",
    "https://www.saa.org/annual-meeting/annual-meeting-archives/abstract-archives/3")

library(rvest)
library(tidyverse)

# get names used for each URL to use as file names
abstract_file_names <- 
  map(link_to_list_of_abstract_links, 
  ~.x %>% 
  read_html() %>% 
  html_nodes(".archive-document a") %>% 
  html_text()) %>% 
  unlist()

# the URLs to the PDFs
abstract_file_urls <- 
  map(link_to_list_of_abstract_links, 
  ~.x %>%  
  read_html() %>% 
  html_nodes(".archive-document a") %>%
  html_attr("href")) %>% 
  unlist()

# download all the PDFs from the URLs
map2(abstract_file_urls, 
     abstract_file_names,
     ~download.file(.x, 
              destfile = paste0("data/raw-data/saa-abstracts-pdfs/", 
                                .y,
                                ".pdf")))
