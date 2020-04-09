library(fs)
library(tidyverse)

# get the file names to text files
pdf_file_names <- dir_ls(here::here("data/raw-data/saa-abstracts-pdfs/"))

# metadata about abstract PDFs, done by hand
abstracts_tally <- readxl::read_excel(here::here("data/raw-data/saa-abstracts-tally.xlsx"))

# abstracts that are page images
abstracts_that_are_page_images <- 
abstracts_tally %>% 
  filter(image_or_text == "image") %>% 
  filter( year %in% 1993:1994)

# OCR abstracts that are images

for(i in seq_len(nrow(abstracts_that_are_page_images))){
  
  # select PDF document to work on 
  year <- abstracts_that_are_page_images$year[i]
  page_start <- abstracts_that_are_page_images$abstract_start_page[i]
  page_end <- abstracts_that_are_page_images$abstract_end_page[i]
  
  # create directory to hold results
  wd <- getwd()
  dir_create(paste0(here::here("data/derived-data/abstracts-ocr/"), year, "/pngs"))
  setwd(paste0(here::here("data/derived-data/abstracts-ocr/"), year, "/pngs"))
  
  # convert PDF to PNG
  pdf_convert_files <- 
  pdf_file_names %>% 
    str_subset(as.character(year)) %>% 
    pdftools::pdf_convert(pages = page_start:page_end, 
                          dpi = 600,
                           )

  setwd(wd)
  
}

# split the pages in two, vertically, for those that are two page spreads
library(magick)

abstracts_that_are_page_images_two_page_spread <- 
  abstracts_that_are_page_images %>% 
  filter(single_or_double_page_spread == "double")

abstracts_that_are_page_images_two_page_spread_paths <- 
  paste0(here::here("data/derived-data/abstracts-ocr/"), 
                abstracts_that_are_page_images_two_page_spread$year)

# only work on those directories that actuall exist currently 
abstracts_that_are_page_images_two_page_spread_paths_existing <- 
  abstracts_that_are_page_images_two_page_spread_paths[dir.exists(abstracts_that_are_page_images_two_page_spread_paths)]

# split images of two-page spreads into two images
for(i in seq_len(length(abstracts_that_are_page_images_two_page_spread_paths_existing))){
  
  wd <- getwd()
  pngs_path <- paste0(abstracts_that_are_page_images_two_page_spread_paths_existing[i], "/pngs")
  png_files <- list.files(pngs_path, full.names = TRUE)
  
  dir_create(paste0(abstracts_that_are_page_images_two_page_spread_paths_existing[i], "/pngs_split"))
  setwd(paste0(abstracts_that_are_page_images_two_page_spread_paths_existing[i], "/pngs_split"))
  
  for(j in seq_len(length(png_files))){
    system(paste0("convert -crop 50%x100% +repage ", 
                  shQuote(png_files[j]), 
                  "  ", 
                  paste0(shQuote(basename(png_files[j])), "_%d.png")))
  }

 setwd(wd)
  
}


# Do OCR on PNG of split pages
for(i in seq_len(length(abstracts_that_are_page_images_two_page_spread_paths))){
  
  pngs_split_path <- paste0(abstracts_that_are_page_images_two_page_spread_paths[i], "/pngs_split")
  png_split_files <- list.files(pngs_split_path, full.names = TRUE)
  
  text <- vector("list", length = length(png_split_files))
  for(j in seq_len(length(png_split_files))){
    text[[j]] <- tesseract::ocr(png_split_files[j]) 
  }
  
  year <- str_match_all(abstracts_that_are_page_images_two_page_spread_paths[i], "\\d{4}")[[1]][2,1]
  text_c <- paste0(text, collapse = ", ") # combine into a single element
  dir_create(paste0(abstracts_that_are_page_images_two_page_spread_paths[i], "/txt_from_split_pages"))
  write_lines(text_c, 
              paste0(abstracts_that_are_page_images_two_page_spread_paths[i], 
                     "/txt_from_split_pages/abstract-text-", 
                     year, 
                     ".txt"))

}






# convert PDF to text
library(tidyverse)
txts <- 
  map(pdf_file_names, 
      pdf_text)

# write out to text files
walk2(txts, 
      basename(names(txts)),
      ~write_lines(.x, path = paste0("data/raw-data/saa-abstracts-txts/", .y, ".txt")))


