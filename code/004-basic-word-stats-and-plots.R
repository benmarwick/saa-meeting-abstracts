

# number of abstracts

# split the text-pdf by [num]
text_pdf <- 
  readxl::read_excel(here::here("data/raw-data/saa-abstracts-tally.xlsx")) %>% 
  filter(image_or_text == "text") %>% 
  pull(year)
  

all_txts_text_pdf <- all_txts[names(all_txts) %in% text_pdf]
all_txts_text_pdf_tally <- vector("list", length = length(all_txts_text_pdf))


for(i in seq_len(length(all_txts_text_pdf))){
  
  x1 <- all_txts_text_pdf[i]
  x2 <- unlist(str_split(x1, "\\[\\d+\\]"))
  x3 <- x2[!str_detect(x2, "^ see")]  # because many items are a cross-reference to another
  all_txts_text_pdf_tally[[i]] <- length(x3)
  
}

number_of_abstracts_per_year <- 
tibble(year = parse_number(names(all_txts_text_pdf)),
       number_of_abstracts = unlist(all_txts_text_pdf_tally))

# also get count of abstracts from image scans also
number_of_abstracts_per_year_all_years <- 
  readxl::read_excel(here::here("data/raw-data/saa-abstracts-tally.xlsx")) %>% 
  filter(image_or_text == "image") %>% 
  select(year, number_of_abstracts) %>% 
  bind_rows(number_of_abstracts_per_year)

# plot abstracts for all years
plot_all_abstracts <- 
ggplot(number_of_abstracts_per_year_all_years,
       aes(year, 
           number_of_abstracts)) +
  geom_col() +
  scale_x_continuous(labels = c(seq(1960, 2020, 2)), 
                     breaks = seq(1960, 2020, 2),
                     name = "Year") +
  scale_y_continuous(name = "Number of abstracts per year",
                     labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) 

library(patchwork)
plot_all_words / plot_all_abstracts

ggsave(here::here("figures/time-series-count-abstracts-and-words.png"))



