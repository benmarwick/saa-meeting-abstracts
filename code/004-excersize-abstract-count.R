library(ggplot2)
library(tidyverse)

abs <- readxl::read_excel(here::here("data", "raw-data", "saa-abstracts-tally.xlsx"))

year_with_abs <-
  abs %>% 
  filter(!is.na(`number_of_abstracts`))

ggplot(year_with_abs,
       aes(x = year,
           y = `number_of_abstracts`)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(from =1962, to = 2020, by =2)) +
  theme(axis.text.x = element_text(angle = 90))
