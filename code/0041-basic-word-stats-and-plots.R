df <- all_txts_c_dtm

dont_care <- 
  c('university', 'abstracts', 'meeting', 
    'annual', 'paper', 'chair', 'across', 
    'presented', 'session', 'college', 'area',
    'discussed', 'this', 'that', 'there', 'which',
    'their', 'poster', 'using', 'through', 'into',
    'some', 'from', 'been', 'discuss', 'while',
    'over')

# from tidytext
df_tbl <- 
  convert(df, to = "data.frame") %>% 
  pivot_longer(-document, 
               names_to = "word",
               values_to = "n") %>% 
  mutate(year = parse_number(document)) %>% 
  filter(word %in% names(data_int_syllables)) %>% 
  filter(str_length(word) > 3) %>% 
  filter(!word %in% dont_care)

df_tbl_yearly_totals <- 
  df_tbl %>% 
  group_by(year) %>% 
  summarise(total_words = sum(n, na.rm = TRUE))
  
  
# compute proportion of all words per year
df_tbl_prop <- 
  df_tbl %>% 
 # left_join(all_txts_c_summary) %>% 
  left_join(df_tbl_yearly_totals) %>% 
  mutate(prop = n / total_words ) %>% 
  filter(prop > 0) %>% 
  group_by(word) %>% 
  mutate(sum_the_word = sum(n)) %>% 
  mutate(keyword_n = str_c(word, " (n = ", sum_the_word, ")"))

# want to find words with the greatest difference in 
# proportion between two time periods 

df_tbl_prop_two_groups <- 
  df_tbl_prop %>% 
  mutate(year_group = ifelse(document < 2004, 
                             "early", 
                             "late"))

df_tbl_prop_two_groups_diff <- 
df_tbl_prop_two_groups %>% 
  group_by(word, year_group) %>% 
  summarise(mean_prop = mean(prop, na.rm = TRUE)) %>% 
  pivot_wider(names_from = year_group,
              values_from = mean_prop) %>% 
  drop_na() %>% 
  mutate(diff = late - early)

df_tbl_prop_two_groups_diff_interesting <- 
df_tbl_prop_two_groups_diff %>% 
  filter(diff >= 0.00001 | diff <= -0.00001)

df_tbl_prop_two_groups_diff_interesting_top <- 
  df_tbl_prop_two_groups_diff_interesting %>% 
  dplyr::arrange(desc(diff)) %>% 
  head(30)

df_tbl_prop_two_groups_diff_interesting_bottom <- 
  df_tbl_prop_two_groups_diff_interesting %>% 
  dplyr::arrange((diff)) %>% 
  head(30)

# check 
hist_of_diffs <- 
ggplot(df_tbl_prop_two_groups_diff_interesting, 
       aes(diff)) +
  geom_histogram() +
  xlim(-0.0001, 0.0001) +
  theme_minimal() +
  xlab("← words used less often ... words used more often →\nin more recent years")
  
library(ggrepel)
library(ggforce)
word_clouds <- 
ggplot() +
  geom_text_repel(data = df_tbl_prop_two_groups_diff_interesting_top,
                  aes(x = diff ,
                      y = 1,
                      label = word,
                      size = diff  ),
                  bg.color = "white", 
                  bg.r = 0.1,
                 max.overlaps = 100,
                 force = 20,
                  segment.color = NA) +
 geom_text_repel(data = df_tbl_prop_two_groups_diff_interesting_bottom,
                aes(x = diff , 
                    y = 1,
                    size = abs(diff)  ,
                    label = word ),
                bg.color = "white", 
                bg.r = 0.1,
                force = 20,
                max.overlaps = 100,
                segment.color = NA) +
  #xlim(-0.001, 0.001) +
 theme_void() +
  guides(size = "none")

word_clouds

# seems like more words decline in use than increase in use over time
# this looks good

hist_of_diffs + 
  annotation_custom(
    ggplotGrob(word_clouds), 
    xmin = -1e-04, 
    xmax = 1e-04, 
    ymin = 10, 
    ymax = 5000
  )

# what about completely new words appearing/disappearing? 
# not very useful
df_tbl_prop_two_groups_wide <- 
df_tbl_prop_two_groups %>% 
  select(word, prop, year_group) %>% 
  pivot_wider(names_from = year_group,
              values_from = prop,
              values_fn = list(prop = mean)) 

df_tbl_prop_two_groups_wide_only_in_late <- 
  df_tbl_prop_two_groups_wide %>% 
  filter(is.na(early )) %>% 
  arrange(desc(late))

df_tbl_prop_two_groups_wide_only_in_early <- 
  df_tbl_prop_two_groups_wide %>% 
  filter(is.na(late )) %>% 
  arrange(desc(early))
              




