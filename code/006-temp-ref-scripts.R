

#-----------------------------------------------------------
# read in the abstracts text files
# takes a long time!
read_in_the_abstracts_data <- 
  read_in_the_abstracts_fn()

# save it so we don't have to repeat this each time
saveRDS(read_in_the_abstracts_data, 
        here::here("data/derived-data/read_in_the_abstracts_data.rds"))
#-----------------------------------------------------------

# investigate the target feature a bit

# we can explore these
related_words <- c("theory", "theories", "theoretical")
map(related_words, ~colSums(read_in_the_abstracts_data$all_txts_c_dtm[, .x ]))
#  so use the most common one, eg.:

# model
# theory
# mechanisms
# hypothesis
# explanations
# inferences

# narrative'
# interpretation
# understanding
# agency
# practice

# set the word here that we use in the later functions:
target_feature <-  "theory"
year_interval <-  5

insert_generate_plot_fn <- 
  function(target_feature, 
           year_interval, 
           read_in_the_abstracts_data){

# Now we insert the time-specific token, like "theory_1975_1980"

#-----------------------------------------------------------
# insert time specific token in the dfm
# takes a few moments 
insert_time_specific_token_data <- 
  insert_time_specific_token_fn(read_in_the_abstracts_data$all_txts,
                                target_feature,
                                year_interval)
#-----------------------------------------------------------

# Now we can generate the word embeddings for our target feature:

#-----------------------------------------------------------
# generate word embeddings
# takes a long time!
generate_embedding_matrix_data <- 
  generate_embedding_matrix_fn(insert_time_specific_token_data$all_txts_c_fcm,
                               insert_time_specific_token_data$time_specific_token,
                               n_similar_words = 50)
#-----------------------------------------------------------

# Now we can draw the plot:

#-----------------------------------------------------------
# draw a plot
# pretty fast
temporal_referencing_plot_output <- 
  temporal_referencing_plot_fn(generate_embedding_matrix_data$embedding_matrix,
                               generate_embedding_matrix_data$myTokens,
                               time_specific_token = insert_time_specific_token_data$time_specific_token,
                               target_feature,
                               year_interval,
                               perplexity = 50)
#-----------------------------------------------------------

# Now we can save the plot:

ggsave(here::here(str_glue("figures/tr-plot-target-word-is-{target_feature}-{year_interval}-by-year-intervals.png")))

  }


# Analyze and plot many target features in one action: 

target_words <- 
  c(
 "model"
,"theory"
,"mechanisms"
,"hypothesis"
,"explanations"
,"inference"
,"narrative"
,"interpretation"
,"understanding"
,"agency"
,"practice"
  )


map(target_words, 
    ~insert_generate_plot_fn(.x, 5, read_in_the_abstracts_data))
