
# from http://text2vec.org/glove.html

library(text2vec)
text8_file = "~/text8"
if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "~/text8.zip")
  unzip ("~/text8.zip", files = "text8", exdir = "~/")
}
wiki = readLines(text8_file, n = 1, warn = FALSE)

# Create iterator over tokens
tokens = space_tokenizer(wiki)[[1]][1:10000]
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)

vocab = prune_vocabulary(vocab, term_count_min = 5L)

# Use our filtered vocabulary
vectorizer = vocab_vectorizer(vocab)
# use window of 5 for context words
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(rank = 50, x_max = 10)

# Error unused arguments (word_vectors_size = 50, vocabulary = vocab)
glove = GlobalVectors$new(word_vectors_size = 50, 
                          vocabulary = vocab, 
                          x_max = 10)

wv_main = glove$fit_transform(tcm, n_iter = 10, 
                              convergence_tol = 0.01, 
                              n_threads = 8)




wv_context = glove$components
word_vectors = wv_main + t(wv_context)
