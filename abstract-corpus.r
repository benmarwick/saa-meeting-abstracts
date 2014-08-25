library(tm)
#library(topicmodels)


cwd <- getwd()
path <- paste(cwd, "data", sep="/")

ds <- DirSource(path)
abs_corpus <- VCorpus(ds, readerControl = list(reader=readPDF()))

corpus_nostop <- tm_map(abs_corpus, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(corpus_nostop)
