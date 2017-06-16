library(tm)
library(SnowballC)
library(wordcloud)
library(memoise)


## https://www.r-bloggers.com/r-and-foreign-characters/

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(text) {
  ## Careful not to let just any name slip in here; a
  ## malicious user could manipulate this value.
  text <- iconv(enc2utf8(text),sub="byte")
  vsource <- VectorSource(text)
  myCorpus <- Corpus(vsource, readerControl = list(reader = reader(vsource), language = "fr"))
  ## Exception if not lazy = TRUE
  ## http://stackoverflow.com/questions/18287981/tm-map-has-parallelmclapply-error-in-r-3-0-1-on-mac
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeWords,
                     c(stopwords("SMART"), "le", "la", "de", "du", "des", "dans"),lazy = TRUE)
  #myCorpus <- tm_map(myCorpus, stem, lazy = TRUE)

  myDTM <- TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m <- as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
