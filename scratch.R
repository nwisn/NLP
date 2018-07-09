
cname <- file.path("/home", "ubuntu", "volume", "NLP", "Trump")

require(tm)
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
lemmatizeDocument <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

docs <- VCorpus(DirSource(cname))
summary(docs)
inspect(docs[1])
writeLines(as.character(docs[1]))

# Preprocessing
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)   

require(textstem)
docs <- tm_map(docs, content_transformer(lemmatize_words)) 
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, toSpace, "/|@|\\|")
DocsCopy <- docs

docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)

toString <- content_transformer(function(x, from, to) gsub(from, to, x))
docs <- tm_map(docs, toString, "fake news", "fake_news")
docs <- tm_map(docs, toString, "inner city", "inner-city")
docs <- tm_map(docs, toString, "politically correct", "politically_correct")

docs <- tm_map(docs, PlainTextDocument)

# stemming
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1])) # Check to see if it worked.
docs <- docs_st

# stem completion (maybe doesn't work?)
docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
docs_stc <- tm_map(docs_stc, PlainTextDocument)
writeLines(as.character(docs_stc[1])) # Check to see if it worked.
# docs <- docs_stc


docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)




dtm <- DocumentTermMatrix(docs)   
dtm   
inspect(dtm[1:5, 1:20])

tdm <- TermDocumentMatrix(docs)   
tdm 

freq <- colSums(as.matrix(dtm))   
length(freq)   

ord <- order(freq)   

dtms <- removeSparseTerms(dtm, 0.2) # This makes a matrix that is 20% empty space, maximum.   
dtms

freq <- colSums(as.matrix(dtm))

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  


require(ggplot2)
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p 

findAssocs(dtm, c("country" , "american"), corlimit=0.85) # specifying a correlation limit of 0.85

require(wordcloud)
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)  
