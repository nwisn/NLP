require(tm)
require(textstem)
require(Rgraphviz)
require(ggplot2)
require(wordcloud)

# define corpus path
#cname <- file.path("/home", "ubuntu", "volume", "NLP", "Trump")
cname <- file.path("/home", "ubuntu", "volume", "NLP", "gutenberg_sub")

# define transformations
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
toString <- content_transformer(function(x, from, to) gsub(from, to, x))

# get
docs <- VCorpus(DirSource(cname))

# Preprocessing
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))  
#docs <- tm_map(docs, removeWords, c("the", "and", stopwords("english"))) 
#docs <- tm_map(docs, toSpace, "/|@|\\|")
#docs <- tm_map(docs, content_transformer(lemmatize_strings)) 
docs <- tm_map(docs, stripWhitespace)
#docs <- tm_map(docs, PlainTextDocument)

#writeLines(as.character(docs[1])) # Check to see if it worked.

# create matrices
dtm <- DocumentTermMatrix(docs)
#tdm <- TermDocumentMatrix(docs)
#findFreqTerms(dtm, lowfreq=50)
#findAssocs(dtm, "america", corlimit=0.85)
#sort(tm_term_score(dtm, terms = lemmatize_words(c("make", "america", "great", "again"))), decreasing = T)


# correlations plot
plot(dtm,
     terms=findFreqTerms(dtm, lowfreq=50)[1:50],
     corThreshold=0.8)


# remove sparse terms
dtms <- removeSparseTerms(dtm, 0.5)

dim(as.matrix(dtm))
dim(as.matrix(dtms))


# frequency
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf <- data.frame(word=names(freq), freq=freq)
subset(wf, freq>10) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# wordcloud
set.seed(137)
wordcloud(names(freq), freq, min.freq=10000, colors=brewer.pal(6, "Dark2"))


# tf-idf
dtm_tfidf <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))
freq <- sort(colSums(as.matrix(dtm_tfidf)), decreasing=TRUE)
#sort(tm_term_score(dtm_tfidf, terms = c("make", "america", "great", "again")), decreasing = T)
pdf("wordcloud.pdf", height = 6, width = 6)
wordcloud(names(freq), freq, min.freq=.01, colors=brewer.pal(6, "Dark2"))
dev.off()

load("expanded_words.RData")
taste.scores <- lapply(lemmas, function(x) tm_term_score(dtm_tfidf, terms = x))
#taste.scores <- lapply(original, function(x) tm_term_score(dtm_tfidf, terms = x))
taste.scores.df <- as.data.frame(taste.scores)

require(ggbiplot)
titles <- sapply(strsplit(rownames(taste.scores.df), "___"), function(x) x[2])
authors <- sapply(strsplit(rownames(taste.scores.df), "___"), function(x) sapply(strsplit(x[3], "[.]"), function(x) x[1]))
authors[which(titles == "Three Translations of The Koran (Al-Qur'an) side by side")] <- "Mohammad"
titles[grep( "The Song Celestial", titles)] <- "Bhagavad-Gita"
titles[grep( "The Symbolism of Freemasonry", titles)] <- "The Symbolism of Freemasonry"
titles[grep( "The Writings of Thomas Jefferson", titles)] <- "The Writings of Thomas Jefferson - Vol. 6"
titles[grep("Morals and Dogma", titles)] <- "Morals and Dogma"
authors[grep("Morals and Dogma", titles)]

pca <- prcomp(taste.scores.df, center = T, scale. = T)
require(ggrepel)
pdf("biplot.pdf", height = 10, width = 10)
ggbiplot(pca,
         groups = factor(authors),
         ellipse = T,
         alpha = .5
         #labels = titles,
         #labels.size = 2
         ) + geom_text_repel(aes(label = titles, color = factor(authors)), vjust="inward",hjust="inward", size = 2.5) +
  theme_bw()
dev.off()

require(pheatmap)
require(RColorBrewer)
require(WGCNA)
annotations_row <- data.frame(author = authors)
rownames(annotations_row) <- rownames(taste.scores.df)
annotations_color <- list()
colors <- standardColors(length(unique(annotations_row$author)))
annotations_color$author <- colors
names(annotations_color$author) <- as.character(unique(annotations_row$author))

pdf("heatmap.pdf", height = 8, width = 10, onefile = F)
pheatmap(taste.scores.df,
         scale = "column", 
         labels_row = substr(titles, 1, 70),
         color = colorRampPalette(rev(brewer.pal(n = 7, name ="RdBu")))(100),
         annotation_row = annotations_row,
         annotation_colors = annotations_color
         )
dev.off()

