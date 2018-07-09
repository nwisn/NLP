require(tm)
require(textstem)
require(Rgraphviz)
require(ggplot2)
require(wordcloud)

# define corpus path
cname <- file.path("/home", "ubuntu", "volume", "NLP", "World English Bible 5000")

# define transformations
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
toString <- content_transformer(function(x, from, to) gsub(from, to, x))

# get
docs <- VCorpus(DirSource(cname))

# Preprocessing
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))  
docs <- tm_map(docs, removeWords, c("the", "and", "thou", "unto", "thee", "also", "therefore", "upon", stopwords("english"))) 
docs <- tm_map(docs, toSpace, "/|@|\\|")
docs <- tm_map(docs, stripWhitespace)

# create matrices
dtm <- DocumentTermMatrix(docs)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
freq[1:20]


docs.stem <- tm_map(docs, content_transformer(lemmatize_strings)) 
docs.stem <- tm_map(docs.stem, content_transformer(stem_strings)) 

#docs <- tm_map(docs, PlainTextDocument)

# create matrices
dtm.stem <- DocumentTermMatrix(docs.stem)
#tdm <- TermDocumentMatrix(docs)
#findFreqTerms(dtm, lowfreq=50)
#findAssocs(dtm, "america", corlimit=0.85)
#sort(tm_term_score(dtm, terms = lemmatize_words(c("make", "america", "great", "again"))), decreasing = T)


# correlations plot
# plot(dtm,
#      terms=findFreqTerms(dtm, lowfreq=50)[1:50],
#      corThreshold=0.8)



# remove sparse terms
dtms <- removeSparseTerms(dtm, .5)

dim(as.matrix(dtm))
dim(as.matrix(dtms))


# frequency
freq <- sort(colSums(as.matrix(dtms)), decreasing=TRUE)
freq[1:20]
# wf <- data.frame(word=names(freq), freq=freq)
# subset(wf, freq>10) %>%
#   ggplot(aes(word, freq)) +
#   geom_bar(stat="identity") +
#   theme(axis.text.x=element_text(angle=45, hjust=1))

# wordcloud
set.seed(137)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))


# tf-idf
dtm_tfidf <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))
freq <- sort(colSums(as.matrix(dtm_tfidf)), decreasing=TRUE)
freq[1:20]

dtms_tfidf <- removeSparseTerms(dtm_tfidf, .7)
freq <- sort(colSums(as.matrix(dtms_tfidf)), decreasing=TRUE)
freq[1:20]


#sort(tm_term_score(dtm_tfidf, terms = c("make", "america", "great", "again")), decreasing = T)
pdf("world_english_bible_wordcloud_dtms_tfidf.pdf", height = 6, width = 6)
set.seed(120)
wordcloud(names(freq), freq, min.freq=freq[100], colors=brewer.pal(6, "Dark2"))
dev.off()

load("expanded_words.RData")
original.lemmastems <- lapply(lapply(lapply(original, lemmatize_words), stem_words), function(x) unique(tolower(x)))
#taste.scores <- lapply(lemmas, function(x) tm_term_score(dtm_tfidf, terms = x))
taste.scores <- lapply(original.lemmastems, function(x) tm_term_score(dtm_tfidf, terms = x))
#taste.scores <- lapply(original, function(x) tm_term_score(dtm_tfidf, terms = x))

taste.scores.df <- as.data.frame(taste.scores)

require(ggbiplot)
titles <- sapply(strsplit(rownames(taste.scores.df), "___"), function(x) x[2])
titles.short <- sapply(strsplit(titles, ": "), function(x) x[2])

authors <- c()
authors[1:5] <- "Pentateuch"
authors[6:39] <- "Old Testament"
authors[40:65] <- "New Testament"
authors[66] <- "All"

pca <- prcomp(taste.scores.df, center = T, scale. = T)
require(ggrepel)
pdf("world_english_bible_biplot.pdf", height = 10, width = 10)
ggbiplot(pca,
         groups = factor(authors),
         ellipse = T,
         alpha = .5,
         main = "World English Bible",
         #labels = titles,
         #labels.size = 2
) + geom_text_repel(aes(label = titles.short, color = factor(authors)), vjust="inward",hjust="inward", size = 2.5) +
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

pdf("world_english_bible_heatmap.pdf", height = 12, width = 5, onefile = F)
pheatmap(taste.scores.df,
         scale = "column", 
         cluster_rows = F,
         labels_row = substr(titles.short, 1, 70),
         color = colorRampPalette(rev(brewer.pal(n = 7, name ="RdBu")))(100),
         annotation_row = annotations_row,
         annotation_colors = annotations_color,
         main = "World English Bible"
)
dev.off()

