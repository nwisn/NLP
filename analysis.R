require(tm)
require(textstem)
require(wordcloud)

# define corpus path
cname.philosophy <- file.path("/home", "ubuntu", "volume", "NLP", "Philosophy_5000")
cname.bible <- file.path("/home", "ubuntu", "volume", "NLP", "King James Bible 5000")
cname.other <- file.path("/home", "ubuntu", "volume", "NLP", "other 5000")

# get texts
docs.philosophy <- VCorpus(DirSource(cname.philosophy))
docs.bible <- VCorpus(DirSource(cname.bible))
docs.other <- VCorpus(DirSource(cname.other))

# functions
preprocess <- function(docs,
                       remove.words = c("the", "and", "thou", "unto", "thee", "also", "therefore", "upon")){
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  toString <- content_transformer(function(x, from, to) gsub(from, to, x))
  docs <- tm_map(docs, removePunctuation) 
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, content_transformer(tolower))  
  docs <- tm_map(docs, removeWords, c(remove.words, stopwords("english"))) 
  docs <- tm_map(docs, toSpace, "/|@|\\|")
  docs <- tm_map(docs, stripWhitespace)
  return(docs)
}

lemmastem <- function(docs){
  docs.stem <- tm_map(docs, content_transformer(lemmatize_strings)) 
  docs.stem <- tm_map(docs.stem, content_transformer(stem_strings))
  return(docs.stem)
}

DocumentTermMatrices <- function(docs){
  dtm.nnn <- DocumentTermMatrix(docs)
  dtm.nnc <- DocumentTermMatrix(docs, control = list(weighting = function(x) weightSMART(x, spec = "nnc")))
  dtm.ntc <- DocumentTermMatrix(docs, control = list(weighting = function(x) weightSMART(x, spec = "ntc")))
  dtm.ntn <- DocumentTermMatrix(docs, control = list(weighting = function(x) weightSMART(x, spec = "ntn")))
  ret <- list(nnn = dtm.nnn,
              nnc = dtm.nnc,
              ntc = dtm.ntc,
              ntn = dtm.ntn)
  return(ret)
}

make_wordclouds <- function(freq, dirname, ntop = 100, modifier = ""){
  for(ii in 1:length(freq)){
    for(jj in 1:length(freq[[ii]])){
      this_freq <- freq[[ii]][[jj]]
      pdfname <- paste0(dirname, "/", "wordcloud_", names(freq)[ii], "_", names(freq[[ii]])[jj], "_", modifier, ".pdf")
      pdf(pdfname)
      wordcloud(names(this_freq), this_freq, min.freq = this_freq[ntop], colors=brewer.pal(6, "Dark2"))
      dev.off()
    }
  }
}

process_authors <- function(taste.scores.df){
  titles <- sapply(strsplit(rownames(taste.scores.df), "___"), function(x) x[2])
  authors <- sapply(strsplit(rownames(taste.scores.df), "___"), function(x) sapply(strsplit(x[3], "[.]"), function(x) x[1]))
  authors[which(titles == "Three Translations of The Koran (Al-Qur'an) side by side")] <- "Mohammad"
  titles[grep( "Three Translations of The Koran (Al-Qur'an) side by side", titles)] <- "Koran"
  titles[grep( "The Song Celestial", titles)] <- "Bhagavad-Gita"
  titles[grep( "The Symbolism of Freemasonry", titles)] <- "The Symbolism of Freemasonry"
  titles[grep( "The Writings of Thomas Jefferson", titles)] <- "The Writings of Thomas Jefferson - Vol. 6"
  titles[grep("Morals and Dogma", titles)] <- "Morals and Dogma"

  
  if(dirname == "bible"){
    titles <- sapply(strsplit(rownames(taste.scores.df), "___"), function(x) x[2])
    titles <- sapply(strsplit(titles, ": "), function(x) x[2])
    
    authors <- c()
    authors[1:5] <- "Pentateuch"
    authors[6:39] <- "Old Testament"
    authors[40:43] <- "Gospels"
    authors[44:66] <- "New Testament"
  }
  
  ret <- list(titles = titles,
              authors = authors)
  return(ret)
}


make_pca_biplot <- function(taste.scores.df, dirname, modifier, authors, titles){
  require(ggrepel)
  pca <- prcomp(taste.scores.df, center = T, scale. = T)
  pdfname <- paste0(dirname, "/", "biplot_", modifier, ".pdf")
  pdf(pdfname, height = 10, width = 10)
  g <- ggbiplot(pca,
           groups = factor(authors),
           ellipse = T,
           alpha = .5
  ) + geom_text_repel(aes(label = titles, color = factor(authors)), size = 2.5) + theme_bw()
  print(g)
  dev.off()
}


make_heatmap <- function(taste.scores.df, dirname, modifier, authors, titles){
  require(pheatmap)
  require(RColorBrewer)
  require(WGCNA)
  annotations_row <- data.frame(author = authors)
  rownames(annotations_row) <- rownames(taste.scores.df)
  annotations_color <- list()
  colors <- standardColors(length(unique(annotations_row$author)))
  annotations_color$author <- colors
  names(annotations_color$author) <- as.character(unique(annotations_row$author))
  pdfname <- paste0(dirname, "/", "heatmap_", modifier, ".pdf")
  pdf(pdfname, width = 6, height = max(nrow(taste.scores.df)/4,10), onefile = F)
  pheatmap(taste.scores.df,
           scale = "column", 
           labels_row = substr(titles, 1, 70),
           color = colorRampPalette(rev(brewer.pal(n = 7, name ="RdBu")))(100),
           annotation_row = annotations_row,
           annotation_colors = annotations_color
  )
  dev.off()
}













dirname <- "bible"
these_docs <- c(docs.bible)

# preprocess
docs <- preprocess(these_docs)

# compute Document Term Matrices
docs.stem <- lemmastem(docs)
DTM <- list(docs = DocumentTermMatrices(docs),
            docs.stem = DocumentTermMatrices(docs.stem))
DTM.sparse <- lapply(DTM, function(dtm) lapply(dtm, function(x) removeSparseTerms(x, .7)) )

freq <- lapply(DTM, function(dtm) lapply(dtm, function(x) sort(colSums(as.matrix(x)), decreasing = TRUE)))
freq.sparse <- lapply(DTM.sparse, function(dtm) lapply(dtm, function(x) sort(colSums(as.matrix(x)), decreasing = TRUE)))

make_wordclouds(freq, dirname, 150, modifier = "full")
make_wordclouds(freq.sparse, dirname, 150, modifier = "sparse")

load("expanded_words.RData")
original.lemmastems <- stems
original.lemmastems <- lapply(lapply(lapply(original, lemmatize_words), stem_words), function(x) unique(tolower(x)))

taste.scores <- lapply(DTM, function(dtm) lapply(dtm, function(x) as.data.frame(lapply(original.lemmastems, function(taste) tm_term_score(x, terms = taste))) ))
taste.scores.sparse <- lapply(DTM.sparse, function(dtm) lapply(dtm, function(x) as.data.frame(lapply(original.lemmastems, function(taste) tm_term_score(x, terms = taste))) ))

metadata <- process_authors(taste.scores$docs.stem$ntc)

for(this_name in names(taste.scores$docs.stem)){
  print(this_name)
  make_pca_biplot(taste.scores$docs.stem[[this_name]], 
                  dirname = dirname,
                  modifier = paste0("stem_", this_name, "_expanded"),
                  authors = metadata$authors,
                  titles = metadata$titles
  )
  make_heatmap(taste.scores$docs.stem[[this_name]], 
               dirname = dirname,
               modifier = paste0("stem_", this_name, "_expanded"),
               authors = metadata$authors,
               titles = metadata$titles)
}


for(this_name in names(taste.scores.sparse$docs.stem)){
  make_pca_biplot(taste.scores.sparse$docs.stem[[this_name]], 
                  dirname = dirname,
                  modifier = paste0("stem_", this_name, "_sparse_", "expanded"),
                  authors = metadata$authors,
                  titles = metadata$titles
  )
  make_heatmap(taste.scores.sparse$docs.stem[[this_name]], 
               dirname = dirname,
               modifier = paste0("stem_", this_name, "_sparse_", "expanded"),
               authors = metadata$authors,
               titles = metadata$titles)
}




require("topicmodels")
dtm <- DTM$docs$nnn
SEED <- 137
k=5
tm <- list(VEM = LDA(dtm, k = k, control = list(seed = SEED)),
         VEM_fixed = LDA(dtm, k = k,control = list(estimate.alpha = FALSE, seed = SEED)),
         Gibbs = LDA(dtm, k = k, method = "Gibbs",control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
         CTM = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))))

lapply(tm, function(x) terms(x, 15))
lapply(1:k, function(i) names(topics(tm[["Gibbs"]])[topics(tm[["Gibbs"]]) == i]))


# determine best K
many_models <- mclapply(seq(2, 35, by = 1), function(x) {LDA(dtm, x, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000))} )
many_models.logLik <- as.data.frame(as.matrix(lapply(many_models, logLik)))
plot(2:35, unlist(many_models.logLik), xlab="Number of Topics", ylab="Log-Likelihood")



library(LDAvis)
vignette("details", package = "LDAvis")
help(createJSON, package = "LDAvis")
