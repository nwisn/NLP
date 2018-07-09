install.packages("gutenbergr")

require(dplyr)
require(gutenbergr)
require(quanteda)

index <- gutenberg_works(author %in% c("Plato", 
                                       "Aristotle", 
                                       "Pike, Albert", 
                                       "Mackey, Albert Gallatin", 
                                       "Hume, David", 
                                       "Rand, Ayn", 
                                       "Spinoza, Benedictus de", 
                                       "Jefferson, Thomas",
                                       "Smith, Adam",
                                       "Locke, John",
                                       "Einstein, Albert",
                                       "Mill, John Stuart",
                                       "Kant, Immanuel",
                                       "Bentham, Jeremy",
                                       "Nietzsche, Friedrich Wilhelm",
                                       "Marx, Karl",
                                       "Lenin, Vladimir Il'ich") )

big_index <- gutenberg_works()
index.bible <- big_index[grep("The Bible, King James version, Book", big_index$title),]
index.bible2 <- big_index[grep("The World English Bible", big_index$title),]

index.koran <- big_index[grep("Translations of The Koran", big_index$title),]
index.gita <- big_index[grep("Bhagavad", big_index$title),]
                           
big_index[grep("Gospel", big_index$title),] %>% View()
big_index[grep("Bible", big_index$title),]%>% View()

full.index <- rbind(index, index.bible, index.koran, index.gita)

table(index$author)

bible <- gutenberg_download(index.bible$gutenberg_id, meta_fields = c("title", "author"))
save(bible, file="King_James.RData")

bible <- gutenberg_download(index.bible2$gutenberg_id, meta_fields = c("title", "author"))
save(bible, file="WEB_Bible.RData")

books <- gutenberg_download(full.index$gutenberg_id, meta_fields = c("title", "author"))
save(books, file="gutenberg.RData")

unique(books$gutenberg_id)

books <- bible

for (id in unique(books$gutenberg_id)){
  meta <- gutenberg_works(gutenberg_id == id)
  filename <- paste0(id, "___", meta$title, "___", meta$author)
  print(filename)
  txt <- do.call(paste, c(books[books$gutenberg_id == id, "text"], collapse=" "))
  fileConn <- file(paste0("World English Bible/", filename, ".txt"))
  writeLines(txt, fileConn)
  close(fileConn)
}


# ________________________________________________________________________________________________

# random sample
select_ids <- sort(as.numeric(sapply(strsplit(list.files("King James Bible"), "___"), function(x) x[1])))
load("King_James.RData")
books <- bible

for (id in select_ids){
  meta <- gutenberg_works(gutenberg_id == id)
  filename <- paste0(id, "___", meta$title, "___", meta$author)
  print(filename)
  txt <- do.call(paste, c(books[books$gutenberg_id == id, "text"], collapse=" "))
  this_tokens <- tokens(txt, what = "word", remove_numbers = TRUE,  remove_punct = TRUE, remove_separators = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE)
  this_tokens_filtered <- tolower(this_tokens[[1]][sapply(this_tokens[[1]], nchar) > 3])
  this_tokens_random_sample <- sample(this_tokens_filtered, 5000, replace = T)
  fileConn <- file(paste0("King James Bible 5000/", filename, ".txt"))
  writeLines(this_tokens_random_sample, fileConn)
  close(fileConn)
}









