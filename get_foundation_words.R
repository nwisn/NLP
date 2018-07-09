require(tm)

path <- file.path("/home", "ubuntu", "volume", "NLP", "foundation_stems_updated")
filelist <- list.files(path, pattern = ".*.txt")
datalist <- lapply(paste0(path, "/", filelist), function(x) readChar(x, file.info(x)$size))

require(stringr)
datalist <- lapply(lapply(datalist, function(x) strsplit(x, ",")), function(x) trimws(x[[1]], which="both"))
datalist <- lapply(datalist, function(x) gsub("\001", "", x))



require(wordnet)
expandnyms <- function(word){
  this_word <- word
  this_word <- "safely"
  filter <- getTermFilter("ExactMatchFilter", this_word, TRUE)
  terms <- lapply(c("ADJECTIVE", "ADVERB", "NOUN", "VERB"), function(x) getIndexTerms(x, 10, filter))
  synsets <- lapply(terms, function(x) if(!is.null(x)) getSynsets(x[[1]]))
  related <- lapply(synsets, function(x) if(!is.null(x)) getRelatedSynsets(x[[1]], "!"))
  antonyms <- unique(unlist(lapply(related, function(y) if(!is.null(y)) sapply(y, getWord))))
  synonyms <- unique(unlist(lapply(terms, function(x) if(!is.null(x)) getSynonyms(x[[1]]))))
  return(unique(c(synonyms, antonyms)))
}

expanded <- lapply(datalist, function(taste) sapply(taste, function(x) {
  print(x)
  result = tryCatch({
    expandnyms(x)
  }, warning = function(w) {
    print("warning")
  }, error = function(e) {
    print("error")
  }, finally = {
    print("finally")
  })
  })
  )


expanded.words <- lapply(expanded, function(x) unique(as.character(unlist(x))))
expanded.words.clean <- lapply(lapply(expanded.words, function(x) gsub("[(]a[])]", "", x)), function(y) gsub("[(]p[)]", "", y))



original <- lapply(datalist, function(x) unique(x))
words <- expanded.words.clean
require(textstem)
lemmas <- lapply(expanded.words.clean, function(x) unique(lemmatize_words(x)))
stems <- lapply(lemmas, function(y) unique(stem_strings(y)))


setnames <- gsub("[.]txt", "", filelist)
names(words) <- names(lemmas) <- names(stems) <- names(original) <- setnames

save(original, words, lemmas, stems, file="expanded_words.RData")
