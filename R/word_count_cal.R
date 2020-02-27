#split_doc: A list of documents in words
Rcpp::sourceCpp("/Users/jiangkunzhao/Downloads/src/keyword_count.cpp")
word_count_cal <- function(split_doc){
  kwc <- new(KeywordCount)
  kwc$addDocs(split_doc)
  word_count <- kwc$getKeywords()
  return(word_count)
}

