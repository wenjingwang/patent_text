## split doc ##
# text is a vector each element is a document
library(jiebaR)
library(stringr)
library(tm)
library(quanteda)
library(stringi)
engine <- worker(stop_word = "/Users/jiangkunzhao/Downloads/stopwords_merge.txt", "tag")
split_doc_cal <- function(text){
  split_doc <- list()
  corpus <- tm::Corpus(tm::VectorSource(text))
  for(i in 1:length(corpus)){
    corpus_clean <- str_replace_all(corpus[[i]]$content, "[\r\n\t]" , "")
    corpus_clean <- segment(corpus_clean, engine)
    corpus_clean <- str_replace_all(corpus_clean, " " , "")
    corpus_clean <- corpus_clean[nchar(corpus_clean) > 1]
    corpus_clean <- removeNumbers(corpus_clean) 
    corpus_clean <- str_replace_all(corpus_clean, "[-.]" , "")
    corpus_clean <- char_toupper(corpus_clean)
    corpus_clean <- stri_remove_empty(corpus_clean)
    split_doc[[i]] <- corpus_clean
    print(i)
  }
  return(split_doc) # splict_doc is a list
}
