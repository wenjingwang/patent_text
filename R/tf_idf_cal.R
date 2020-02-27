# word_count: return dataframe of word_count_cal function
# split_doc: A list of documents in words
# m: A list of tf-idf weights of each words in each document
Rcpp::sourceCpp('/Users/jiangkunzhao/wenjing/patent_text/src/tf_idf_calc.cpp')
tf_idf_cal <- function(word_count, split_doc){
  tfcalc <- new(TfidfCalc)
  n <- nrow(word_count)
  tfcalc$Init(word_count, n)
  m <- tfcalc$calc(split_doc)
  return(m)
}
