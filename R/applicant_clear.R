library(stringdist)
d <- stringdistmatrix(applicants)
d_mat <- as.matrix(d)
d_mat[upper.tri(d_mat, diag = FALSE)] <- 100
diag(d_mat) <- 100
nrow(d_mat)
similars <- list()
for(i in 1:ncol(d_mat)){
  original <- applicants[i]
  similars[[i]] <- applicants[which(d_mat[i, ] < 3)]
  print(original)
  print(similars[[i]])
}
##
library(readxl)
path <- "/Users/jiangkunzhao/Downloads/applicant_clear.csv"
applicant_data <- read.csv(path, stringsAsFactors = TRUE, header = FALSE)
applicant_clear <- applicant_data$V1
applicant_clear_num <- as.integer(factor(applicant_clear))


