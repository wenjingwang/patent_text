# m: a list of tf-idf weights of each document
library(Matrix)
sim_dist_matrix_cal <- function(m){
  n <- length(m)
  m_mat <- Matrix(unlist(m), nrow = n, byrow = TRUE)
  csim <- m_mat/sqrt(rowSums(m_mat * m_mat))
  csim <- csim %*% t(csim) #csim is sparse
  csim <- as.matrix(csim) # csim is none sparse
  cdist <- 1 - csim
  cdist[upper.tri(cdist, diag = TRUE)] <- 0
  csim[upper.tri(csim, diag = TRUE)] <- 0
  return(list(sim_mat = csim, dist_mat = cdist))
}


