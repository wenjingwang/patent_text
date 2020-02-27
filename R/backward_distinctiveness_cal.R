# dates: in order;
# cdist: distance matrix with lower triangle
backwards_cal <- function(dates, cdist, gap){
  backwards <- rep(0, ncol(cdist))
  n <- length(dates)
  for(j in 2:n){
    print(j)
    backwards[j] <- min(cdist[j,(j-1):1])
    for(k in (j - 1):1){
      days <- (dates[j]-dates[k])/86400
      if(days > 365*gap){
        backwards[j] <- min(cdist[j, (j-1):(k+1)])
        print('cut')
        break
      }
    }
  }
  return(backwards)
}


