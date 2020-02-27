# dates: in order
# csim: similarity matrix with lower triangles
forwards_cal <- function(dates, csim, gap){
  n <- ncol(csim)
  forwards <- rep(0, n)
  for(j in 1:n){
    print(j)
    if(j == n) break
    for(k in (j + 1):n){
      days <- (dates[k]-dates[j])/86400
      if(days > 365*gap){
        forwards[j] <- 1/(sum(csim[(j+1):k, j])+1)
        print('cut')
        break
      }
    }
  }
  return(forwards)
}