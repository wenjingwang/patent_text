library(stringr)
ll <- list()
total <- 0
g_cite <- g_text$cited_patent
for(idx in 1:length(g_cite)){
  if(is.na(g_cite[idx])){
    ll[idx] <- NULL
  }else{
    line <- as.character(g_cite[idx])
    items <- str_split(line, ";")[[1]]
    total <- total + length(items)
    ll[[idx]]<- items
  }
}
lld <- unlist(ll)
lld <- unique(lld)

