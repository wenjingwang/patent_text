library(dplyr)
library(tidyr)
split_doc <- split_doc_cal(g_text$full_cn)
word_count <- word_count_cal(split_doc)
##
load("/Users/jiangkunzhao/wenjing/patent_text/rdata/csim.rdata")
word_count <- word_count %>%
  arrange(desc(count))
word_count2 <- word_count %>%
    filter(count > 20) %>%
    filter(count < 80000) %>%
    filter(df < 10000) %>%
    filter(df > 10) %>%
    arrange(desc(count))
nrow(word_count2)
# tf-idf
m <- tf_idf_cal(word_count2, split_doc)
## similarity matrix
csim <- sim_dist_matrix_cal(m)[[1]]
cdist <- sim_dist_matrix_cal(m)[[2]]
## backward ##
dates <- as.numeric(as.POSIXct(g_text$new_date, format = "%Y.%m.%d"))
backwards5 <- backwards_cal(dates, cdist, 5)
backwards3 <- backwards_cal(dates, cdist, 3)
backwards10 <- backwards_cal(dates, cdist, 10)
backwards_all <- backwards_cal(dates, cdist, 100)
plot(backwards5)

b_df3 <- tibble(id = 1:length(backwards3), backwards3) %>%
  filter(backwards3 != 0)
b_df5 <- tibble(id = 1:length(backwards3), backwards5) %>%
  filter(backwards5 != 0)
b_df10 <- tibble(id = 1:length(backwards3), backwards10) %>%
  filter(backwards10 != 0)
b_df_all <- tibble(id = 1:length(backwards_all), backwards_all) %>%
  filter(backwards_all != 0)
plot(backwards_all)

ggplot(data = b_df5, aes(x = id, y = backwards5)) +
  geom_point(color = 'red', alpha = 0.5) +
  geom_point(data = b_df3, aes(x = id, y = backwards3), color = 'blue', alpha = 0.3) +
  geom_point(data = b_df_all, aes(x = id, y = backwards_all), alpha = 0.8) +
  geom_hline(yintercept = quantile(b_df_all$backwards_all, 0.05), color = 'red') +
  xlab("Patent") +
  ylab("Initial distinctiveness")+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=25))
## forwards - set time window ##
forwards1 <- forwards_cal(dates, csim, 1) # year 1
forwards3 <- forwards_cal(dates, csim, 3)
forwards5 <- forwards_cal(dates, csim, 5) # year 5
forwards7 <- forwards_cal(dates, csim, 7)
forwards10 <- forwards_cal(dates, csim, 10) # year 10

f_df <- tibble(id = 1:length(forwards10), forwards10) %>%
  filter(forwards10 != 0) 

ggplot(f_df, aes(x = id, y = forwards10)) +
  geom_point(data = dplyr::filter(f_df, forwards10 < quantile(f_df$forwards10, 0.95))) + 
  geom_point(data = dplyr::filter(f_df, forwards10 >= quantile(f_df$forwards10, 0.95)), 
             color = 'red', shape = 17) +
  geom_hline(yintercept = quantile(f_df$forwards10, 0.95), color = 'red') +
  xlab("Patent") +
  ylab("Looking forward of distinctiveness (time window = 10 year)")+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=25))


f_df <- tibble(id = 1:length(forwards3), forwards3) %>%
  filter(forwards3 != 0) 

ggplot(f_df, aes(x = id, y = forwards3)) +
  geom_point(data = dplyr::filter(f_df, forwards3 < quantile(f_df$forwards3, 0.95))) + 
  geom_point(data = dplyr::filter(f_df, forwards3 >= quantile(f_df$forwards3, 0.95)), 
             color = 'red', shape = 17) +
  geom_hline(yintercept = quantile(f_df$forwards3, 0.95), color = 'red') +
  xlab("Patent") +
  ylab("Looking forward of distinctiveness (time window = 3 year)")+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=25))

f_df <- tibble(id = 1:length(forwards5), forwards5) %>%
  filter(forwards5 != 0) 

ggplot(f_df, aes(x = id, y = forwards5)) +
  geom_point(data = dplyr::filter(f_df, forwards5 < quantile(f_df$forwards5, 0.95))) + 
  geom_point(data = dplyr::filter(f_df, forwards5 >= quantile(f_df$forwards5, 0.95)), 
             color = 'red', shape = 17) +
  geom_hline(yintercept = quantile(f_df$forwards5, 0.95), color = 'red') +
  xlab("Patent") +
  ylab("Looking forward of distinctiveness (time window = 5 year)")+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=25))


### patent value ###
pv1 <- backwards_all[forwards3 > 0]/forwards3[forwards3 > 0]
pv2 <- backwards_all[forwards5 > 0]/forwards5[forwards5 > 0]
pv3 <- backwards_all[forwards10 > 0]/forwards10[forwards10 > 0]

pv_df1 <- tibble(id = 1:length(pv1), pv1)
pv_df2 <- tibble(id = 1:length(pv2), pv2)  
pv_df3 <- tibble(id = 1:length(pv3), pv3) 
ggplot(pv_df1, aes(x = id, y = pv1)) +
  geom_line() +
  geom_line(data = pv_df2, aes(x = id, y = pv2), color = 'red', alpha = 0.5) +
  geom_line(data = pv_df3, aes(x = id, y = pv3), color = 'blue', alpha = 0.2)+
  xlab('Patent') +
  ylab('Initial distinctiveness / looking forward distinctiveness') +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=25))
  

load("/Users/jiangkunzhao/wenjing/patent_text/rdata/word_count.rdata")
load("/Users/jiangkunzhao/wenjing/patent_text/rdata/g_text.rdata")

## keywords changing ##
g_phrase1 <- g_text %>%
  filter(new_date < "2003.12.31")
g_phrase2 <- g_text %>%
  filter(new_date < '2008.12.31' & new_date >= '2004.1.1')
g_phrase3 <- g_text %>%
  filter(new_date < '2020.12.31' & new_date >= '2009.1.1')
## look deep into the terms ##
load("/Users/jiangkunzhao/wenjing/patent_text/rdata/m.rdata")
m1 <- m[1:nrow(g_phrase1)]
m2 <- m[1:nrow(g_phrase2)]
m3 <- m[1:nrow(g_phrase3)]
m1_mat <- matrix(unlist(m1), nrow = nrow(g_phrase1), byrow = TRUE)
m2_mat <- matrix(unlist(m2), nrow = nrow(g_phrase2), byrow = TRUE)
m3_mat <- matrix(unlist(m3), nrow = nrow(g_phrase3), byrow = TRUE)

word1 <- apply(m1_mat, 2, sum)
word2 <- apply(m2_mat, 2, sum)
word3 <- apply(m3_mat, 2, sum)

d1 <- word_count2[which(word1 != 0), ]
nrow(d1)

View(d1)


d2 <- as.character(word_count2$key[which(word1 == 0 & word2 != 0)])
length(d2)
d3 <- as.character(word_count2$key[which(word3 != 0 & word2 == 0 & word1 == 0)])
length(d3)
write.table(d3, file = "/Users/jiangkunzhao/wenjing/patent_text/data/d3.csv")




