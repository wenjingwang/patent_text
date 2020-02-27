##
g1 <- ggplot(cited_data, aes(x = id, y = cited_num)) +
  geom_point() +
  geom_text_repel(data = filter(cited_data, cited_num >= 62), aes(label = public_num))+
  xlab("Patent") +
  ylab("Number of citations") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
data_5 <- tibble(backwards5, forwards5) %>%
  filter(backwards5 != 0) %>%
  filter(forwards5 != 0)
g2 <- ggplot(data_5, aes(x = forwards5, y = backwards5)) +
  geom_point(alpha = 0.1)+
  geom_point(data = filter(data1, backwards > 0.5 & forwards < 0.5), 
             color = 'red', alpha = 0.2) +
  geom_text_repel(data = filter(data1, cites > quantile(cites, 0.999)),
                  aes(label = public_num), color = 'blue')
grid.arrange(g1, g2)


## 5 years of forwards and citations ##
head(g_text_model)
colnames(g_text_model)
g_text_model <- g_text %>%
  cbind(backwards_all, forwards1, forwards3, forwards5, forwards7, forwards10)
data_year <- g_text_model %>%
  select(c(public_num, cited_num, value_tech, value_law, value_str, value_mar, 
           value_eco, value_all, backwards_all, forwards3, forwards5, forwards10, applicant))

data10year <- data_year %>%
  filter(forwards10 > 0) %>%
  filter(backwards_all > 0) %>%
  filter(forwards10 > quantile(forwards10, 0.01)) %>%
  filter(backwards_all < quantile(backwards_all, 0.99))

data5year <- data_year %>%
  filter(forwards5 > 0) %>%
  filter(backwards_all > 0) %>%
  filter(forwards5 > quantile(forwards5, 0.01)) %>%
  filter(backwards_all < quantile(backwards_all, 0.99))


data3year <- data_year %>%
  filter(forwards3 > 0) %>%
  filter(backwards_all > 0) %>%
  filter(forwards3 > quantile(forwards3, 0.01)) %>%
  filter(backwards_all < quantile(backwards_all, 0.99))


library(ggrepel)
ggplot(data10year, aes(x = forwards10, y = backwards_all)) +
  geom_point(alpha = 0.2)+
  geom_point(data = filter(data10year, backwards_all > quantile(backwards_all, 0.9) &
                             forwards10 > quantile(forwards10, 0.1)), 
             color = 'blue', alpha = 0.2) +
  geom_point(data = filter(data10year, forwards10 < quantile(forwards10, 0.1) &
                             backwards_all < quantile(backwards_all, 0.9)),
             color = 'green', alpha = 0.2) +
  geom_point(data = filter(data10year, backwards_all > quantile(backwards_all, 0.9) &
                             forwards10 < quantile(forwards10, 0.1)),
             color = 'red', shape = 17) +
  geom_point(data = filter(data10year, backwards_all > quantile(backwards_all, 0.9) &
                             forwards10 > quantile(forwards10, 0.99)),
             color = 'black', shape = 8) +
  geom_text_repel(data = filter(data10year, backwards_all > quantile(backwards_all, 0.9) &
                                  forwards10 < quantile(forwards10, 0.1)),
                  aes(label = public_num), color = 'red', size = 4) +
  geom_text_repel(data = filter(data10year, backwards_all > quantile(backwards_all, 0.9) &
                                  forwards10 > quantile(forwards10, 0.99)),
                  aes(label = public_num), color = 'black', size = 4) +
  xlab("Looking forward distinctiveness") +
  ylab("Initial distinctiveness") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=25))

data10year$value_all <- as.numeric(as.character(data10year$value_all))
data10year_df <- data10year %>%
  arrange(desc(value_all))
data10year_df$num <- 1:nrow(data10year_df)
ggplot(data10year_df, aes(x = num, y = value_all)) +
  geom_point()+
  geom_text_repel(data = filter(data10year_df, backwards_all > quantile(backwards_all, 0.9) &
                                  forwards10 < quantile(forwards10, 0.1)),
                  aes(label = public_num), color = 'red', size = 4) +
  geom_text_repel(data = filter(data10year_df, backwards_all > quantile(backwards_all, 0.9) &
                                  forwards10 > quantile(forwards10, 0.99)),
                  aes(label = public_num), color = 'black', size = 4) +
  xlab("Patent") +
  ylab("Patent value evaluated by Innojoy") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=25))

data1 <-  filter(data10year, backwards_all > quantile(backwards_all, 0.9) &
                forwards10 < quantile(forwards10, 0.1))

d1 <- data1[, c(1, 13)]
nrow(d1)

data1 <-  filter(data10year, backwards_all > quantile(backwards_all, 0.9) &
                   forwards10 < quantile(forwards10, 0.1))


data5year <- data_year %>%
  filter(forwards5 > 0) %>%
  filter(backwards_all > 0) 

data2 <-  filter(data5year, backwards_all > quantile(backwards_all, 0.9) &
                   forwards5 < quantile(forwards5, 0.1))
d2 <- data2[, c(1, 13)]
nrow(d2)

data3year <- data_year %>%
  filter(forwards3 > 0) %>%
  filter(backwards_all > 0) %>%
  filter(forwards3 > quantile(forwards3, 0.01)) %>%
  filter(backwards_all < quantile(backwards_all, 0.99))

data3 <-  filter(data3year, backwards_all > quantile(backwards_all, 0.9) &
                   forwards3 < quantile(forwards3, 0.1))
d3 <- data3[, c(1, 13)]
nrow(d3)

data_all <- rbind(d1, d2, d3)
write.table(data_all, file = "/Users/jiangkunzhao/wenjing/patent_text/applicant.csv")


dat1 = filter(data3year, backwards_all > quantile(backwards_all, 0.9) &
                forwards3 > quantile(forwards3, 0.99))[, c(1, 13)]

dat2 = filter(data5year, backwards_all > quantile(backwards_all, 0.9) &
                forwards5 > quantile(forwards5, 0.99))[, c(1, 13)]

dat3 = filter(data10year, backwards_all > quantile(backwards_all, 0.9) &
                forwards10 > quantile(forwards10, 0.99))[, c(1, 13)]

dat <- rbind(dat1, dat2, dat3)
write.table(dat, file = "/Users/jiangkunzhao/wenjing/patent_text/one_offs.csv")

