## descriptive statistics
# impact forward1
m1 <- c(mean(forwards3), sd(forwards3), quantile(forwards3, 
                                                 c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)))
m2 <- c(mean(forwards5), sd(forwards5), quantile(forwards5, 
                                                 c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)))
m3 <- c(mean(forwards10), sd(forwards10), quantile(forwards10, 
                                                   c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)))
m4 <- c(mean(backwards_all[forwards3>0]/forwards3[forwards3>0]), 
        sd(backwards_all[forwards3>0]/forwards3[forwards3>0]), 
        quantile(backwards_all[forwards3>0]/forwards3[forwards3>0], 
                 c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)) )
m5 <- c(mean(backwards_all[forwards5>0]/forwards5[forwards5>0]), 
        sd(backwards_all[forwards5>0]/forwards5[forwards5>0]), 
        quantile(backwards_all[forwards5>0]/forwards5[forwards5>0], 
                 c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)) )

m5 <- c(mean(backwards_all[forwards10>0]/forwards10[forwards10>0]), 
        sd(backwards_all[forwards10>0]/forwards10[forwards10>0]), 
        quantile(backwards_all[forwards10>0]/forwards5[forwards10>0], 
                 c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)) )
library(dplyr)
citation_structure <- citation_structure %>%
  filter(three != 'NA')
nrow(citation_structure)
m6 <- c(mean(citation_structure$three), sd(citation_structure$three), 
        quantile(citation_structure$three, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)))

m7 <- c(mean(citation_structure$five), sd(citation_structure$five), 
        quantile(citation_structure$five, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)))
m8 <- c(mean(citation_structure$ten), sd(citation_structure$ten), 
        quantile(citation_structure$ten, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)))

innojoy_value <- c(mean(as.numeric(as.character(g_text$value_all))), 
                   sd(as.numeric(as.character(g_text$value_all))), 
                   quantile(as.numeric(as.character(g_text$value_all)), c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)))

datas <- list(m1, m2, m3, m4, m5, m6, m7, m8, innojoy_value)
write.table(datas, file="/Users/jiangkunzhao/wenjing/patent_text/datas.csv")
## load citation structure data ##
library(readxl)
path <- "/Users/jiangkunzhao/Downloads/citation_data.xlsx"
citation_structure <- read_xlsx(path, 1)
g_text <-  g_text[ , !(names(g_text) %in% c("id", "V1"))]
head(citation_structure)
application_year <- substr(g_text$new_date, 1, 4)
grant_year <- substr(g_text$public_date.x, 1, 4)
g_text_model <- g_text %>%
  cbind(citation_structure, backwards_all, forwards1, forwards3, forwards5, forwards7, 
        forwards10, application_year, grant_year, applicant_clear, applicant_clear_num)
g_text_model_df1 <- g_text_model %>%
  filter(one != 'NA') %>%
  filter(forwards1 != 0) %>%
  filter(backwards_all != 0)
g_text_model_df3 <- g_text_model %>%
  filter(one != 'NA') %>%
  filter(forwards3 != 0) %>%
  filter(backwards_all != 0)
g_text_model_df5 <- g_text_model %>%
  filter(one != 'NA') %>%
  filter(forwards5 != 0) %>%
  filter(backwards_all != 0)
g_text_model_df7 <- g_text_model %>%
  filter(one != 'NA') %>%
  filter(forwards7 != 0) %>%
  filter(backwards_all != 0)
g_text_model_df10 <- g_text_model %>%
  filter(one != 'NA') %>%
  filter(forwards10 != 0) %>%
  filter(backwards_all != 0)
## regression model ##
## model 

## model 3
g_text_model_df3$y <- log(1 + as.numeric(g_text_model_df3$three))
g_text_model_df3$patent_value <- log(g_panel_model3$backwards_all/g_panel_model3$forwards3)
g_text_model_df3$ipc <- substr(g_panel_model3$patent_ipc, 1, 4)
m2_1 <- lm(y ~ patent_value + 1, data = g_text_model_df3)
summary(m2_1)
m2_2 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 1, 
           data = g_text_model_df3)
summary(m2_2)
m2_3 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 
               factor(ipc) + 1, data = g_text_model_df3)
summary(m2_3)          
m2_4 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 
             factor(ipc) + factor(applicant_clear_num) + 1, data = g_text_model_df3)
summary(m2_4)
print(c(summary(m2_4)$coefficients[2], summary(m2_4)$coefficients[,"Std. Error"][2]))
m2_5 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 
             factor(ipc) + factor(applicant_clear_num) + grant_year * applicant_clear_num + 1, 
           data = g_text_model_df3)
summary(m2_5)
print(c(summary(m2_5)$coefficients[2], summary(m2_5)$coefficients[,"Std. Error"][2],
        summary(m2_5)$coefficients[,"Pr(>|t|)"][2]))
## model 5
g_text_model_df5$y <- log(1 + as.numeric(g_text_model_df5$five))
g_text_model_df5$patent_value <- log(g_text_model_df5$backwards_all/g_text_model_df5$forwards3)
g_text_model_df5$ipc <- substr(g_text_model_df5$patent_ipc, 1, 5)
m3_1 <- lm(y ~ patent_value + 1, data = g_text_model_df5)
summary(m3_1) 
m3_2 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 1, 
           data = g_text_model_df5)
summary(m3_2)
m3_3 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + factor(ipc) + 1,
           data = g_text_model_df5)
summary(m3_3)
print(c(summary(m3_3)$coefficients[2], summary(m3_3)$coefficients[,"Std. Error"][2],
        summary(m3_3)$coefficients[,"Pr(>|t|)"][2]))
m3_4 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 
             factor(ipc) + factor(applicant_clear_num) + 1, data = g_text_model_df5)
summary(m3_4)
print(c(summary(m3_4)$coefficients[2], summary(m3_4)$coefficients[,"Std. Error"][2],
        summary(m3_4)$coefficients[,"Pr(>|t|)"][2]))
m3_5 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 
             factor(ipc) + factor(applicant_clear_num) + grant_year * applicant_clear_num + 1, 
           data = g_text_model_df5)
summary(m3_5)
print(c(summary(m3_5)$coefficients[2], summary(m3_5)$coefficients[,"Std. Error"][2],
        summary(m3_5)$coefficients[,"Pr(>|t|)"][2]))
## model 7 ##

g_text_model_df7$y <- log(1 + as.numeric(g_text_model_df7$seven))
g_text_model_df7$patent_value <- log(g_text_model_df7$backwards_all/g_text_model_df7$forwards7)
g_text_model_df7$ipc <- substr(g_text_model_df7$patent_ipc, 1, 5)
m4_1 <- lm(y ~ patent_value + 1, data = g_text_model_df7)
summary(m4_1) 
m4_2 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 1, 
           data = g_text_model_df7)
summary(m4_2)
m4_3 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + factor(ipc) + 1,
           data = g_text_model_df7)
summary(m4_3)
print(c(summary(m3_3)$coefficients[2], summary(m3_3)$coefficients[,"Std. Error"][2],
        summary(m3_3)$coefficients[,"Pr(>|t|)"][2]))
m4_4 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 
             factor(ipc) + factor(applicant_clear_num) + 1, data = g_text_model_df7)
summary(m4_4)
print(c(summary(m4_4)$coefficients[2], summary(m4_4)$coefficients[,"Std. Error"][2],
        summary(m4_4)$coefficients[,"Pr(>|t|)"][2]))
m4_5 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 
             factor(ipc) + factor(applicant_clear_num) + grant_year * applicant_clear_num + 1, 
           data = g_text_model_df7)
summary(m4_5)
print(c(summary(m4_5)$coefficients[2], summary(m4_5)$coefficients[,"Std. Error"][2],
        summary(m4_5)$coefficients[,"Pr(>|t|)"][2]))

## model 10 ##

g_text_model_df10$y <- log(1 + as.numeric(g_text_model_df10$ten))
g_text_model_df10$patent_value <- log(g_text_model_df10$backwards/g_text_model_df10$forwards3)
g_text_model_df10$ipc <- substr(g_text_model_df10$patent_ipc, 1, 5)
m5_1 <- lm(y ~ patent_value + 1, data = g_text_model_df10)
summary(m5_1) 
m5_2 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 1, 
           data = g_text_model_df10)
summary(m5_2)
m5_3 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + factor(ipc) + 1,
           data = g_text_model_df10)
summary(m5_3)
print(c(summary(m3_3)$coefficients[2], summary(m3_3)$coefficients[,"Std. Error"][2],
        summary(m3_3)$coefficients[,"Pr(>|t|)"][2]))
m5_4 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 
             factor(ipc) + factor(applicant_clear_num) + 1, data = g_text_model_df10)
summary(m5_4)
print(c(summary(m5_4)$coefficients[2], summary(m5_4)$coefficients[,"Std. Error"][2],
        summary(m5_4)$coefficients[,"Pr(>|t|)"][2]))
m5_5 <- lm(y ~ patent_value + factor(application_year) + factor(grant_year) + 
             factor(ipc) + factor(applicant_clear_num) + grant_year * applicant_clear_num + 1, 
           data = g_text_model_df10)
summary(m5_5)
print(c(summary(m5_5)$coefficients[2], summary(m5_5)$coefficients[,"Std. Error"][2],
        summary(m5_5)$coefficients[,"Pr(>|t|)"][2]))

##
coef1 <- c(summary(m2_1)$coef[2], summary(m2_2)$coef[2], summary(m2_3)$coef[2], summary(m2_4)$coef[2], 
        summary(m2_5)$coef[2])
error1 <- c(summary(m2_1)$coefficients[,"Std. Error"][2], summary(m2_2)$coefficients[,"Std. Error"][2], 
            summary(m2_3)$coefficients[,"Std. Error"][2], summary(m2_4)$coefficients[,"Std. Error"][2], 
            summary(m2_5)$coefficients[,"Std. Error"][2])
r1 <- c(summary(m2_1)$r.square, summary(m2_2)$r.square, summary(m2_3)$r.square, summary(m2_4)$r.square, 
            summary(m2_5)$r.square)

coef2 <- c(summary(m3_1)$coef[2], summary(m3_2)$coef[2], summary(m3_3)$coef[2], summary(m3_4)$coef[2], 
           summary(m3_5)$coef[2])
error2 <- c(summary(m3_1)$coefficients[,"Std. Error"][2], summary(m3_2)$coefficients[,"Std. Error"][2], 
            summary(m3_3)$coefficients[,"Std. Error"][2], summary(m3_4)$coefficients[,"Std. Error"][2], 
            summary(m3_5)$coefficients[,"Std. Error"][2])
r2 <- c(summary(m3_1)$r.square, summary(m3_2)$r.square, summary(m3_3)$r.square, summary(m3_4)$r.square, 
        summary(m3_5)$r.square)

coef3 <- c(summary(m4_1)$coef[2], summary(m4_2)$coef[2], summary(m4_3)$coef[2], summary(m4_4)$coef[2], 
           summary(m4_5)$coef[2])
error3 <- c(summary(m4_1)$coefficients[,"Std. Error"][2], summary(m4_2)$coefficients[,"Std. Error"][2], 
            summary(m4_3)$coefficients[,"Std. Error"][2], summary(m4_4)$coefficients[,"Std. Error"][2], 
            summary(m4_5)$coefficients[,"Std. Error"][2])
r3 <- c(summary(m4_1)$r.square, summary(m4_2)$r.square, summary(m4_3)$r.square, summary(m4_4)$r.square, 
        summary(m4_5)$r.square)

coef4 <- c(summary(m5_1)$coef[2], summary(m5_2)$coef[2], summary(m5_3)$coef[2], summary(m5_4)$coef[2], 
           summary(m5_5)$coef[2])
error4 <- c(summary(m5_1)$coefficients[,"Std. Error"][2], summary(m5_2)$coefficients[,"Std. Error"][2], 
            summary(m5_3)$coefficients[,"Std. Error"][2], summary(m5_4)$coefficients[,"Std. Error"][2], 
            summary(m5_5)$coefficients[,"Std. Error"][2])
r4 <- c(summary(m5_1)$r.square, summary(m5_2)$r.square, summary(m5_3)$r.square, summary(m5_4)$r.square, 
        summary(m5_5)$r.square)

rd <- list(model1 = data.frame(coef1, error1, r1), model2 = data.frame(coef2, error2, r2),
           model3 = data.frame(coef3, error3, r3), model4 = data.frame(coef4, error4, r4))
write.table(rd, file = "/Users/jiangkunzhao/wenjing/patent_text/rd.csv")


# regression model 2 #
y <- log(1 + as.numeric(g_panel_model1$one))
forward_distinctiveness <- log(g_panel_model1$forwards1)
backward_distinctiveness <- log(g_panel_model1$backwards)
ipc <- substr(g_panel_model1$patent_ipc, 1, 5)
s1_1 <- lm(y ~ forward_distinctiveness + backward_distinctiveness + 1, data = g_panel_model1)
summary(s1_1)     
s1_2 <- lm(y ~ forward_distinctiveness + backward_distinctiveness + factor(application_year) +
             factor(grant_year) + 1, data = g_panel_model1)
summary(s1_2)$coefficients[,"Pr(>|t|)"][1:3]
s1_3 <- lm(y ~ forward_distinctiveness + backward_distinctiveness + factor(application_year) +
             factor(grant_year) + factor(ipc) + 1, data = g_panel_model1)
summary(s1_3)$coefficients[,"Pr(>|t|)"][1:3]
s1_4 <- lm(y ~ forward_distinctiveness + backward_distinctiveness + factor(application_year) +
             factor(grant_year) + factor(applicant_clear) + 1, data = g_panel_model1)
summary(s1_4)$coefficients[,"Pr(>|t|)"][1:3]

s1_5 <- lm(y ~ forward_distinctiveness + backward_distinctiveness + factor(application_year) +
             factor(grant_year) + factor(applicant_clear) + 
             grant_year * applicant_clear + 1, data = g_panel_model1)
summary(s1_5)$coefficients[,"Pr(>|t|)"][1:3]
##
y <- log(1 + as.numeric(g_panel_model2$five))
forward_distinctiveness <- log(g_panel_model2$forwards2)
backward_distinctiveness <- log(g_panel_model2$backwards)
ipc <- substr(g_panel_model2$patent_ipc, 1, 5)
s2_1 <- lm(y ~forward_distinctiveness + backward_distinctiveness + 1, 
           data = g_panel_model2)
summary(s2_1)$coefficients[,"Pr(>|t|)"][1:3]        
s2_2 <- lm(y ~forward_distinctiveness + backward_distinctiveness +
             factor(application_year) + factor(grant_year)  + 1, 
           data = g_panel_model2)
summary(s2_2)$coefficients[,"Pr(>|t|)"][1:3]        
s2_3 <- lm(y ~forward_distinctiveness + backward_distinctiveness +
               factor(application_year) + factor(grant_year) + factor(ipc) + 1, 
             data = g_panel_model2)
s2_4 <- lm(y ~forward_distinctiveness + backward_distinctiveness +
             factor(application_year) + factor(grant_year) + factor(ipc) + factor(applicant_clear) + 1, 
           data = g_panel_model2)
summary(s2_4)$coefficients[,"Pr(>|t|)"][1:3] 

s2_5 <- lm(y ~forward_distinctiveness + backward_distinctiveness +
             factor(application_year) + factor(grant_year) + factor(ipc) + factor(applicant_clear) + 
             grant_year * applicant_clear + 1, 
           data = g_panel_model2)
summary(s2_5)$coefficients[,"Pr(>|t|)"][1:3]  
##
y <- log(1 + as.numeric(g_panel_model3$ten))
forward_distinctiveness <- log(g_panel_model3$forwards3)
backward_distinctiveness <- log(g_panel_model3$backwards)
ipc <- substr(g_panel_model3$patent_ipc, 1, 5)
s3_1 <- lm(y ~ forward_distinctiveness + backward_distinctiveness  + 1, 
           data = g_panel_model3)
summary(s3_1)$coefficients[1:3]
summary(s3_1)$coefficients[,"Pr(>|t|)"][1:3] 
s3_2 <- lm(y ~ forward_distinctiveness + backward_distinctiveness +
             factor(application_year) + factor(grant_year)  + 1, 
           data = g_panel_model3)
summary(s3_2)$coefficients[1:3]
summary(s3_2)$coefficients[,"Pr(>|t|)"][1:3] 
s3_3 <- lm(y ~ forward_distinctiveness + backward_distinctiveness +
               factor(application_year) + factor(grant_year) + factor(ipc) + 1, 
             data = g_panel_model3)
summary(s3_3)$coefficients[1:3]
summary(s3_3)$coefficients[,"Pr(>|t|)"][1:3] 
s3_4 <- lm(y ~ forward_distinctiveness + backward_distinctiveness +
             factor(application_year) + factor(grant_year) + factor(ipc) + factor(applicant_clear) +1, 
           data = g_panel_model3)
summary(s3_4)$coefficients[1:3]
summary(s3_4)$coefficients[,"Pr(>|t|)"][1:3] 

s3_5 <- lm(y ~ forward_distinctiveness + backward_distinctiveness +
             factor(application_year) + factor(grant_year) + factor(ipc) + factor(applicant_clear) +
             grant_year * applicant_clear + 1, 
           data = g_panel_model3)
summary(s3_4)$coefficients[1:3]
summary(s3_4)$coefficients[,"Pr(>|t|)"][1:3] 

## Distribution of patent similarity score ##
# panel 1 #
bm <- round(mean(backwards), 2)       
bs <- round(sd(backwards), 2)
bq <- round(quantile(backwards, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)
# panel 2 #
f1m <- round(mean(forwards1), 2)
f1s <- round(sd(forwards1), 2)
f1q <- round(quantile(forwards1, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)

f2m <- round(mean(forwards2[forwards2 != 0]), 2)
f2s <- round(sd(forwards2[forwards2 != 0]), 2)
f2q <- round(quantile(forwards2[forwards2 != 0], c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)

f3m <- round(mean(forwards3[forwards3 != 0]), 2)
f3s <- round(sd(forwards3[forwards3 != 0]), 2)
f3q <- round(quantile(forwards3[forwards3 != 0], c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)

# panel 3 #

value1 <- backwards/forwards1
value2 <- backwards[which(forwards2 != 0)]/forwards2[forwards2 != 0]
value3 <- backwards[which(forwards3 != 0)]/forwards3[forwards3 != 0]

v1m <- round(mean(value1), 2)
v1s <- round(sd(value1), 2)
v1q <- round(quantile(value1, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)

v2m <- round(mean(value2), 2)
v2s <- round(sd(value2), 2)
v2q <- round(quantile(value2, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)

v3m <- round(mean(value3), 2)
v3s <- round(sd(value3), 2)
v3q <- round(quantile(value3, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)

# panel 4 #

cite1 <- as.numeric(citation_structure$one)
cite2 <- as.numeric(citation_structure$five)
cite3 <- as.numeric(citation_structure$ten)

cite1m <- round(mean(cite1), 2)
cite1s <- round(sd(cite1), 2)
cite1q <- round(quantile(cite1, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)

cite2m <- round(mean(cite2), 2)
cite2s <- round(sd(cite2), 2)
cite2q <- round(quantile(cite2, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)

cite3m <- round(mean(cite3), 2)
cite3s <- round(sd(cite3), 2)
cite3q <- round(quantile(cite3, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)), 2)
#####

applicants <- unique(g_text$applicant)
#write.table(applicants, file = '/Users/jiangkunzhao/Downloads/applicants.csv')


