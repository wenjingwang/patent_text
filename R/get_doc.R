library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(gridExtra)
library(jiebaR)
library(Matrix)
library(stringr)
library(tm)
library(quanteda)
library(stringi)
root_path <- "/Users/jiangkunzhao/wenjing/patent_text/"
path1 <- paste0(root_path, "data/dwent_cite1.xls", sep = "")
path2 <- paste0(root_path, "data/dwent_cite2 2.xls", sep = "")
path3 <- paste0(root_path, "data/dwent_cite3.xls", sep = "")
path4 <- paste0(root_path, "data/dwent_cite4.xls", sep = "")
path5 <- paste0(root_path, "data/dwent_cite5.xls", sep = "")
dwent_data1 <- read_xls(path1, 1)
dwent_data2 <- read_xls(path2, 1)
dwent_data3 <- read_xls(path3, 1)
dwent_data4 <- read_xls(path4, 1)
dwent_data5 <- read_xls(path5, 1)
dwent_data <- rbind(dwent_data1, dwent_data2, dwent_data3, dwent_data4, dwent_data5)
cited_data <- data.frame(public_num = dwent_data$`公开(公告)号`,
                         public_date = dwent_data$`公开(公告)日`,
                         cited_num = dwent_data$被引证数,
                         prior_date = dwent_data$最早优先权日,
                         apply_date = dwent_data$申请日,
                         cited_patent = dwent_data$被引证专利,
                         name = dwent_data$名称,
                         value_tech = dwent_data$技术价值,
                         value_law = dwent_data$法律指数,
                         value_str = dwent_data$战略指数,
                         value_mar = dwent_data$市场指数,
                         value_eco = dwent_data$经济指数,
                         value_all = dwent_data$专利综合价值)
new_date <- rep("0", nrow(cited_data))
for(i in 1:nrow(cited_data)){
  if(is.na(cited_data$prior_date[i]) == FALSE){
    new_date[i] = as.character(cited_data$prior_date[i])
  }else{
    new_date[i] = as.character(cited_data$apply_date[i])
  }
  print(i)
}
cited_data$new_date <- new_date
cited_data <- cited_data %>%
  arrange(desc(cited_num)) %>%
  distinct()
cited_data$id <- 1:nrow(cited_data)
##
path_basic_vars <- paste0(root_path, "rdata/app3_patent_basic_vars.rdata", sep = "")
path_text_vars <- paste0(root_path, "rdata/app3_data.rdata", sep = "")
load(path_basic_vars)
load(path_text_vars)
colnames(app2_text) <- c('public_num', 'full_cn')
app2_text_arr <- left_join(app2_text, patent_basic_vars, by = 'public_num')
g_text <- left_join(cited_data, app2_text_arr, by = 'public_num')
g_text <- g_text %>%
  arrange(new_date) %>%
  distinct(name, applicant, .keep_all = TRUE)