# binned scatter plot
library(binsreg)
ipc <- factor(substr(g_text_model_df5$patent_ipc, 1, 4))
w <- matrix(c(g_text_model_df5$grant_year, g_text_model_df5$applicant_clear_num, 
              g_text_model_df5$application_year, ipc), ncol = 4, byrow = FALSE)
y <- log(as.numeric(g_text_model_df5$five)+1)
patent_value <- log(g_text_model_df5$backwards_all/g_text_model_df5$forwards5)
br <- binsreg(y, patent_value, w)
br

ipc <- factor(substr(g_text_model_df10$patent_ipc, 1, 4))
w <- matrix(c(g_text_model_df10$grant_year, g_text_model_df10$applicant_clear_num, 
              g_text_model_df10$application_year, ipc), ncol = 4, byrow = FALSE)
y <- log(as.numeric(g_text_model_df10$ten)+1)
patent_value <- log(g_text_model_df10$backwards_all/g_text_model_df10$forwards10)
br <- binsreg(y, patent_value, w)
br

ipc <- factor(substr(g_text_model_df3$patent_ipc, 1, 4))
w <- matrix(c(g_text_model_df3$grant_year, g_text_model_df3$applicant_clear_num, 
              g_text_model_df3$application_year, ipc), ncol = 4, byrow = FALSE)
y <- log(as.numeric(g_text_model_df3$three)+1)
patent_value <- log(g_text_model_df3$backwards_all/g_text_model_df3$forwards3)
br <- binsreg(y, patent_value, w)
br
