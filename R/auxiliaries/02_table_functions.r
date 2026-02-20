################################################################################
##### Table functions
################################################################################

pacman::p_load(tidyverse, emdi)

################################################################################

Precision_Tables <- function(model, includegamma){
  
#Compute MSE/CV for direct/model based estimates

all_precisions <- emdi:::mse_emdi(object = model, indicator = "all", 
  CV = TRUE)
colnames(all_precisions$ind) <- c("Domain", paste0(c("FH_Direct", 
  "FH_Model"), "_MSE"))
colnames(all_precisions$ind_cv) <- c("Domain", paste0(c("FH_Direct", 
                                                          "FH_Model"), "_CV"))

#MSE dir
df_MSE_dir <- model$MSE$Direct %>%
  summary() %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  `colnames<-`("Direct MSE")

#MSE model
df_MSE_model <- model$MSE$FH %>%
  summary() %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  `colnames<-`("Model MSE")

#CV dir:
df_CV_dir <- all_precisions$ind_cv$FH_Direct_CV %>%
  summary() %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  `colnames<-`("Dir CV")

#CV model
df_CV_model <- all_precisions$ind_cv$FH_Model_CV %>%
  summary() %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  `colnames<-`("Model CV")

if (includegamma) {
  
  #Gamma model
  df_gamma <- model$model$gamma$Gamma %>%
    summary() %>% 
    as.matrix() %>% 
    as.data.frame() %>% 
    `colnames<-`("Gamma") %>% 
    t()
  
  
  df_precisions <- cbind(
                         df_MSE_dir,
                         df_MSE_model,
                         df_CV_dir,
                         df_CV_model) %>% t()
  df_precisions <- rbind(df_gamma, df_precisions)
  
  
}else{
  
  df_precisions <- cbind(df_MSE_dir,
                         df_MSE_model,
                         df_CV_dir,
                         df_CV_model) %>% t()  
  
  
  }


df_precisions

}
