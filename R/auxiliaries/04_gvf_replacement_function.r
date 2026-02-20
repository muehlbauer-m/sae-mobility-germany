################################################################################
##### GVF replacement function
################################################################################

pacman::p_load(tidyverse)

################################################################################

GVF_Replacer <- function(df, howmany){
  
  df <- combined_scaled_lasso
  
  maxvals <- sort(df$vardir_GVF_calibboot,
                  decreasing = TRUE)[1:howmany]
  
  index <- df$vardir_GVF_calibboot %in% maxvals %>% which()
  
  df$vardir_GVF_calibboot_rep <- df$vardir_GVF_calibboot
  df$vardir_GVF_calibboot_rep[index] <- df$vardir_calibboot[index]
  
  list(Index = index,
    vardir_GVF_calibboot_rep = df$vardir_GVF_calibboot_rep)
  
}
  
