################################################################################
##### Data wrangling functions
################################################################################

pacman::p_load(tidyverse)

################################################################################

Covar_Prepper <- function(direct,
                         DesignVar_ags5,
                         BootVar_ags5,
                         KishVar_ags5,
                         BootCalibVar_ags5,
                         df_Covars,
                         Var_Selection,
                         lambda_type = c("lambda_min", "lambda_1SE")){
  
  #load smoothed variances:
  
  load(file = paste0(data_path,"/temp/update10/Variance_Smoothing_Image"))
  
  GVF_BootCalibVar_ags5 <- df_GVF$Vgvf
  
  #Calculate the Coefficient of Variation:
  
  CV_ana <- sqrt(DesignVar_ags5)/direct
  CV_boot <- sqrt(BootVar_ags5)/direct
  CV_kish <- sqrt(KishVar_ags5)/direct
  CV_calibboot <- sqrt(BootCalibVar_ags5)/direct
  CV_GVF_calibboot <- sqrt(GVF_BootCalibVar_ags5)/direct
  
  auxnames <-   eval(parse(text = paste0("Var_Selection$", lambda_type))) %>% 
    dplyr::slice(-1) %>% rownames()
  aux <- df_Covars
  aux <- aux[order(aux$ags5),] %>%          # sort aux by dom
    dplyr::select(any_of(c("ags5", auxnames)))     # select significant variables
  
  aux_scaled  <- aux %>% 
    dplyr::select(!ags5) %>% #scale covariates
    as.matrix() %>% 
    scale() %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(ags5 = aux$ags5)
  
  aux <- aux %>%
    dplyr::mutate(vardir_ana = DesignVar_ags5) %>% 
    dplyr::mutate(vardir_boot = BootVar_ags5) %>% 
    dplyr::mutate(vardir_kish = KishVar_ags5) %>% 
    dplyr::mutate(vardir_calibboot = BootCalibVar_ags5) %>% 
    dplyr::mutate(vardir_GVF_calibboot = GVF_BootCalibVar_ags5)
  
  
  combined <- aux %>%
    dplyr::mutate(dir = dir) %>%
    dplyr::mutate(nd = as.numeric(nd)) %>%
    data.frame()
  
  aux_scaled <- aux_scaled %>%
    dplyr::mutate(vardir_ana = DesignVar_ags5) %>% 
    dplyr::mutate(vardir_boot = BootVar_ags5) %>% 
    dplyr::mutate(vardir_kish = KishVar_ags5) %>% 
    dplyr::mutate(vardir_calibboot = BootCalibVar_ags5) %>% 
    dplyr::mutate(vardir_GVF_calibboot = GVF_BootCalibVar_ags5)
  
  combined_scaled <- aux_scaled %>%
    dplyr::mutate(dir = dir) %>%
    dplyr::mutate(nd = as.numeric(nd)) %>%
    data.frame()

  list(auxnames = auxnames, combined_scaled = combined_scaled)
  
}
  
