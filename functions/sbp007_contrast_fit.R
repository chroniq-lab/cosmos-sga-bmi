# x = difference_grid_sga$exposure[1]
# gee_saved_fit = sbp004_output$n2_output[[1]]
# y = difference_grid_sga$modifier[1]

sbp007_contrast_fit <- function(gee_saved_fit,x,y){
  
  if(x %in% names(gee_saved_fit$coefficients)){
    
    if(y %in% names(gee_saved_fit$coefficients)){
      
      mm <- prepare_contrasts(nterms_het = length(gee_saved_fit$coefficients),
                              names_het = names(gee_saved_fit$coefficients),
                              cov_het = gee_saved_fit$robust.cov,
                              # Temporary ------
                              names_cov_het = names(gee_saved_fit$coefficients),
                              exposure = x,
                              modifier = y,
                              exposure_value = 1,
                              modifier_value = 1,
                              e_m_term = TRUE)
      
      # mm[[2]][2:3,which(names(gee_saved_fit$coefficients) %in% y)] <- 1 
      
    }else{
      
      mm <- prepare_contrasts(nterms_het = length(gee_saved_fit$coefficients),
                              names_het = names(gee_saved_fit$coefficients),
                              cov_het = gee_saved_fit$robust.cov,
                              # Temporary ------
                              names_cov_het = names(gee_saved_fit$coefficients),
                              exposure = x,
                              exposure_value = 1,
                              e_m_term = TRUE)
      
    }
    
  }else{
    
    mm <- prepare_contrasts(nterms_het = length(gee_saved_fit$coefficients),
                            names_het = names(gee_saved_fit$coefficients),
                            cov_het = gee_saved_fit$robust.cov,
                            # Temporary ------
                            names_cov_het = names(gee_saved_fit$coefficients),
                            exposure = y,
                            exposure_value = 1,
                            e_m_term = TRUE)
    
    
    
  }
  
  contrasts_geeglm(
    model_matrix = mm[[2]],
    vcov_gee = gee_saved_fit$robust.cov,
    coef_gee = gee_saved_fit$coefficients,
    dfcom_gee = gee_saved_fit$df.residual
  ) %>% 
    dplyr::filter(term %in% c("Contrast 2"))  %>% 
    return(.)
  
}
