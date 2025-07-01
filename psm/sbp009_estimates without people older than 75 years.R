rm(list=ls());gc();source(".Rprofile")

source("analysis/sba_logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)
# Bmi_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 
Bmi_after_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp002/analytic sample with matchit.RDS")) %>% 
  dplyr::filter(!is.na(Bmi_after)) %>% 
  dplyr::filter(age <= 75) %>% 
  mutate(exposure_duration = case_when(exposure_binary == 0 ~ 0,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(1,2) ~ 1,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(3,4) ~ 2,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited >=5 ~ 3,
                                       TRUE ~ NA_real_
  )) %>% 
  mutate(exposure_duration = factor(exposure_duration,levels=c(0:3),labels=c("Unexposed","1to2","3to4","5plus")))


p0 <- run_geeglm_bmi(f = formula_p0,
                     df=Bmi_after_df,type = "matchit")

p3 <- run_geeglm_bmi(f = formula_p3,
                     df=Bmi_after_df,type = "matchit")

m0 <- run_geeglm_bmi(f = formula_m0,
                     df=Bmi_after_df,type = "matchit")

m8 <- run_geeglm_bmi(f = formula_m8,
                     df=Bmi_after_df,type = "matchit")

n0 <- run_geeglm_bmi(f = formula_n0,
                     df=Bmi_after_df,type = "matchit")

n8 <- run_geeglm_bmi(f = formula_n8,
                     df=Bmi_after_df,type = "matchit")

list(
  p0_output = list(p0, outcome = "Weight_after",model="p0"),
  p8_output = list(p8, outcome = "Weight_after",model="p8"),
  
  m0_output = list(m0, outcome = "pctchange_Weight_after",model="m0"),
  m8_output = list(m8, outcome = "pctchange_Weight_after",model="m8"),
  
  n0_output = list(n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0"),
  n8_output = list(n8, outcome = "pctchange_Weight_after_ge_minus5",model="n8")
  
) %>% 
  saveRDS(.,paste0(path_sga_bmi_folder,"/working/sbp009/geeglm regressions without people older than 75 years.RDS"))


sbp009_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp009/geeglm regressions without people older than 75 years.RDS"))

coefs_model <- bind_rows(
  
  tidy_save_geeglm(sbp009_output$p0_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sbp009_output$p0_output$outcome,
           model = sbp009_output$p0_output$model),
  
  tidy_save_geeglm(sbp009_output$p8_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbp009_output$p8_output$outcome,
           model = sbp009_output$p8_output$model),
  
  
  tidy_save_geeglm(sbp009_output$m0_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sbp009_output$m0_output$outcome,
           model = sbp009_output$m0_output$model),
  
  tidy_save_geeglm(sbp009_output$m8_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbp009_output$m8_output$outcome,
           model = sbp009_output$m8_output$model),
  

  tidy_save_geeglm(sbp009_output$n0_output[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sbp009_output$n0_output$outcome,
           model = sbp009_output$n0_output$model),
  
  tidy_save_geeglm(sbp009_output$n8_output[[1]],exponentiate = TRUE) %>%
    mutate(outcome = sbp009_output$n8_output$outcome,
           model = sbp009_output$n8_output$model)
  
  
)

coefs_model %>% 
  write_csv("psm/sbp009_coefficients without people older than 75 years.csv")
