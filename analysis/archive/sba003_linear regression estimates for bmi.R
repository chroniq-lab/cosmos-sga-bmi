rm(list=ls());gc();source(".Rprofile")


# formula_m0 = paste0(outcome_bmi,exposure_main," + AAPMonthYear_BodyMassIndex")
# formula_m1 = paste0(outcome_bmi,exposure_time," + AAPMonthYear_BodyMassIndex")
# formula_m2 = paste0(outcome_bmi,exposure_sga," + AAPMonthYear_BodyMassIndex")
# formula_m3 = paste0(outcome_bmi,exposure_main," + AAPMonthYear_BodyMassIndex",covariates)
# formula_m4 = paste0(outcome_bmi,exposure_sga_time," + AAPMonthYear_BodyMassIndex")
# formula_m5 = paste0(outcome_bmi,exposure_sga," + AAPMonthYear_BodyMassIndex",covariates)


source("analysis/sba_logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)
# Bmi_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 
Bmi_after_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS")) %>% 
  dplyr::filter(!is.na(Bmi_after)) 


m0 <- run_geeglm_bmi(f = formula_m0,
           df=Bmi_after_df)

m1 <- run_geeglm_bmi(f = formula_m1,
           df=Bmi_after_df)

m2 <- run_geeglm_bmi(f = formula_m2,
           df=Bmi_after_df)

m3 <- run_geeglm_bmi(f = formula_m3,
           df=Bmi_after_df)

m4 <- run_geeglm_bmi(f = formula_m4,
           df=Bmi_after_df)

m5 <- run_geeglm_bmi(f = formula_m5,
           df=Bmi_after_df)





list(
  m0_output = list(m0, outcome = "Bmi_after",model="m0"),
  m1_output = list(m1, outcome = "Bmi_after",model="m1"),
  m2_output = list(m2, outcome = "Bmi_after",model="m2"),
  m3_output = list(m3, outcome = "Bmi_after",model="m3"),
  m4_output = list(m4, outcome = "Bmi_after",model="m4"),
  m5_output = list(m5, outcome = "Bmi_after",model="m5")
) %>% 
  saveRDS(.,paste0(path_sga_bmi_folder,"/working/sba003/geeglm linear regression output.RDS"))


sba003_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sba003/geeglm linear regression output.RDS"))

coefs_model <- bind_rows(
  
  tidy_save_geeglm(sba003_output$m0_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sba003_output$m0_output$outcome,
           model = sba003_output$m0_output$model),
  
  tidy_save_geeglm(sba003_output$m1_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sba003_output$m1_output$outcome,
           model = sba003_output$m1_output$model),
  
  tidy_save_geeglm(sba003_output$m2_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sba003_output$m2_output$outcome,
           model = sba003_output$m2_output$model),
  
  tidy_save_geeglm(sba003_output$m3_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sba003_output$m3_output$outcome,
           model = sba003_output$m3_output$model),
  
  tidy_save_geeglm(sba003_output$m4_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sba003_output$m4_output$outcome,
           model = sba003_output$m4_output$model),
  
  tidy_save_geeglm(sba003_output$m5_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sba003_output$m5_output$outcome,
           model = sba003_output$m5_output$model)
  
)

coefs_model %>% 
  write_csv("analysis/sba003_coefficients of linear regression.csv")

