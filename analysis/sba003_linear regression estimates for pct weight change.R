rm(list=ls());gc();source(".Rprofile")

source("analysis/sba_linear and logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)
# Bmi_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 
Bmi_after_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS")) %>% 
  dplyr::filter(!is.na(Bmi_after)) %>% 
  mutate(exposure_category = case_when(exposure_binary == 0 ~ 0,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(1,2) ~ 1,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(3,4) ~ 2,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited >=5 ~ 3,
                                       TRUE ~ NA_real_
                                       )) %>% 
  mutate(exposure_category = factor(exposure_category,levels=c(0:3),labels=c("Unexposed","1to2","3to4","5plus")))


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

m6 <- run_geeglm_bmi(f = formula_m6,
                     df=Bmi_after_df)

m7 <- run_geeglm_bmi(f = formula_m7,
                     df=Bmi_after_df)





list(
  m0_output = list(m0, outcome = "pctchange_Weight_after",model="m0"),
  m1_output = list(m1, outcome = "pctchange_Weight_after",model="m1"),
  m2_output = list(m2, outcome = "pctchange_Weight_after",model="m2"),
  m3_output = list(m3, outcome = "pctchange_Weight_after",model="m3"),
  m4_output = list(m4, outcome = "pctchange_Weight_after",model="m4"),
  m5_output = list(m5, outcome = "pctchange_Weight_after",model="m5"),
  m6_output = list(m6, outcome = "pctchange_Weight_after",model="m6"),
  m7_output = list(m7, outcome = "pctchange_Weight_after",model="m7")
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
           model = sba003_output$m5_output$model),
  
  
  tidy_save_geeglm(sba003_output$m6_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sba003_output$m6_output$outcome,
           model = sba003_output$m6_output$model),
  
  
  tidy_save_geeglm(sba003_output$m7_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sba003_output$m7_output$outcome,
           model = sba003_output$m7_output$model)
  
)

coefs_model %>% 
  write_csv("analysis/sba003_coefficients of linear regression for pct weight change.csv")

