rm(list=ls());gc();source(".Rprofile")
source("analysis/sba_linear and logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)
# Bmi_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 
Weight_after_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS")) %>% 
  dplyr::filter(!is.na(Weight_after))  %>% 
  mutate(exposure_category = case_when(exposure_binary == 0 ~ 0,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(1,2) ~ 1,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(3,4) ~ 2,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited >=5 ~ 3,
                                       TRUE ~ NA_real_
  )) %>% 
  mutate(exposure_category = factor(exposure_category,levels=c(0:3),labels=c("Unexposed","1to2","3to4","5plus")))


n0 <- run_geeglm_bmi(f = formula_n0,
                 df=Weight_after_df)

n1 <- run_geeglm_bmi(f = formula_n1,
                 df=Weight_after_df)

n2 <- run_geeglm_bmi(f = formula_n2,
                 df=Weight_after_df)

n3 <- run_geeglm_bmi(f = formula_n3,
                 df=Weight_after_df)

n4 <- run_geeglm_bmi(f = formula_n4,
                 df=Weight_after_df)

n5 <- run_geeglm_bmi(f = formula_n5,
                 df=Weight_after_df)

n6 <- run_geeglm_bmi(f = formula_n6,
                 df=Weight_after_df)

n7 <- run_geeglm_bmi(f = formula_n7,
                 df=Weight_after_df)

list(
  n0_output = list(n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0"),
  n1_output = list(n1, outcome = "pctchange_Weight_after_ge_minus5",model="n1"),
  n2_output = list(n2, outcome = "pctchange_Weight_after_ge_minus5",model="n2"),
  n3_output = list(n3, outcome = "pctchange_Weight_after_ge_minus5",model="n3"),
  n4_output = list(n4, outcome = "pctchange_Weight_after_ge_minus5",model="n4"),
  n5_output = list(n5, outcome = "pctchange_Weight_after_ge_minus5",model="n5"),
  n6_output = list(n6, outcome = "pctchange_Weight_after_ge_minus5",model="n6"),
  n7_output = list(n7, outcome = "pctchange_Weight_after_ge_minus5",model="n7")
) %>% 
  saveRDS(.,paste0(path_sga_bmi_folder,"/working/sba004/geeglm binomial regression output.RDS"))


sba004_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sba004/geeglm binomial regression output.RDS"))

coefs_model <- bind_rows(
  
  tidy_save_geeglm(sba004_output$n0_output[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sba004_output$n0_output$outcome,
           model = sba004_output$n0_output$model),
  
  tidy_save_geeglm(sba004_output$n1_output[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sba004_output$n1_output$outcome,
           model = sba004_output$n1_output$model),
  
  tidy_save_geeglm(sba004_output$n2_output[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sba004_output$n2_output$outcome,
           model = sba004_output$n2_output$model),
  
  tidy_save_geeglm(sba004_output$n3_output[[1]],exponentiate = TRUE) %>%
    mutate(outcome = sba004_output$n3_output$outcome,
           model = sba004_output$n3_output$model),
  
  tidy_save_geeglm(sba004_output$n4_output[[1]],exponentiate = TRUE) %>%
    mutate(outcome = sba004_output$n4_output$outcome,
           model = sba004_output$n4_output$model),
  
  tidy_save_geeglm(sba004_output$n5_output[[1]],exponentiate = TRUE) %>%
    mutate(outcome = sba004_output$n5_output$outcome,
           model = sba004_output$n5_output$model),
  
  tidy_save_geeglm(sba004_output$n6_output[[1]],exponentiate = TRUE) %>%
    mutate(outcome = sba004_output$n6_output$outcome,
           model = sba004_output$n6_output$model),
  
  tidy_save_geeglm(sba004_output$n7_output[[1]],exponentiate = TRUE) %>%
    mutate(outcome = sba004_output$n7_output$outcome,
           model = sba004_output$n7_output$model)
  
)

coefs_model %>% 
  write_csv("analysis/sba004_coefficients of binomial regression.csv")
