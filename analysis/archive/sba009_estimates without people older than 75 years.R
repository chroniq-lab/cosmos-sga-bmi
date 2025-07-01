rm(list=ls());gc();source(".Rprofile")

source("analysis/sba_logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)
# Bmi_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 
Bmi_after_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS")) %>% 
  dplyr::filter(!is.na(Bmi_after)) %>% 
  dplyr::filter(age <= 75) %>% 
  mutate(exposure_category = case_when(exposure_binary == 0 ~ 0,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(1,2) ~ 1,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(3,4) ~ 2,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited >=5 ~ 3,
                                       TRUE ~ NA_real_
  )) %>% 
  mutate(exposure_category = factor(exposure_category,levels=c(0:3),labels=c("Unexposed","1to2","3to4","5plus")))


p0 <- run_geeglm_bmi(f = formula_p0,
                     df=Bmi_after_df)

p3 <- run_geeglm_bmi(f = formula_p3,
                     df=Bmi_after_df)

m0 <- run_geeglm_bmi(f = formula_m0,
                     df=Bmi_after_df)

m3 <- run_geeglm_bmi(f = formula_m3,
                     df=Bmi_after_df)

n0 <- run_geeglm_bmi(f = formula_n0,
                     df=Bmi_after_df)

n3 <- run_geeglm_bmi(f = formula_n3,
                     df=Bmi_after_df)

list(
  p0_output = list(p0, outcome = "Weight_after",model="p0"),
  p3_output = list(p3, outcome = "Weight_after",model="p3"),
  
  m0_output = list(m0, outcome = "pctchange_Weight_after",model="m0"),
  m3_output = list(m3, outcome = "pctchange_Weight_after",model="m3"),
  
  n0_output = list(n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0"),
  n3_output = list(n3, outcome = "pctchange_Weight_after_ge_minus5",model="n3")
  
) %>% 
  saveRDS(.,paste0(path_sga_bmi_folder,"/working/sba009/geeglm regressions without people older than 75 years.RDS"))


sba009_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sba009/geeglm regressions without people older than 75 years.RDS"))

coefs_model <- bind_rows(
  
  tidy_save_geeglm(sba009_output$p0_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sba009_output$p0_output$outcome,
           model = sba009_output$p0_output$model),
  
  tidy_save_geeglm(sba009_output$p3_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sba009_output$p3_output$outcome,
           model = sba009_output$p3_output$model),
  
  
  tidy_save_geeglm(sba009_output$m0_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sba009_output$m0_output$outcome,
           model = sba009_output$m0_output$model),
  
  tidy_save_geeglm(sba009_output$m3_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sba009_output$m3_output$outcome,
           model = sba009_output$m3_output$model),
  

  tidy_save_geeglm(sba009_output$n0_output[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sba009_output$n0_output$outcome,
           model = sba009_output$n0_output$model),
  
  tidy_save_geeglm(sba009_output$n3_output[[1]],exponentiate = TRUE) %>%
    mutate(outcome = sba009_output$n3_output$outcome,
           model = sba009_output$n3_output$model)
  
  
)

coefs_model %>% 
  write_csv("analysis/sba009_coefficients without people older than 75 years.csv")
