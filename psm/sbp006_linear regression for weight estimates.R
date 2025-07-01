rm(list=ls());gc();source(".Rprofile")

source("analysis/sba_logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)
# Bmi_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 
Bmi_after_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp002/analytic sample with matchit.RDS")) %>% 
  dplyr::filter(!is.na(Bmi_after)) %>% 
  mutate(exposure_category = case_when(exposure_binary == 0 ~ 0,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(1,2) ~ 1,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited %in% c(3,4) ~ 2,
                                       exposure_binary == 1 & maxWLprescriptionduration_edited >=5 ~ 3,
                                       TRUE ~ NA_real_
  )) %>% 
  mutate(exposure_category = factor(exposure_category,levels=c(0:3),labels=c("Unexposed","1to2","3to4","5plus")))


p0 <- run_geeglm_bmi(f = formula_p0,
                     df=Bmi_after_df,type = "matchit")

p1 <- run_geeglm_bmi(f = formula_p1,
                     df=Bmi_after_df,type = "matchit")

p2 <- run_geeglm_bmi(f = formula_p2,
                     df=Bmi_after_df,type = "matchit")

p3 <- run_geeglm_bmi(f = formula_p3,
                     df=Bmi_after_df,type = "matchit")

p4 <- run_geeglm_bmi(f = formula_p4,
                     df=Bmi_after_df,type = "matchit")

p5 <- run_geeglm_bmi(f = formula_p5,
                     df=Bmi_after_df,type = "matchit")

p6 <- run_geeglm_bmi(f = formula_p6,
                     df=Bmi_after_df,type = "matchit")

p7 <- run_geeglm_bmi(f = formula_p7,
                     df=Bmi_after_df,type = "matchit")

list(
  p0_output = list(p0, outcome = "Weight_after",model="p0"),
  p1_output = list(p1, outcome = "Weight_after",model="p1"),
  p2_output = list(p2, outcome = "Weight_after",model="p2"),
  p3_output = list(p3, outcome = "Weight_after",model="p3"),
  p4_output = list(p4, outcome = "Weight_after",model="p4"),
  p5_output = list(p5, outcome = "Weight_after",model="p5"),
  p6_output = list(p6, outcome = "Weight_after",model="p6"),
  p7_output = list(p7, outcome = "Weight_after",model="p7")
) %>% 
  saveRDS(.,paste0(path_sga_bmi_folder,"/working/sbp006/geeglm linear regression for weight output.RDS"))


sbp006_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp006/geeglm linear regression for weight output.RDS"))

coefs_model <- bind_rows(
  
  tidy_save_geeglm(sbp006_output$p0_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sbp006_output$p0_output$outcome,
           model = sbp006_output$p0_output$model),
  
  tidy_save_geeglm(sbp006_output$p1_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sbp006_output$p1_output$outcome,
           model = sbp006_output$p1_output$model),
  
  tidy_save_geeglm(sbp006_output$p2_output[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sbp006_output$p2_output$outcome,
           model = sbp006_output$p2_output$model),
  
  tidy_save_geeglm(sbp006_output$p3_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbp006_output$p3_output$outcome,
           model = sbp006_output$p3_output$model),
  
  tidy_save_geeglm(sbp006_output$p4_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbp006_output$p4_output$outcome,
           model = sbp006_output$p4_output$model),
  
  tidy_save_geeglm(sbp006_output$p5_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbp006_output$p5_output$outcome,
           model = sbp006_output$p5_output$model),
  
  tidy_save_geeglm(sbp006_output$p6_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbp006_output$p6_output$outcome,
           model = sbp006_output$p6_output$model),
  
  tidy_save_geeglm(sbp006_output$p7_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbp006_output$p7_output$outcome,
           model = sbp006_output$p7_output$model)
  
)

coefs_model %>% 
  write_csv("psm/sbp006_coefficients of linear regression for weight.csv")
