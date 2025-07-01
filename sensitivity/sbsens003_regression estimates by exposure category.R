
rm(list=ls());gc();source(".Rprofile")
source("analysis/sba_linear and logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)
# Bmi_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 

analytic_sample <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp002/analytic sample with matchit.RDS")) %>% 
  dplyr::filter(!is.na(Weight_after)) %>% 
  dplyr::select(-exposure_category) %>% 
  left_join(read_parquet(paste0(path_sga_bmi_folder,"/working/analytic sample.parquet")) %>% 
              dplyr::select(PatientDurableKey,AAPMonthYearKey,exposure_category),
            by=c("PatientDurableKey","AAPMonthYearKey"))


sema_df <-  analytic_sample %>% 
  dplyr::filter(exposure_category %in% c("semaglutide","both","none"))

tirz_df <- analytic_sample %>% 
  dplyr::filter(exposure_category %in% c("tirzepatide","both","none"))

sema_n0 <- run_geeglm_bmi(f = formula_n0,
                            df=sema_df,type = "matchit")

tirz_n0 <- run_geeglm_bmi(f = formula_n0,
                          df=tirz_df,type = "matchit")

sema_m0 <- run_geeglm_bmi(f = formula_m0,
                            df=sema_df,type = "matchit")

tirz_m0 <- run_geeglm_bmi(f = formula_m0,
                          df=tirz_df,type = "matchit")

sema_p0 <- run_geeglm_bmi(f = formula_p0,
                            df=sema_df,type = "matchit")

tirz_p0 <- run_geeglm_bmi(f = formula_p0,
                          df=tirz_df,type = "matchit")

sema_n5 <- run_geeglm_bmi(f = formula_n5,
                          df=sema_df,type = "matchit")

tirz_n5 <- run_geeglm_bmi(f = formula_n5,
                          df=tirz_df,type = "matchit")

sema_m5 <- run_geeglm_bmi(f = formula_m5,
                          df=sema_df,type = "matchit")

tirz_m5 <- run_geeglm_bmi(f = formula_m5,
                          df=tirz_df,type = "matchit")

sema_p5 <- run_geeglm_bmi(f = formula_p5,
                          df=sema_df,type = "matchit")

tirz_p5 <- run_geeglm_bmi(f = formula_p5,
                          df=tirz_df,type = "matchit")

list(
  sema_n0_output = list(sema_n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0",population = "sema"),
  tirz_n0_output = list(tirz_n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0",population = "tirz"),
  sema_m0_output = list(sema_m0, outcome = "pctchange_Weight_after",model="m0",population = "sema"),
  tirz_m0_output = list(tirz_m0, outcome = "pctchange_Weight_after",model="m0",population = "tirz"),
  sema_p0_output = list(sema_p0, outcome = "Weight_after",model="p0",population = "sema"),
  tirz_p0_output = list(tirz_p0, outcome = "Weight_after",model="p0",population = "tirz"),
  
  sema_n5_output = list(sema_n5, outcome = "pctchange_Weight_after_ge_minus5",model="n5",population = "sema"),
  tirz_n5_output = list(tirz_n5, outcome = "pctchange_Weight_after_ge_minus5",model="n5",population = "tirz"),
  sema_m5_output = list(sema_m5, outcome = "pctchange_Weight_after",model="m5",population = "sema"),
  tirz_m5_output = list(tirz_m5, outcome = "pctchange_Weight_after",model="m5",population = "tirz"),
  sema_p5_output = list(sema_p5, outcome = "Weight_after",model="p5",population = "sema"),
  tirz_p5_output = list(tirz_p5, outcome = "Weight_after",model="p5",population = "tirz")
) %>% 
  saveRDS(.,paste0(path_sga_bmi_folder,"/working/sbsens003/geeglm binomial and linear regression output by exposure_category.RDS"))


sbsens003_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sbsens003/geeglm binomial and linear regression output by exposure_category.RDS"))

coefs_model <- bind_rows(
  
  tidy_save_geeglm(sbsens003_output$sema_n0[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sbsens003_output$sema_n0$outcome,
           model = sbsens003_output$sema_n0$model,
           population = sbsens003_output$sema_n0$population),
  
  tidy_save_geeglm(sbsens003_output$tirz_n0[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sbsens003_output$tirz_n0$outcome,
           model = sbsens003_output$tirz_n0$model,
           population = sbsens003_output$tirz_n0$population),
  
  tidy_save_geeglm(sbsens003_output$sema_m0[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sbsens003_output$sema_m0$outcome,
           model = sbsens003_output$sema_m0$model,
           population = sbsens003_output$sema_m0$population),
  
  tidy_save_geeglm(sbsens003_output$tirz_m0[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens003_output$tirz_m0$outcome,
           model = sbsens003_output$tirz_m0$model,
           population = sbsens003_output$tirz_m0$population),
  
  tidy_save_geeglm(sbsens003_output$sema_p0_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens003_output$sema_p0_output$outcome,
           model = sbsens003_output$sema_p0_output$model,
           population = sbsens003_output$sema_p0$population),
  
  tidy_save_geeglm(sbsens003_output$tirz_p0_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens003_output$tirz_p0_output$outcome,
           model = sbsens003_output$tirz_p0_output$model,
           population = sbsens003_output$tirz_p0$population)
  
)

coefs_model %>% 
  write_csv("sensitivity/sbsens003_regression coefficients of linear and binomial regression by exposure category.csv")

# Contrasts --------------------------
source("analysis/sba_difference grids.R")

source("H:/code/functions/imputation/contrasts_geeglm.R")
source("functions/sbp007_contrast_fit.R")

pctchange_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                  function(i){
                                    x_name = difference_grid_sga$exposure[i]
                                    y_name = difference_grid_sga$modifier[i]
                                    bind_rows(
                                      sbp007_contrast_fit(gee_saved_fit = sbsens003_output$sema_m5_output[[1]],x_name,y_name) %>% 
                                        mutate(exposure = x_name,
                                               modifier = y_name,
                                               exposure_value = 1,
                                               modifier_value = 1,
                                               outcome = "pctchange_Weight_after",
                                               population = "sema"),
                                      sbp007_contrast_fit(gee_saved_fit = sbsens003_output$tirz_m5_output[[1]],x_name,y_name) %>% 
                                        mutate(exposure = x_name,
                                               modifier = y_name,
                                               exposure_value = 1,
                                               modifier_value = 1,
                                               outcome = "pctchange_Weight_after",
                                               population = "tirz")
                                      
                                    )
                                    
                                  }
                                  
)

pct5change_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                   function(i){
                                     x_name = difference_grid_sga$exposure[i]
                                     y_name = difference_grid_sga$modifier[i]
                                     bind_rows(
                                       sbp007_contrast_fit(gee_saved_fit = sbsens003_output$sema_n5_output[[1]],x_name,y_name) %>% 
                                         mutate(exposure = x_name,
                                                modifier = y_name,
                                                exposure_value = 1,
                                                modifier_value = 1,
                                                outcome = "pctchange_Weight_after_ge_minus5",
                                                population = "sema"),
                                       sbp007_contrast_fit(gee_saved_fit = sbsens003_output$tirz_n5_output[[1]],x_name,y_name) %>% 
                                         mutate(exposure = x_name,
                                                modifier = y_name,
                                                exposure_value = 1,
                                                modifier_value = 1,
                                                outcome = "pctchange_Weight_after_ge_minus5",
                                                population = "tirz")
                                       
                                     )
                                     
                                   }
                                   
)


weight_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                               function(i){
                                 x_name = difference_grid_sga$exposure[i]
                                 y_name = difference_grid_sga$modifier[i]
                                 bind_rows(
                                   sbp007_contrast_fit(gee_saved_fit = sbsens003_output$sema_p5_output[[1]],x_name,y_name) %>% 
                                     mutate(exposure = x_name,
                                            modifier = y_name,
                                            exposure_value = 1,
                                            modifier_value = 1,
                                            outcome = "Weight_after",
                                            population = "sema"),
                                   sbp007_contrast_fit(gee_saved_fit = sbsens003_output$tirz_p5_output[[1]],x_name,y_name) %>% 
                                     mutate(exposure = x_name,
                                            modifier = y_name,
                                            exposure_value = 1,
                                            modifier_value = 1,
                                            outcome = "Weight_after",
                                            population = "tirz")
                                   
                                 )
                                 
                               }
                               
)


bind_rows(pctchange_contrast_sga,
          pct5change_contrast_sga,
          weight_contrast_sga) %>% 
  mutate(coef = case_when(outcome == "pctchange_Weight_after_ge_minus5" ~ exp(Estimate),
                          TRUE ~ Estimate),
         lci = case_when(outcome == "pctchange_Weight_after_ge_minus5" ~ exp(LCI),
                         TRUE ~ LCI),
         uci = case_when(outcome == "pctchange_Weight_after_ge_minus5" ~ exp(UCI),
                         TRUE ~ UCI)) %>% 
  write_csv(.,"sensitivity/sbsens003_contrasts for sga by exposure category.csv")

