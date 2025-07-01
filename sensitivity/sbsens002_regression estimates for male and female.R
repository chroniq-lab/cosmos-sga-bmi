

rm(list=ls());gc();source(".Rprofile")
source("analysis/sba_linear and logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)
# Bmi_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 
female_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp002/analytic sample with matchit.RDS")) %>% 
  dplyr::filter(!is.na(Weight_after))  %>% 

  dplyr::filter(female == 1 )

male_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp002/analytic sample with matchit.RDS")) %>% 
  dplyr::filter(!is.na(Weight_after))  %>% 

  dplyr::filter(female == 0)

female_n0 <- run_geeglm_bmi(f = formula_n0,
                            df=female_df,type = "matchit")

male_n0 <- run_geeglm_bmi(f = formula_n0,
                          df=male_df,type = "matchit")

female_m0 <- run_geeglm_bmi(f = formula_m0,
                            df=female_df,type = "matchit")

male_m0 <- run_geeglm_bmi(f = formula_m0,
                          df=male_df,type = "matchit")

female_p0 <- run_geeglm_bmi(f = formula_p0,
                            df=female_df,type = "matchit")

male_p0 <- run_geeglm_bmi(f = formula_p0,
                          df=male_df,type = "matchit")

female_n5 <- run_geeglm_bmi(f = formula_n5b,
                            df=female_df,type = "matchit")

male_n5 <- run_geeglm_bmi(f = formula_n5b,
                          df=male_df,type = "matchit")

female_m5 <- run_geeglm_bmi(f = formula_m5b,
                            df=female_df,type = "matchit")

male_m5 <- run_geeglm_bmi(f = formula_m5b,
                          df=male_df,type = "matchit")

female_p5 <- run_geeglm_bmi(f = formula_p5b,
                            df=female_df,type = "matchit")

male_p5 <- run_geeglm_bmi(f = formula_p5b,
                          df=male_df,type = "matchit")


list(
  female_n0_output = list(female_n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0",population = "female"),
  male_n0_output = list(male_n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0",population = "male"),
  female_m0_output = list(female_m0, outcome = "pctchange_Weight_after",model="m0",population = "female"),
  male_m0_output = list(male_m0, outcome = "pctchange_Weight_after",model="m0",population = "male"),
  female_p0_output = list(female_p0, outcome = "Weight_after",model="p0",population = "female"),
  male_p0_output = list(male_p0, outcome = "Weight_after",model="p0",population = "male"),
  
  female_n5_output = list(female_n5, outcome = "pctchange_Weight_after_ge_minus5",model="n5",population = "female"),
  male_n5_output = list(male_n5, outcome = "pctchange_Weight_after_ge_minus5",model="n5",population = "male"),
  female_m5_output = list(female_m5, outcome = "pctchange_Weight_after",model="m5",population = "female"),
  male_m5_output = list(male_m5, outcome = "pctchange_Weight_after",model="m5",population = "male"),
  female_p5_output = list(female_p5, outcome = "Weight_after",model="p5",population = "female"),
  male_p5_output = list(male_p5, outcome = "Weight_after",model="p5",population = "male")
) %>% 
  saveRDS(.,paste0(path_sga_bmi_folder,"/working/sbsens002/geeglm binomial and linear regression output by sex.RDS"))


sbsens002_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sbsens002/geeglm binomial and linear regression output by sex.RDS"))

coefs_model <- bind_rows(
  
  tidy_save_geeglm(sbsens002_output$female_n0[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sbsens002_output$female_n0$outcome,
           model = sbsens002_output$female_n0$model,
           population = sbsens002_output$female_n0$population),
  
  tidy_save_geeglm(sbsens002_output$male_n0[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sbsens002_output$male_n0$outcome,
           model = sbsens002_output$male_n0$model,
           population = sbsens002_output$male_n0$population),
  
  tidy_save_geeglm(sbsens002_output$female_m0[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sbsens002_output$female_m0$outcome,
           model = sbsens002_output$female_m0$model,
           population = sbsens002_output$female_m0$population),
  
  tidy_save_geeglm(sbsens002_output$male_m0[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens002_output$male_m0$outcome,
           model = sbsens002_output$male_m0$model,
           population = sbsens002_output$male_m0$population),
  
  tidy_save_geeglm(sbsens002_output$female_p0_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens002_output$female_p0_output$outcome,
           model = sbsens002_output$female_p0_output$model,
           population = sbsens002_output$female_p0$population),
  
  tidy_save_geeglm(sbsens002_output$male_p0_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens002_output$male_p0_output$outcome,
           model = sbsens002_output$male_p0_output$model,
           population = sbsens002_output$male_p0$population)
  
)

coefs_model %>% 
  write_csv("sensitivity/sbsens002_regression coefficients of linear and binomial regression by sex.csv")


# Contrasts --------------------------
source("analysis/sba_difference grids.R")

source("H:/code/functions/imputation/contrasts_geeglm.R")
source("functions/sbp007_contrast_fit.R")

pctchange_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                  function(i){
                                    x_name = difference_grid_sga$exposure[i]
                                    y_name = difference_grid_sga$modifier[i]
                                    bind_rows(
                                      sbp007_contrast_fit(gee_saved_fit = sbsens002_output$female_m5_output[[1]],x_name,y_name) %>% 
                                        mutate(exposure = x_name,
                                               modifier = y_name,
                                               exposure_value = 1,
                                               modifier_value = 1,
                                               outcome = "pctchange_Weight_after",
                                               population = "female"),
                                      sbp007_contrast_fit(gee_saved_fit = sbsens002_output$male_m5_output[[1]],x_name,y_name) %>% 
                                        mutate(exposure = x_name,
                                               modifier = y_name,
                                               exposure_value = 1,
                                               modifier_value = 1,
                                               outcome = "pctchange_Weight_after",
                                               population = "male")
                                      
                                    )
                                    
                                  }
                                  
)

pct5change_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                   function(i){
                                     x_name = difference_grid_sga$exposure[i]
                                     y_name = difference_grid_sga$modifier[i]
                                     bind_rows(
                                       sbp007_contrast_fit(gee_saved_fit = sbsens002_output$female_n5_output[[1]],x_name,y_name) %>% 
                                         mutate(exposure = x_name,
                                                modifier = y_name,
                                                exposure_value = 1,
                                                modifier_value = 1,
                                                outcome = "pctchange_Weight_after_ge_minus5",
                                                population = "female"),
                                       sbp007_contrast_fit(gee_saved_fit = sbsens002_output$male_n5_output[[1]],x_name,y_name) %>% 
                                         mutate(exposure = x_name,
                                                modifier = y_name,
                                                exposure_value = 1,
                                                modifier_value = 1,
                                                outcome = "pctchange_Weight_after_ge_minus5",
                                                population = "male")
                                       
                                     )
                                     
                                   }
                                   
)


weight_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                               function(i){
                                 x_name = difference_grid_sga$exposure[i]
                                 y_name = difference_grid_sga$modifier[i]
                                 bind_rows(
                                   sbp007_contrast_fit(gee_saved_fit = sbsens002_output$female_p5_output[[1]],x_name,y_name) %>% 
                                     mutate(exposure = x_name,
                                            modifier = y_name,
                                            exposure_value = 1,
                                            modifier_value = 1,
                                            outcome = "Weight_after",
                                            population = "female"),
                                   sbp007_contrast_fit(gee_saved_fit = sbsens002_output$male_p5_output[[1]],x_name,y_name) %>% 
                                     mutate(exposure = x_name,
                                            modifier = y_name,
                                            exposure_value = 1,
                                            modifier_value = 1,
                                            outcome = "Weight_after",
                                            population = "male")
                                   
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
  write_csv(.,"sensitivity/sbsens002_contrasts for sga by sex.csv")

