rm(list=ls());gc();source(".Rprofile")
source("analysis/sba_linear and logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)

analytic <- read_parquet(paste0(path_sga_bmi_folder,"/working/analytic sample.parquet")) %>% 
  dplyr::filter(exposure_binary==1)


with(analytic, table(bipolar_manic == 1 | depressive_mdd == 1))

with(analytic, table(schizoaffective == 1 |schizophrenia == 1 | delusional == 1 | schizotypal == 1 | psychosis_unspecified == 1))

affective_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp002/analytic sample with matchit.RDS")) %>% 
  dplyr::filter(!is.na(Weight_after))  %>% 

  dplyr::filter(bipolar_manic == 1 | depressive_mdd == 1)

psychosis_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp002/analytic sample with matchit.RDS")) %>% 
  dplyr::filter(!is.na(Weight_after))  %>% 

  dplyr::filter(schizoaffective == 1 |schizophrenia == 1 | delusional == 1 | schizotypal == 1 | psychosis_unspecified == 1)

affective_n0 <- run_geeglm_bmi(f = formula_n0,
                               df=affective_df,type = "matchit")

psychosis_n0 <- run_geeglm_bmi(f = formula_n0,
                               df=psychosis_df,type = "matchit")

affective_m0 <- run_geeglm_bmi(f = formula_m0,
                               df=affective_df,type = "matchit")

psychosis_m0 <- run_geeglm_bmi(f = formula_m0,
                               df=psychosis_df,type = "matchit")

affective_p0 <- run_geeglm_bmi(f = formula_p0,
                               df=affective_df,type = "matchit")

psychosis_p0 <- run_geeglm_bmi(f = formula_p0,
                               df=psychosis_df,type = "matchit")


affective_n5 <- run_geeglm_bmi(f = formula_n5,
                               df=affective_df,type = "matchit")

psychosis_n5 <- run_geeglm_bmi(f = formula_n5,
                               df=psychosis_df,type = "matchit")

affective_m5 <- run_geeglm_bmi(f = formula_m5,
                               df=affective_df,type = "matchit")

psychosis_m5 <- run_geeglm_bmi(f = formula_m5,
                               df=psychosis_df,type = "matchit")

affective_p5 <- run_geeglm_bmi(f = formula_p5,
                               df=affective_df,type = "matchit")

psychosis_p5 <- run_geeglm_bmi(f = formula_p5,
                               df=psychosis_df,type = "matchit")


list(
  affective_n0_output = list(affective_n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0",population = "affective"),
  psychosis_n0_output = list(psychosis_n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0",population = "psychosis"),
  affective_m0_output = list(affective_m0, outcome = "pctchange_Weight_after",model="m0",population = "affective"),
  psychosis_m0_output = list(psychosis_m0, outcome = "pctchange_Weight_after",model="m0",population = "psychosis"),
  affective_p0_output = list(affective_p0, outcome = "Weight_after",model="p0",population = "affective"),
  psychosis_p0_output = list(psychosis_p0, outcome = "Weight_after",model="p0",population = "psychosis"),
  
  affective_n5_output = list(affective_n5, outcome = "pctchange_Weight_after_ge_minus5",model="n5",population = "affective"),
  psychosis_n5_output = list(psychosis_n5, outcome = "pctchange_Weight_after_ge_minus5",model="n5",population = "psychosis"),
  affective_m5_output = list(affective_m5, outcome = "pctchange_Weight_after",model="m5",population = "affective"),
  psychosis_m5_output = list(psychosis_m5, outcome = "pctchange_Weight_after",model="m5",population = "psychosis"),
  affective_p5_output = list(affective_p5, outcome = "Weight_after",model="p5",population = "affective"),
  psychosis_p5_output = list(psychosis_p5, outcome = "Weight_after",model="p5",population = "psychosis")
) %>% 
  saveRDS(.,paste0(path_sga_bmi_folder,"/working/sbsens001/geeglm binomial and linear regression output by disorder.RDS"))


sbsens001_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sbsens001/geeglm binomial and linear regression output by disorder.RDS"))

coefs_model <- bind_rows(
  
  tidy_save_geeglm(sbsens001_output$affective_n0[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sbsens001_output$affective_n0$outcome,
           model = sbsens001_output$affective_n0$model,
           population = sbsens001_output$affective_n0$population),
  
  tidy_save_geeglm(sbsens001_output$psychosis_n0[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sbsens001_output$psychosis_n0$outcome,
           model = sbsens001_output$psychosis_n0$model,
           population = sbsens001_output$psychosis_n0$population),
  
  tidy_save_geeglm(sbsens001_output$affective_m0[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sbsens001_output$affective_m0$outcome,
           model = sbsens001_output$affective_m0$model,
           population = sbsens001_output$affective_m0$population),
  
  tidy_save_geeglm(sbsens001_output$psychosis_m0[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens001_output$psychosis_m0$outcome,
           model = sbsens001_output$psychosis_m0$model,
           population = sbsens001_output$psychosis_m0$population),
  
  tidy_save_geeglm(sbsens001_output$affective_p0_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens001_output$affective_p0_output$outcome,
           model = sbsens001_output$affective_p0_output$model,
           population = sbsens001_output$affective_p0$population),
  
  tidy_save_geeglm(sbsens001_output$psychosis_p0_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens001_output$psychosis_p0_output$outcome,
           model = sbsens001_output$psychosis_p0_output$model,
           population = sbsens001_output$psychosis_p0_output$population)
  
)

coefs_model %>% 
  write_csv("sensitivity/sbsens001_regression coefficients of linear and binomial regression by disorder.csv")





# Contrasts --------------------------
source("analysis/sba_difference grids.R")

source("H:/code/functions/imputation/contrasts_geeglm.R")
source("functions/sbp007_contrast_fit.R")

pctchange_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                  function(i){
                                    x_name = difference_grid_sga$exposure[i]
                                    y_name = difference_grid_sga$modifier[i]
                                    bind_rows(
                                      sbp007_contrast_fit(gee_saved_fit = sbsens001_output$affective_m5_output[[1]],x_name,y_name) %>% 
                                        mutate(exposure = x_name,
                                               modifier = y_name,
                                               exposure_value = 1,
                                               modifier_value = 1,
                                               outcome = "pctchange_Weight_after",
                                               population = "affective"),
                                      sbp007_contrast_fit(gee_saved_fit = sbsens001_output$psychosis_m5_output[[1]],x_name,y_name) %>% 
                                        mutate(exposure = x_name,
                                               modifier = y_name,
                                               exposure_value = 1,
                                               modifier_value = 1,
                                               outcome = "pctchange_Weight_after",
                                               population = "psychosis")
                                      
                                    )
                                    
                                  }
                                  
)

pct5change_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                  function(i){
                                    x_name = difference_grid_sga$exposure[i]
                                    y_name = difference_grid_sga$modifier[i]
                                    bind_rows(
                                      sbp007_contrast_fit(gee_saved_fit = sbsens001_output$affective_n5_output[[1]],x_name,y_name) %>% 
                                        mutate(exposure = x_name,
                                               modifier = y_name,
                                               exposure_value = 1,
                                               modifier_value = 1,
                                               outcome = "pctchange_Weight_after_ge_minus5",
                                               population = "affective"),
                                      sbp007_contrast_fit(gee_saved_fit = sbsens001_output$psychosis_n5_output[[1]],x_name,y_name) %>% 
                                        mutate(exposure = x_name,
                                               modifier = y_name,
                                               exposure_value = 1,
                                               modifier_value = 1,
                                               outcome = "pctchange_Weight_after_ge_minus5",
                                               population = "psychosis")
                                      
                                    )
                                    
                                  }
                                  
)


weight_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                   function(i){
                                     x_name = difference_grid_sga$exposure[i]
                                     y_name = difference_grid_sga$modifier[i]
                                     bind_rows(
                                       sbp007_contrast_fit(gee_saved_fit = sbsens001_output$affective_p5_output[[1]],x_name,y_name) %>% 
                                         mutate(exposure = x_name,
                                                modifier = y_name,
                                                exposure_value = 1,
                                                modifier_value = 1,
                                                outcome = "Weight_after",
                                                population = "affective"),
                                       sbp007_contrast_fit(gee_saved_fit = sbsens001_output$psychosis_p5_output[[1]],x_name,y_name) %>% 
                                         mutate(exposure = x_name,
                                                modifier = y_name,
                                                exposure_value = 1,
                                                modifier_value = 1,
                                                outcome = "Weight_after",
                                                population = "psychosis")
                                       
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
  write_csv(.,"sensitivity/sbsens001_contrasts for sga by disorder.csv")

