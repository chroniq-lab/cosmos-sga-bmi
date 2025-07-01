rm(list=ls());gc();source(".Rprofile")
source("analysis/sba_linear and logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)

analytic <- read_parquet(paste0(path_sga_bmi_folder,"/working/analytic sample.parquet")) %>% 
  dplyr::filter(exposure_binary==1)


younger_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp002/analytic sample with matchit.RDS")) %>% 
  dplyr::filter(!is.na(Weight_after))  %>% 
  dplyr::filter(age <= 75)


younger_n0 <- run_geeglm_bmi(f = formula_n0,
                               df=younger_df,type = "matchit")


younger_m0 <- run_geeglm_bmi(f = formula_m0,
                               df=younger_df,type = "matchit")


younger_p0 <- run_geeglm_bmi(f = formula_p0,
                               df=younger_df,type = "matchit")



younger_n5 <- run_geeglm_bmi(f = formula_n5,
                               df=younger_df,type = "matchit")


younger_m5 <- run_geeglm_bmi(f = formula_m5,
                               df=younger_df,type = "matchit")


younger_p5 <- run_geeglm_bmi(f = formula_p5,
                               df=younger_df,type = "matchit")



list(
  younger_n0_output = list(younger_n0, outcome = "pctchange_Weight_after_ge_minus5",model="n0",population = "younger"),
  younger_m0_output = list(younger_m0, outcome = "pctchange_Weight_after",model="m0",population = "younger"),
  younger_p0_output = list(younger_p0, outcome = "Weight_after",model="p0",population = "younger"),

  younger_n5_output = list(younger_n5, outcome = "pctchange_Weight_after_ge_minus5",model="n5",population = "younger"),
  younger_m5_output = list(younger_m5, outcome = "pctchange_Weight_after",model="m5",population = "younger"),
  younger_p5_output = list(younger_p5, outcome = "Weight_after",model="p5",population = "younger")
) %>% 
  saveRDS(.,paste0(path_sga_bmi_folder,"/working/sbsens004/geeglm binomial and linear regression output among younger.RDS"))


sbsens004_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sbsens004/geeglm binomial and linear regression output among younger.RDS"))

coefs_model <- bind_rows(
  
  tidy_save_geeglm(sbsens004_output$younger_n0[[1]],exponentiate = TRUE) %>% 
    mutate(outcome = sbsens004_output$younger_n0$outcome,
           model = sbsens004_output$younger_n0$model,
           population = sbsens004_output$younger_n0$population),
  
  tidy_save_geeglm(sbsens004_output$younger_m0[[1]],exponentiate = FALSE) %>% 
    mutate(outcome = sbsens004_output$younger_m0$outcome,
           model = sbsens004_output$younger_m0$model,
           population = sbsens004_output$younger_m0$population),
  
  tidy_save_geeglm(sbsens004_output$younger_p0_output[[1]],exponentiate = FALSE) %>%
    mutate(outcome = sbsens004_output$younger_p0_output$outcome,
           model = sbsens004_output$younger_p0_output$model,
           population = sbsens004_output$younger_p0$population)

)

coefs_model %>% 
  write_csv("sensitivity/sbsens004_regression coefficients of linear and binomial regression among younger.csv")





# Contrasts --------------------------
source("analysis/sba_difference grids.R")

source("H:/code/functions/imputation/contrasts_geeglm.R")
source("functions/sbp007_contrast_fit.R")

pctchange_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                  function(i){
                                    x_name = difference_grid_sga$exposure[i]
                                    y_name = difference_grid_sga$modifier[i]
                                    bind_rows(
                                      sbp007_contrast_fit(gee_saved_fit = sbsens004_output$younger_m5_output[[1]],x_name,y_name) %>% 
                                        mutate(exposure = x_name,
                                               modifier = y_name,
                                               exposure_value = 1,
                                               modifier_value = 1,
                                               outcome = "pctchange_Weight_after",
                                               population = "younger")
                                      
                                    )
                                    
                                  }
                                  
)

pct5change_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                   function(i){
                                     x_name = difference_grid_sga$exposure[i]
                                     y_name = difference_grid_sga$modifier[i]
                                     bind_rows(
                                       sbp007_contrast_fit(gee_saved_fit = sbsens004_output$younger_n5_output[[1]],x_name,y_name) %>% 
                                         mutate(exposure = x_name,
                                                modifier = y_name,
                                                exposure_value = 1,
                                                modifier_value = 1,
                                                outcome = "pctchange_Weight_after_ge_minus5",
                                                population = "younger")
                                       
                                     )
                                     
                                   }
                                   
)


weight_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                               function(i){
                                 x_name = difference_grid_sga$exposure[i]
                                 y_name = difference_grid_sga$modifier[i]
                                 bind_rows(
                                   sbp007_contrast_fit(gee_saved_fit = sbsens004_output$younger_p5_output[[1]],x_name,y_name) %>% 
                                     mutate(exposure = x_name,
                                            modifier = y_name,
                                            exposure_value = 1,
                                            modifier_value = 1,
                                            outcome = "Weight_after",
                                            population = "younger")
                                   
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
  write_csv(.,"sensitivity/sbsens004_contrasts for sga among younger.csv")

