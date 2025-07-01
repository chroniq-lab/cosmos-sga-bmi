rm(list=ls());gc();source(".Rprofile")

source("analysis/sba_difference grids.R")

sbp003_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp003/geeglm linear regression output.RDS"))
sbp004_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp004/geeglm binomial regression output.RDS"))
sbp006_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp006/geeglm linear regression for weight output.RDS"))

source("H:/code/functions/imputation/contrasts_geeglm.R")

source("functions/sbp007_contrast_fit.R")


pctchange_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                      function(i){
                                        x_name = difference_grid_sga$exposure[i]
                                        y_name = difference_grid_sga$modifier[i]
                                        bind_rows(
                                          sbp007_contrast_fit(gee_saved_fit = sbp003_output$m2_output[[1]],x_name,y_name) %>% 
                                            mutate(exposure = x_name,
                                                   modifier = y_name,
                                                   exposure_value = 1,
                                                   modifier_value = 1,
                                                   outcome = "pctchange_Weight_after")
                                          
                                        )
                                        
                                      }
                                      
)

pct5change_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                            function(i){
                              x_name = difference_grid_sga$exposure[i]
                              y_name = difference_grid_sga$modifier[i]
                              bind_rows(
                                sbp007_contrast_fit(gee_saved_fit = sbp004_output$n2_output[[1]],x_name,y_name) %>% 
                                  mutate(exposure = x_name,
                                         modifier = y_name,
                                         exposure_value = 1,
                                         modifier_value = 1,
                                         outcome = "pctchange_Weight_after_ge_minus5")
                                
                              )
                              
                            }
                            
)


weight_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                            function(i){
                              x_name = difference_grid_sga$exposure[i]
                              y_name = difference_grid_sga$modifier[i]
                              bind_rows(
                                sbp007_contrast_fit(gee_saved_fit = sbp006_output$p2_output[[1]],x_name,y_name) %>% 
                                  mutate(exposure = x_name,
                                         modifier = y_name,
                                         exposure_value = 1,
                                         modifier_value = 1,
                                         outcome = "Weight_after")
                                
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
  write_csv(.,"psm/sbp007_contrasts for sga.csv")


