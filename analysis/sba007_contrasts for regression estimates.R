rm(list=ls());gc();source(".Rprofile")

source("analysis/sba_difference grids.R")

sba003_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sba003/geeglm linear regression output.RDS"))
sba004_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sba004/geeglm binomial regression output.RDS"))
sba006_output <- readRDS(paste0(path_sga_bmi_folder,"/working/sba006/geeglm linear regression for weight output.RDS"))

source("H:/code/functions/imputation/contrasts_geeglm.R")

source("functions/sba007_contrast_fit.R")


pctchange_contrast_sga <- map_dfr(1:nrow(difference_grid_sga),
                                      function(i){
                                        x_name = difference_grid_sga$exposure[i]
                                        y_name = difference_grid_sga$modifier[i]
                                        bind_rows(
                                          sba007_contrast_fit(gee_saved_fit = sba003_output$m2_output[[1]],x_name,y_name) %>% 
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
                                sba007_contrast_fit(gee_saved_fit = sba004_output$n2_output[[1]],x_name,y_name) %>% 
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
                                sba007_contrast_fit(gee_saved_fit = sba006_output$p2_output[[1]],x_name,y_name) %>% 
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
  write_csv(.,"analysis/sba007_contrasts for sga.csv")


