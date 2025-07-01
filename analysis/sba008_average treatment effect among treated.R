rm(list=ls());gc();source(".Rprofile")
source("analysis/sba_logistic regression equations.R")
source("functions/run_geeglm_bmi.R")
library(geepack)
# Bmi_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 
Weight_after_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS")) %>% 
  dplyr::filter(!is.na(Weight_after)) 


# Total ---------
model_m0 = geeglm(as.formula(formula_m0),family = gaussian(),
                  weights = stabilizedweights_Bmi,
                  data=Weight_after_df,id=PatientDurableKey,corstr="exchangeable")

model_n0 = geeglm(as.formula(formula_n0),family = poisson(),
                  weights = stabilizedweights_Bmi,
                  data=Weight_after_df,id=PatientDurableKey,corstr="exchangeable")

model_p0 = geeglm(as.formula(formula_p0),family = gaussian(),
                  weights = stabilizedweights_Bmi,
                  data=Weight_after_df,id=PatientDurableKey,corstr="exchangeable")

model_m2 = geeglm(as.formula(formula_m2),family = gaussian(),
                  weights = stabilizedweights_Bmi_sga,
                  data=Weight_after_df,id=PatientDurableKey,corstr="exchangeable")

model_n2 = geeglm(as.formula(formula_n2),family = poisson(),
                  weights = stabilizedweights_Bmi_sga,
                  data=Weight_after_df,id=PatientDurableKey,corstr="exchangeable")

model_p2 = geeglm(as.formula(formula_p2),family = gaussian(),
                  weights = stabilizedweights_Bmi_sga,
                  data=Weight_after_df,id=PatientDurableKey,corstr="exchangeable")


source("H:/code/functions/causality/att.R")



output_df = map_dfr(c("total","low","intermediate","high"),
                    
                    function(x){
                      sga_levels = c("low","intermediate","high")
                      
                      if(x == "total"){
                        sga_levels = sga_levels
                        model_m = model_m0
                        model_n = model_n0
                        model_p = model_p0
                      }else{sga_levels = x
                      model_m = model_m2
                      model_n = model_n2
                      model_p = model_p2
                      }

                      treated_df = Weight_after_df %>% 
                        dplyr::filter(exposure_binary == 1) %>% 
                        dplyr::filter(type_sga %in% sga_levels)
                      
                      baseline_m0 = Hmisc::wtd.mean(treated_df$AAPMonthYear_BodyMassIndex,weights=treated_df$stabilizedweights_Bmi)
                      baseline_p0 = Hmisc::wtd.mean(treated_df$AAPMonthYear_Weight,weights=treated_df$stabilizedweights_Bmi)
                      
                      final_m0 = Hmisc::wtd.mean(treated_df$Bmi_after,weights=treated_df$stabilizedweights_Bmi)
                      final_p0 = Hmisc::wtd.mean(treated_df$Weight_after,weights=treated_df$stabilizedweights_Bmi)
                      
                      result_m0 = att_estimation(df_treated = treated_df,model_obj = model_m,exposure_var = "exposure_binary",weight_var = "stabilizedweights_Bmi")
                      result_n0 = att_estimation(df_treated = treated_df,model_obj = model_n,exposure_var = "exposure_binary",weight_var = "stabilizedweights_Bmi")
                      result_p0 = att_estimation(df_treated = treated_df,model_obj = model_p,exposure_var = "exposure_binary",weight_var = "stabilizedweights_Bmi")
                      
                                            
                      out = bind_rows(result_m0 %>% 
                                        mutate(baseline = baseline_m0,
                                               final = final_m0,
                                               outcome = "Bmi_after"),
                                      result_n0 %>% 
                                        mutate(baseline = NA_real_,
                                               final = NA_real_,
                                               outcome = "pctchange_Weight_after_ge_minus5"),
                                      result_p0 %>% 
                                        mutate(baseline = baseline_p0,
                                               final = final_p0,
                                               outcome = "Weight_after")
                                      ) %>% 
                        mutate(level = x)
                      
                      return(out)
                      
                      
                      
                    })

write_csv(output_df,"analysis/sba008_average treatment effect among treated.csv")

