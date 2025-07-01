rm(list=ls());gc();source(".Rprofile")
library(tidymodels)


analytic_sample <- read_parquet(paste0(path_sga_bmi_folder,"/working/analytic sample.parquet")) 


analytic_sample_imputed = recipe(exposure_binary ~ .,
                                 data = analytic_sample) %>% 
  
  update_role(PatientDurableKey,AAPMonthYearKey,
              YearMonthKey_Hba1c_before,
              YearMonthKey_Tgl_before,
              YearMonthKey_Ldlc_before,
              YearMonthKey_Hdlc_before,
              YearMonthKey_Sbp_before,
              YearMonthKey_Bmi_before,
              new_role="ID") %>% 
  
  # https://stackoverflow.com/questions/67963439/error-warning-there-are-new-levels-in-a-factor-na
  # https://github.com/tidymodels/recipes/issues/756 
  # There might be scenarios where it is more reasonable to impute missing values per group/subsample.
  step_impute_linear(any_of(c("SviOverallPctlRankByZip2020_X")),
                     impute_with = imp_vars(one_of(c("female","age","raceeth",
                                                     "type_sga",
                                                     # "Bmi_before","Bmi_after",
                                                     "insurance_medicare","insurance_medicaid","insurance_other","insurance_selfpay",
                                                     
                                                     "schizoaffective","bipolar_manic","depressive_mdd",
                                                     "schizophrenia","htn","obesity","delusional","cerebro",
                                                     "hld","cardiovascular","pulmonary","schizotypal",
                                                     "statin","antidepressant","antihypertensive","other_antipsychotic",
                                                     "other_antihyperlipid"))))  %>%
  prep(.,training = analytic_sample) %>% 
  # bake(): For a recipe with at least one preprocessing operation that has been trained by prep(), apply the computations to new data.
  bake(.,new_data=analytic_sample) 

analytic_sample_imputed  %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey,SviOverallPctlRankByZip2020_X) %>% 
  rename(SviOverallPctlRankByZip2020_X_imputed = SviOverallPctlRankByZip2020_X)  %>% 
  mutate(SviOverallPctlRankByZip2020_X_imputed = case_when(is.na(SviOverallPctlRankByZip2020_X_imputed) ~ mean(SviOverallPctlRankByZip2020_X_imputed,na.rm=TRUE),
                                                           TRUE ~ SviOverallPctlRankByZip2020_X_imputed)) %>% 
  write_parquet(.,paste0(path_sga_bmi_folder,"/working/analytic sample imputed.parquet"))

