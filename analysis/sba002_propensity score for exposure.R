rm(list=ls());gc();source(".Rprofile")

source("analysis/sba_linear and logistic regression equations.R")
analytic_sample_imputed <- read_parquet(paste0(path_sga_bmi_folder,"/working/analytic sample imputed.parquet"))

analytic_sample <- read_parquet(paste0(path_sga_bmi_folder,"/working/analytic sample.parquet")) %>% 
  left_join(analytic_sample_imputed,
            by = c("PatientDurableKey","AAPMonthYearKey")) %>% 
  mutate(Bmi_available = case_when(!is.na(Bmi_after) ~ 1,
                                     TRUE ~ 0)
         )

mean(analytic_sample$Bmi_available)

ps_equation <- paste0("exposure_binary ~ AAPMonthYear_BodyMassIndex",covariates)
ltfu_Bmi_equation <- paste0("Bmi_available ~ exposure_binary + AAPMonthYear_BodyMassIndex",covariates)


ps_model <- glm(ps_equation,data=analytic_sample,family = binomial())
ltfu_Bmi_model <- glm(ltfu_Bmi_equation,data=analytic_sample,family = binomial())

analytic_sample$prob_exposed = predict(ps_model,newdata=analytic_sample,type="response")
analytic_sample$prob_Bmi_followup = predict(ltfu_Bmi_model,newdata=analytic_sample,type="response")

rm(ps_model,ltfu_Bmi_model);gc()

# prob_e = mean(analytic_sample$exposure_binary)

analytic_df = analytic_sample %>% 
  mutate(mean_e = mean(exposure_binary)) %>% 
  group_by(type_sga) %>% 
  mutate(mean_e_sga = mean(exposure_binary)) %>% 
  ungroup() %>% 
  mutate(sipw = case_when(exposure_binary == 1 ~ mean_e/prob_exposed,
                         TRUE ~ (1-mean_e)/(1-prob_exposed)),
         sipw_att = case_when(exposure_binary == 1 ~ 1,
                             TRUE ~ mean_e/(1-prob_exposed)),
         sipw_sga = case_when(exposure_binary == 1 ~ mean_e_sga/prob_exposed,
                              TRUE ~ (1-mean_e_sga)/(1-prob_exposed)),
         ipw = case_when(exposure_binary == 1 ~ 1/prob_exposed,
                         TRUE ~ 1/(1-prob_exposed)),
         ipcw_Bmi = case_when(Bmi_available == 1 ~ 1/prob_Bmi_followup,
                          TRUE ~ 1/(1-prob_Bmi_followup))) %>% 
  mutate(stabilizedweights_Bmi = sipw*ipcw_Bmi,
         stabilizedweights_Bmi_sga = sipw_sga*ipcw_Bmi,
         weights_Bmi = ipw*ipcw_Bmi) %>% 
  # Using cleaned data - pctchange calculation to remove outliers is presented in sba_analytic sample processing.R
  mutate(pctchange_Weight_after = (Weight_after - AAPMonthYear_Weight)/AAPMonthYear_Weight) %>% 
  mutate(pctchange_Weight_after_ge_minus5 = case_when(pctchange_Weight_after <= -0.05 ~ 1,
                                                !is.na(pctchange_Weight_after) ~ 0,
                                                TRUE ~ NA_real_))

summary(analytic_df$sipw)
summary(analytic_df$ipw)
summary(analytic_df$ipcw_Bmi)

saveRDS(analytic_df,paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS"))
write_parquet(analytic_df,paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet"))


