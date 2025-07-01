m0 <- geeglm(formula = as.formula(m0),data=Bmi_after_df,corstr="exchangeable",
             weights=stabilizedweights_Bmi,id=PatientDurableKey)
m1 <- geeglm(formula = as.formula(m1),data=Bmi_after_df,corstr="exchangeable",
             weights=stabilizedweights_Bmi,id=PatientDurableKey)
m2 <- geeglm(formula = as.formula(m2),data=Bmi_after_df,corstr="exchangeable",
             weights=stabilizedweights_Bmi,id=PatientDurableKey)
m3 <- geeglm(formula = as.formula(m3),data=Bmi_after_df,corstr="exchangeable",
             weights=stabilizedweights_Bmi,id=PatientDurableKey)



bind_rows(
  broom::tidy(m0) %>% mutate(type = "IPW",model="M0"),
  broom::tidy(m1) %>% mutate(type = "IPW",model="M1"),
  broom::tidy(m2) %>% mutate(type = "IPW",model="M2"),
  broom::tidy(m3) %>% mutate(type = "IPW",model="M3")
) %>% 
  write_csv(.,"analysis/sba003_linear regression coefficients.csv")


library(geepack)
Weight_after_df <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.parquet")) %>% 
  dplyr::filter(!is.na(pctchange_Weight_after_ge_minus5)) 



n0 <- geeglm(formula=as.formula(formula_n0),data=Weight_after_df,corstr="exchangeable",
             weights=stabilizedweights_Bmi,id=PatientDurableKey,family=binomial())

n1 <- geeglm(formula=as.formula(formula_n0),data=Weight_after_df,corstr="exchangeable",
             weights=stabilizedweights_Bmi,id=PatientDurableKey,family=binomial())

n2 <- geeglm(formula=as.formula(formula_n0),data=Weight_after_df,corstr="exchangeable",
             weights=stabilizedweights_Bmi,id=PatientDurableKey,family=binomial())
summary(n0)

bind_rows(
  broom::tidy(n0) %>% mutate(type = "IPW",model="N0"),
  broom::tidy(n1) %>% mutate(type = "IPW",model="N1"),
  broom::tidy(n2) %>% mutate(type = "IPW",model="N2"),
  broom::tidy(n3) %>% mutate(type = "IPW",model="N3")
) %>% 
  write_csv(.,"analysis/sba004_logistic regression coefficients.csv")

