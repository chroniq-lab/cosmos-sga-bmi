dx_rx = c("schizoaffective","bipolar_manic","depressive_mdd",
          "schizophrenia","htn","obesity","delusional","cerebro",
          "hld","cardiovascular","pulmonary","schizotypal","psychosis_unspecified",
          "statin","antidepressant","antihypertensive","other_antipsychotic",
          "other_antihyperlipid")

covariates = paste0(" + type_sga + n_all_sga + region + SviOverallPctlRankByZip2020_X_imputed + age + female  + 
                      insurance_medicare + insurance_medicaid + insurance_other + factor(ValidatedStateOrProvince_X)"," + ",paste0(dx_rx,collapse=" + "))

covariates_minus_sex = paste0(" + type_sga + n_all_sga + region + SviOverallPctlRankByZip2020_X_imputed + age + 
                      insurance_medicare + insurance_medicaid + insurance_other + factor(ValidatedStateOrProvince_X)"," + ",paste0(dx_rx,collapse=" + "))


outcome_bmi = "Bmi_after ~ "
outcome_pctchangeweight = "pctchange_Weight_after ~ "
outcome_pct5weightchange = "pctchange_Weight_after_ge_minus5 ~ "
outcome_weight = "Weight_after ~ "

exposure_main = "exposure_binary + months_diff_WL_Bmi" # Previously months_diff_AAP_Bmi
exposure_time = "exposure_binary*months_diff_WL_Bmi"
exposure_sga = "exposure_binary*type_sga + months_diff_WL_Bmi"
exposure_sga_time = "exposure_binary*type_sga*months_diff_WL_Bmi"
exposure_secondary = "exposure_category + months_diff_WL_Bmi"
exposure_duration = "exposure_duration + months_diff_WL_Bmi"


formula_m0 = paste0(outcome_pctchangeweight,exposure_main," + AAPMonthYear_Weight")
formula_m3 = paste0(outcome_pctchangeweight,exposure_main," + AAPMonthYear_Weight",covariates)
formula_m1 = paste0(outcome_pctchangeweight,exposure_time," + AAPMonthYear_Weight")

formula_m2 = paste0(outcome_pctchangeweight,exposure_sga," + AAPMonthYear_Weight")
formula_m5 = paste0(outcome_pctchangeweight,exposure_sga," + AAPMonthYear_Weight",covariates)
formula_m5b = paste0(outcome_pctchangeweight,exposure_sga," + AAPMonthYear_Weight",covariates_minus_sex)

formula_m4 = paste0(outcome_pctchangeweight,exposure_sga_time," + AAPMonthYear_Weight")

formula_m6 = paste0(outcome_pctchangeweight,exposure_secondary," + AAPMonthYear_Weight")
formula_m7 = paste0(outcome_pctchangeweight,exposure_secondary," + AAPMonthYear_Weight",covariates)

formula_m8 = paste0(outcome_pctchangeweight,exposure_duration," + AAPMonthYear_Weight",covariates)


formula_n0 = paste0(outcome_pct5weightchange,exposure_main," + AAPMonthYear_Weight")
formula_n1 = paste0(outcome_pct5weightchange,exposure_time," + AAPMonthYear_Weight")
formula_n2 = paste0(outcome_pct5weightchange,exposure_sga," + AAPMonthYear_Weight")
formula_n3 = paste0(outcome_pct5weightchange,exposure_main," + AAPMonthYear_Weight",covariates)
formula_n4 = paste0(outcome_pct5weightchange,exposure_sga_time," + AAPMonthYear_Weight")
formula_n5 = paste0(outcome_pct5weightchange,exposure_sga," + AAPMonthYear_Weight",covariates)
formula_n5b = paste0(outcome_pct5weightchange,exposure_sga," + AAPMonthYear_Weight",covariates_minus_sex)
formula_n6 = paste0(outcome_pct5weightchange,exposure_secondary," + AAPMonthYear_Weight")
formula_n7 = paste0(outcome_pct5weightchange,exposure_secondary," + AAPMonthYear_Weight",covariates)
formula_n8 = paste0(outcome_pct5weightchange,exposure_duration," + AAPMonthYear_Weight",covariates)

formula_p0 = paste0(outcome_weight,exposure_main," + AAPMonthYear_Weight")
formula_p1 = paste0(outcome_weight,exposure_time," + AAPMonthYear_Weight")
formula_p2 = paste0(outcome_weight,exposure_sga," + AAPMonthYear_Weight")
formula_p3 = paste0(outcome_weight,exposure_main," + AAPMonthYear_Weight",covariates)
formula_p4 = paste0(outcome_weight,exposure_sga_time," + AAPMonthYear_Weight")
formula_p5 = paste0(outcome_weight,exposure_sga," + AAPMonthYear_Weight",covariates)
formula_p5b = paste0(outcome_weight,exposure_sga," + AAPMonthYear_Weight",covariates_minus_sex)
formula_p6 = paste0(outcome_weight,exposure_secondary," + AAPMonthYear_Weight")
formula_p7 = paste0(outcome_weight,exposure_secondary," + AAPMonthYear_Weight",covariates)
formula_p8 = paste0(outcome_weight,exposure_duration," + AAPMonthYear_Weight",covariates)

