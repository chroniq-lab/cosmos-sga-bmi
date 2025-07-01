
rm(list=ls());gc();source(".Rprofile")

# From rfa_analytic sample preprocessing.R
# source("analysis/atan002_analytic sample preprocessing.R")
analytic <- readRDS(paste0(path_sga_bmi_folder,"/working/sbp002/analytic sample with matchit.RDS")) %>% 
  dplyr::filter(is.na(Bmi_after)) 



# Table generation -------
library(gtsummary)

(sbp005e_descriptives <- analytic %>% 
    mutate(SviOverallPctlRankByZip2020_X = SviOverallPctlRankByZip2020_X*100) %>% 
    tbl_summary(by=exposure_binary,
                include = c(n_type_sga,n_all_sga,
                            type_sga,
                            age,female,raceeth,
                            
                            SviOverallPctlRankByZip2020_X,region,
                            
                            insurance_medicare,
                            insurance_other,
                            insurance_medicaid,
                            insurance_selfpay,
                            
                            AAPMonthYear_BodyMassIndex, 
                            AAPMonthYear_Weight,
                            Height_ever,
                            
                            
                            
                            schizoaffective,bipolar_manic,depressive_mdd,
                            schizophrenia,htn,obesity,delusional,cerebro,
                            hld,cardiovascular,pulmonary,schizotypal,
                            psychosis_unspecified ,
                            # mafld, 
                            pcos,
                            
                            statin,antidepressant,antihypertensive,other_antipsychotic,
                            other_antihyperlipid,
                            
                            Bmi_before,
                            Hba1c_before, Ldlc_before, 
                            Tgl_before, Hdlc_before, 
                            Sbp_before, Dbp_before,
                            
                            Bmi_after, Hba1c_after,ldl_after,tgl_after,hdl_after,Sbp_after, Dbp_after,
                            
                            neutro_before,lympho_before,neutleuk_before,lymphleuk_before,ratio_neutlymph_before,
                            neutro_after,lympho_after,neutleuk_after,lymphleuk_after,ratio_neutlymph_after,
                            
                            months_diff_AAP_Bmi,
                            
                            ValidatedStateOrProvince_X,PrimaryRUCA_X
                            
                            
                            
                            
                ),
                missing = "ifany",
                missing_text = "Missing",
                type = list(n_type_sga ~ "continuous2",
                            n_all_sga ~ "continuous2",
                            age ~ "continuous",
                            female ~ "dichotomous",
                            raceeth ~ "categorical",
                            
                            SviOverallPctlRankByZip2020_X ~ "continuous2",
                            region ~ "categorical",
                            insurance_medicare ~ "dichotomous",
                            insurance_other ~ "dichotomous",
                            insurance_medicaid ~ "dichotomous",
                            insurance_selfpay ~ "dichotomous",
                            
                            AAPMonthYear_BodyMassIndex ~ "continuous",
                            AAPMonthYear_Weight ~ "continuous",
                            Height_ever ~ "continuous",
                            
                            
                            schizoaffective ~ "dichotomous",bipolar_manic ~ "dichotomous",depressive_mdd ~ "dichotomous",
                            schizophrenia ~ "dichotomous",htn ~ "dichotomous",obesity ~ "dichotomous",delusional ~ "dichotomous",cerebro ~ "dichotomous",
                            hld ~ "dichotomous",cardiovascular ~ "dichotomous",pulmonary~ "dichotomous",schizotypal~ "dichotomous",
                            psychosis_unspecified ~ "dichotomous",
                            # mafld ~ "dichotomous", 
                            pcos ~ "dichotomous",
                            statin~ "dichotomous",antidepressant~ "dichotomous",antihypertensive~ "dichotomous",other_antipsychotic~ "dichotomous",
                            other_antihyperlipid~ "dichotomous",
                            
                            Bmi_before ~ "continuous",
                            Hba1c_before~ "continuous2", Ldlc_before~ "continuous", 
                            Tgl_before~ "continuous", Hdlc_before~ "continuous", 
                            Sbp_before~ "continuous", Dbp_before~ "continuous",
                            
                            Bmi_after~ "continuous", Hba1c_after~ "continuous2",ldl_after~ "continuous",tgl_after~ "continuous",
                            hdl_after~ "continuous",Sbp_after~ "continuous", Dbp_after~ "continuous",
                            
                            neutro_before~ "continuous2",lympho_before~ "continuous2",neutleuk_before~ "continuous2",
                            lymphleuk_before~ "continuous2",ratio_neutlymph_before~ "continuous2",
                            neutro_after~ "continuous2",lympho_after~ "continuous2",neutleuk_after~ "continuous2",
                            lymphleuk_after~ "continuous2",ratio_neutlymph_after~ "continuous2",
                            months_diff_AAP_Bmi ~ "continuous2",
                            ValidatedStateOrProvince_X ~ "categorical",PrimaryRUCA_X ~ "categorical"
                            
                ),
                digits = list(n_type_sga ~ c(1,1,1,1,1), n_all_sga~ c(1,1,1,1,1),
                              age ~ c(1,1),
                              SviOverallPctlRankByZip2020_X ~ c(1,1,1,1,1),
                              
                              AAPMonthYear_BodyMassIndex ~ c(1,1),
                              AAPMonthYear_Weight ~ c(1,1),
                              Height_ever ~ c(1,1),
                              Bmi_before ~ c(1,1),
                              Hba1c_before~ c(1,1,1,1,1), Ldlc_before~ c(1,1), 
                              Tgl_before~ c(1,1), Hdlc_before~ c(1,1), 
                              Sbp_before~ c(1,1), Dbp_before~ c(1,1),
                              
                              Bmi_after~ c(1,1), Hba1c_after~ c(1,1,1,1,1),ldl_after ~ c(1,1),tgl_after ~ c(1,1),
                              hdl_after ~ c(1,1),Sbp_after~ c(1,1), Dbp_after~ c(1,1),
                              
                              neutro_before~ c(1,1,1,1,1),lympho_before~ c(1,1,1,1,1),neutleuk_before~ c(1,1,1,1,1),
                              lymphleuk_before~ c(1,1,1,1,1),ratio_neutlymph_before~ c(1,1,1,1,1),
                              neutro_after~ c(1,1,1,1,1),lympho_after~ c(1,1,1,1,1),neutleuk_after~ c(1,1,1,1,1),
                              lymphleuk_after~ c(1,1,1,1,1),ratio_neutlymph_after~ c(1,1,1,1,1),
                              months_diff_AAP_Bmi ~ c(1,1,1,1,1)
                              
                ),
                statistic = list(all_continuous() ~ "{mean} ({sd})",
                                 all_continuous2() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))
    ) %>% 
    add_n() %>% 
    add_overall()) %>%
  as_gt() %>%
  gt::gtsave(filename = "psm/sbp005e_unweighted descriptive characteristics for excluded.html")

