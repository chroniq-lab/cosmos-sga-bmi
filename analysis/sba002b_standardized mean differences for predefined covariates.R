rm(list=ls());gc();source(".Rprofile")

analytic_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS"))

source("H:/code/functions/causality/standardized_difference.R")

p_vars = c("female","insurance_medicare",
           "insurance_other",
           "insurance_medicaid",
           "insurance_selfpay",
           "schizoaffective","bipolar_manic","depressive_mdd",
           "schizophrenia","htn","obesity","delusional","cerebro",
           "hld","cardiovascular","pulmonary","schizotypal",
           "psychosis_unspecified" ,
           # mafld, 
           "pcos",
           
           "statin","antidepressant","antihypertensive","other_antipsychotic",
           "other_antihyperlipid")

c_vars = c("age","n_type_sga","n_all_sga",
           "SviOverallPctlRankByZip2020_X",
           "AAPMonthYear_BodyMassIndex",
           "AAPMonthYear_Weight",
           "Height_ever",
           "Bmi_before",
           "Hba1c_before", "Ldlc_before", 
           "Tgl_before", "Hdlc_before", 
           "Sbp_before", "Dbp_before",
           "neutro_before","lympho_before","neutleuk_before","lymphleuk_before","ratio_neutlymph_before")

g_vars = c("raceeth","type_sga","region")


smd = map_dfr(c(c_vars,p_vars,g_vars),
                function(c_v){
                  print(c_v);
                  standardized_difference(x_variable=analytic_df %>% dplyr::select(one_of(c_v)) %>% pull(),
                                          a_variable=analytic_df$exposure_binary,
                                          w_variable =analytic_df$sipw) %>% 
                    mutate(variable = c_v) %>% 
                    return(.)
                })

  write_csv(smd,file="analysis/sba002b_standardized mean differences.csv.csv")
  
