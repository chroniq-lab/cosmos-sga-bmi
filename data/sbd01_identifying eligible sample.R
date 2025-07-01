aap_t2dm_analytic <- read_parquet(paste0(path_aap_t2dm_folder,"/working/analytic sample.parquet")) 


View(aap_t2dm_analytic %>% 
       dplyr::select(PatientDurableKey,type_sga,AAPMonthYearKey,YearMonthKey_Bmi_before,YearMonthKey_Bmi_after,
                     AAPMonthYear_BodyMassIndex,Bmi_before,Bmi_after,
                     metformin,glp1ra,other_glp1ra,other_wlrx))

