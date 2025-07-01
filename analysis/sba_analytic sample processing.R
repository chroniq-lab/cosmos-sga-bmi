rm(list=ls());gc();source(".Rprofile")

unexposed_wlrx <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/unexposed to prescriptions wlrx.parquet"))

# EXPOSURE ------------
bmi_on <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/bmi and weight for AAPMonthYearKey.parquet")) %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey,Weight_kg)

height_ever = read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/height ever for AAPMonthYearKey.parquet"))

closest_wlrx_by_sga <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/closest_prescriptions wlrx.parquet")) %>% 
  dplyr::filter(type_wl == "glp1ra") %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey,diff_AAP_WL,WLYearMonthKey,WLgenericnames,WLprescriptionduration,maxWLprescriptionduration,WLprescribers) %>% 
  mutate(maxWLprescriptionduration_edited = case_when(maxWLprescriptionduration == 0 ~ 1,
                                               is.na(maxWLprescriptionduration) ~ 1,
                                               TRUE ~ maxWLprescriptionduration))

other_wlrx_on_AAP <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/closest_prescriptions wlrx.parquet")) %>% 
  dplyr::filter(type_wl != "glp1ra") %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey,diff_AAP_WL,WLYearMonthKey,WLgenericnames,WLprescriptionduration,maxWLprescriptionduration,WLprescribers) %>% 
  inner_join(closest_wlrx_by_sga %>% 
              dplyr::select(PatientDurableKey,AAPMonthYearKey),
             by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  distinct(PatientDurableKey,AAPMonthYearKey)

# OUTCOME ---------
predictors_after <- bind_rows(
  read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/earliest predictors 6months after exposure date.parquet")),
  read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/earliest predictors 6months after unexposed date.parquet"))) %>% 
  
  inner_join(
    bind_rows(closest_wlrx_by_sga %>% dplyr::select(PatientDurableKey,AAPMonthYearKey),
              unexposed_wlrx %>% dplyr::select(PatientDurableKey,AAPMonthYearKey)),
    by=c("PatientDurableKey","AAPMonthYearKey")
    
    
  )

sga_predictors = open_dataset(paste0(path_aap_t2dm_folder,"/working/atan001/prescribers of exposure.parquet")) %>% 
  inner_join(predictors_after,
             by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  collect() %>% 
  left_join(bmi_on %>% 
              rename(AAPMonthYear_Weight = Weight_kg),
            by=c("PatientDurableKey","AAPMonthYearKey"))


# COVARIATES ----------

predictors_before <- read_parquet(paste0(path_aap_t2dm_folder,"/working/atan001/predictors 1y before exposure date.parquet")) %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey,contains("before"))

insurance_before <- read_parquet(paste0(path_aap_t2dm_folder,"/working/atan001/insurance before exposure date.parquet"))  %>% 
  mutate(INS_ymd = ymd(INSYearMonthKey),
         AAP_ymd = ymd(AAPMonthYearKey)) %>% 
  mutate(diff_insurance_months = as.numeric(difftime(AAP_ymd,INS_ymd,units = "days"))/30.5) %>% 
  dplyr::filter(diff_insurance_months <= 2) %>% 
  dplyr::select(-INS_ymd,-AAP_ymd)

diagnosis_before <- read_parquet(paste0(path_aap_t2dm_folder,"/working/atan001/diagnosis before exposure date.parquet")) %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey,dx) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from=dx,values_from = value,values_fill = 0)

comorbidity_rx_before <- read_parquet(paste0(path_aap_t2dm_folder,"/working/atan001/closest_comorbidity prescriptions before sga.parquet")) %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey,rx) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from=rx,values_from = value,values_fill = 0)

demographics <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atdat03"),format="parquet",partitioning = "ValidatedStateOrProvince_X") %>% 
  dplyr::filter(PatientDurableKey %in% predictors_after$PatientDurableKey) %>% 
  mutate(BirthDate = ymd(BirthDate),
         PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  collect() %>% 
  mutate(region = case_when(PrimaryRUCA_X %in% as.character(c(1:6)) ~ "Urban",
                            PrimaryRUCA_X %in% as.character(c(7:10)) ~ "Rural",
                            TRUE ~ "Urban")) %>% 
  mutate(raceeth = case_when(Ethnicity == "Hispanic or Latino" ~ 3,
                             FirstRace == "Black or African American" | 
                               SecondRace == "Black or African American" | 
                               ThirdRace == "Black or African American" ~ 2,
                             FirstRace == "White" & SecondRace == "" ~ 1,
                             TRUE ~ 4),
         female = case_when(Sex == "Female" ~ 1,
                            TRUE ~ 0)) %>% 
  dplyr::select(-Ethnicity,-FirstRace,-SecondRace,-ThirdRace)

# ANALYTIC SAMPLE -------------

wlrx_vars = c("metformin","glp1ra","other_glp1ra","other_wlrx")

analytic_sample = sga_predictors %>% 
  mutate(AAPMonthYearKey_ymd = ymd(AAPMonthYearKey)) %>% 
  left_join(closest_wlrx_by_sga,
            by=c("PatientDurableKey","AAPMonthYearKey"))  %>% 
  
  mutate(exposure_binary = case_when(is.na(WLgenericnames) ~ 0,
                                    TRUE ~ 1),
         exposure_category = case_when(str_detect(WLgenericnames,"semaglutide") & str_detect(WLgenericnames,"tirzepatide") ~ 4,
                                      str_detect(WLgenericnames,"tirzepatide") ~ 3,
                                      str_detect(WLgenericnames,"semaglutide") ~ 2,
                                      TRUE ~ 1
                                      )) %>% 
  mutate(exposure_category = factor(exposure_category,
                                   levels=c(1:4),
                                   labels=c("none","semaglutide","tirzepatide","both"))) %>% 
  
  left_join(predictors_before ,
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
   mutate(ratio_neutlymph_before = case_when(!is.na(lympho_before) & lympho_before > 0 ~ neutro_before/lympho_before,
                                            !is.na(lymphleuk_before) & lymphleuk_before > 0 ~ neutleuk_before/lymphleuk_before,
                                            TRUE ~ NA_real_),
         ratio_neutlymph_after = case_when(!is.na(lympho_after) & lympho_after > 0 ~ neutro_after/lympho_after,
                                           !is.na(lymphleuk_after) & lymphleuk_after > 0 ~ neutleuk_after/lymphleuk_after,
                                           TRUE ~ NA_real_)
  ) %>% 
  
  left_join(insurance_before,
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(diagnosis_before,
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(comorbidity_rx_before,
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  mutate(across(one_of("insurance_medicare","insurance_medicaid","insurance_other","insurance_selfpay",
                       "schizoaffective","bipolar_manic","depressive_mdd",
                       "schizophrenia","htn","obesity","delusional","cerebro",
                       "hld","cardiovascular","pulmonary","schizotypal",
                       "psychosis_unspecified","pcos",
                       "statin","antidepressant","antihypertensive","other_antipsychotic",
                       "other_antihyperlipid"
  ),.f=function(x) case_when(is.na(x) ~ 0,
                             TRUE ~ x)))  %>% 
  
  left_join(demographics,
            by=c("PatientDurableKey")) %>% 
  dplyr::filter(SourceCountry_X == "United States of America")  %>% 
  mutate(age = as.numeric(difftime(AAPMonthYearKey_ymd,BirthDate,units="days")/365.25))  %>% 
  dplyr::filter(!ValidatedStateOrProvince_X %in% c("*Masked","*Unspecified","Northern Mariana Islands",
                                                   "Virgin Islands","American Samoa, South Pacific","Puerto Rico",
                                                   "Guam")) %>%
  dplyr::filter(age >= 18, age <100) %>% 
  mutate(raceeth = factor(raceeth,levels=c(1:4),labels=c("NHWhite","NHBlack","Hispanic","NHOther")),
         type_sga = factor(type_sga,levels=c("low","intermediate","high"))) %>% 
  mutate(year = as.character(AAPMonthYearKey %/% 10000)) %>%
  left_join(height_ever,
            by=c("PatientDurableKey")) %>% 
  # Imputing heights based on available 
  mutate(Height_before = case_when(is.na(Height_before) & !is.na(Height_ever) ~ Height_ever,
                                   is.na(Height_before) & !is.na(AAPMonthYear_BodyMassIndex) & !is.na(AAPMonthYear_Weight) ~ 100*sqrt(AAPMonthYear_BodyMassIndex/AAPMonthYear_Weight),
                                   is.na(Height_before) & !is.na(Bmi_after) & !is.na(Weight_after) ~ 100*sqrt(Bmi_after/Weight_after),
                                   is.na(Height_before) & !is.na(Bmi_before) & !is.na(Weight_before) ~ 100*sqrt(Bmi_before/Weight_before),
                                   TRUE ~ Height_before),
         Height_after = case_when(is.na(Height_after) & !is.na(Height_ever) ~ Height_ever,
                                   is.na(Height_after) & !is.na(AAPMonthYear_BodyMassIndex) & !is.na(AAPMonthYear_Weight) ~ 100*sqrt(AAPMonthYear_BodyMassIndex/AAPMonthYear_Weight),
                                   is.na(Height_after) & !is.na(Bmi_after) & !is.na(Weight_after) ~ 100*sqrt(Bmi_after/Weight_after),
                                   is.na(Height_after) & !is.na(Bmi_before) & !is.na(Weight_before) ~ 100*sqrt(Bmi_before/Weight_before),
                                   TRUE ~ Height_after)
         ) %>% 
  mutate(AAPMonthYear_Weight = case_when(is.na(AAPMonthYear_Weight) & !is.na(Height_before) ~ AAPMonthYear_BodyMassIndex*((Height_before/100)^2),
                                         is.na(AAPMonthYear_Weight) & !is.na(Height_after) ~ AAPMonthYear_BodyMassIndex*((Height_after/100)^2),
                                         TRUE ~ AAPMonthYear_Weight),
         Weight_after = case_when(is.na(Weight_after) & !is.na(Height_after) & !is.na(Bmi_after) ~ Bmi_after*((Height_after/100)^2),
                                  is.na(Weight_after) & !is.na(Height_before) & !is.na(Bmi_after) ~ Bmi_after*((Height_before/100)^2),
                                  TRUE ~ Weight_after),
         Weight_before = case_when(is.na(Weight_before) & !is.na(Height_before) & !is.na(Bmi_before) ~ Bmi_before*((Height_before/100)^2),
                                  is.na(Weight_before) & !is.na(Height_after) & !is.na(Bmi_before) ~ Bmi_before*((Height_after/100)^2),
                                  TRUE ~ Weight_before),
         ) %>% 
  
  ## Additional restrictions for cleaner analysis -------
  
  # Need baseline weight
  dplyr::filter(!is.na(AAPMonthYear_Weight)) %>% 
  # Need weight to be within 181 days and 272 days
  mutate(Weight_after = case_when(diff_AAP_Bmi > (181+ (30.5*3)) ~ NA_real_,
                                  TRUE ~ Weight_after),
         Bmi_after = case_when(diff_AAP_Bmi > (181+ (30.5*3)) ~ NA_real_,
                               
                               TRUE ~ Bmi_after)) %>% 
  

  # Treat both relative to 6 months
  mutate(months_diff_AAP_Bmi = case_when(diff_AAP_Bmi > (181+ (30.5*3)) ~ NA_real_,
                                         is.na(Bmi_after) ~ NA_real_,
                                         TRUE ~ (diff_AAP_Bmi-181)/30.5),
         months_diff_WL_Bmi = case_when(diff_AAP_Bmi > (181+ (30.5*3)) ~ NA_real_,
                                        is.na(Bmi_after) ~ NA_real_,
                                        exposure_binary == 0 ~ months_diff_AAP_Bmi, # For Unexposed
                                        TRUE ~ ((diff_AAP_Bmi-diff_AAP_WL)/30.5)-6)
         )

table(is.na(analytic_sample$AAPMonthYear_BodyMassIndex),is.na(analytic_sample$AAPMonthYear_Weight))
table(is.na(analytic_sample$Bmi_after),is.na(analytic_sample$Weight_after))



analytic_sample  %>%
  write_parquet(.,paste0(path_sga_bmi_folder,"/working/analytic sample.parquet"))

table(is.na(analytic_sample$AAPMonthYear_BodyMassIndex),is.na(analytic_sample$AAPMonthYear_Weight))
table(is.na(analytic_sample$Bmi_after),is.na(analytic_sample$Weight_after))
table(analytic_sample$exposure_binary)
table(analytic_sample$exposure_category)
analytic_sample %>% 
  dplyr::select(contains("Weight")) %>% 
  View()

