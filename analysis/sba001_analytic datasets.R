rm(list=ls());gc();source(".Rprofile")
exposure_arrow = open_dataset(paste0(path_aap_t2dm_folder,"/working/atan001/prescribers of exposure.parquet"))

# WEIGHT ON AAPMonthYearKey ---------
# This is not available in the exposure dataset since AAPMonthYearKey was decided exclusively based on BMI
bmi_on = open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(exposure_arrow %>% 
               mutate(AAPMonthYearKey_ymd = ymd(AAPMonthYearKey,tz=NULL)),
             by = c("PatientDurableKey")) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >60 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  mutate(Weight_kg = case_when(is.na(BodyMassIndex) ~ NA_real_,
                               TRUE ~ Weight*0.453592),
         Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  # 0|1|2|3|4|5|
  # 0 to dmonths(5)
  dplyr::filter(AAPMonthYearKey == YearMonthKey) %>%   
  collect() 

write_parquet(bmi_on,paste0(path_sga_bmi_folder,"/working/sba001/bmi and weight for AAPMonthYearKey.parquet"))
rm(bmi_on)


height_ever_history = open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh01a"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  dplyr::filter(Height >= 3*12, Height <= 7*12) %>% 
  mutate(Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey) %>% 
  summarize(Height_ever = mean(Height_cm)) %>% 
  # 0|1|2|3|4|5|
  # 0 to dmonths(5)
  collect() 

height_ever_followup = open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey)) %>% 
  dplyr::filter(Height >= 3*12, Height <= 7*12) %>% 
  mutate(Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  group_by(PatientDurableKey) %>% 
  summarize(Height_ever = mean(Height_cm)) %>% 
  # 0|1|2|3|4|5|
  # 0 to dmonths(5)
  collect() 

bind_rows(height_ever_history,
          height_ever_followup) %>% 
  group_by(PatientDurableKey) %>% 
  summarize(Height_ever = mean(Height_ever)) %>% 
  ungroup() %>% 
write_parquet(.,paste0(path_sga_bmi_folder,"/working/sba001/height ever for AAPMonthYearKey.parquet"))

rm(bmi_on,height_ever_history,height_ever_followup)


# PRESCRIPTIONS WLRX ------------

# Any weight loss medicines ever between -5y and +2y of AAPMonthYearKey

atcoh10b <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh10b_OBESITY"),partitioning = c("Year","PharmaceuticalClass")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         WLYearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(WLYearMonthKey = as.numeric(WLYearMonthKey)) %>% 
  mutate(type_wl = case_when(SimpleGenericName %in% c("metformin HCl","") ~ "metformin",
                             SimpleGenericName %in% c("semaglutide","tirzepatide") ~ "glp1ra",
                             SimpleGenericName %in% c("exenatide microspheres","liraglutide","dulaglutide",
                                                      "	exenatide","albiglutide") ~ "other glp1ra",
                             SimpleGenericName %in% c("phentermine HCl","	phentermine/topiramate",
                                                      "naltrexone HCl/bupropion HCl",
                                                      "lorcaserin HCl","phendimetrazine tartrate",
                                                      "diethylpropion HCl","orlistat",
                                                      "carboxymethylcellulose/citric") ~ "other wlrx",
                             TRUE ~ NA_character_
  )) %>% 
  mutate(Psychiatry = case_when(str_detect(PrimarySpecialty,"Psychiatry") ~ 1,
                                str_detect(PrimarySpecialty,"Psychiatry") ~ 1,
                                str_detect(PrimarySpecialty,"Behavior") ~ 1,
                                str_detect(SecondSpecialty,"Behavior") ~ 1,
                                str_detect(PrimarySpecialty,"Mental\\s") ~ 1,
                                str_detect(SecondSpecialty,"Mental\\s") ~ 1,
                                TRUE ~ 0),
         PrimaryCare = case_when(PrimarySpecialty %in% c("Family Medicine","Internal Medicine","Endocrinology",
                                                         "Nurse Practitioner","General Practice","Primary Care",
                                                         "Geriatric Medicine","Diabetes Services","Gerontology",
                                                         "Endocrinology, Diabetes & Metabolism","Preventative Medicine") ~ 1,
                                 SecondSpecialty %in% c("Family Medicine","Internal Medicine","Endocrinology",
                                                        "Nurse Practitioner","General Practice","Primary Care",
                                                        "Geriatric Medicine","Diabetes Services","Gerontology",
                                                        "Endocrinology, Diabetes & Metabolism","Preventative Medicine") ~ 1,
                                 TRUE ~ 0)) %>% 
  dplyr::filter(!is.na(type_wl))  %>% 
  collect()

atcoh10a <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh10a_DIABETES"),partitioning = c("Year","PharmaceuticalClass")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         WLYearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(WLYearMonthKey = as.numeric(WLYearMonthKey)) %>% 
  mutate(type_wl = case_when(SimpleGenericName %in% c("metformin HCl","") ~ "metformin",
                             SimpleGenericName %in% c("semaglutide","tirzepatide") ~ "glp1ra",
                             SimpleGenericName %in% c("exenatide microspheres","liraglutide","dulaglutide",
                                                      "	exenatide","albiglutide") ~ "other glp1ra",
                             SimpleGenericName %in% c("phentermine HCl","	phentermine/topiramate",
                                                      "naltrexone HCl/bupropion HCl",
                                                      "lorcaserin HCl","phendimetrazine tartrate",
                                                      "diethylpropion HCl","orlistat",
                                                      "carboxymethylcellulose/citric") ~ "other wlrx",
                             TRUE ~ NA_character_
  )) %>% 
  mutate(Psychiatry = case_when(str_detect(PrimarySpecialty,"Psychiatry") ~ 1,
                                str_detect(PrimarySpecialty,"Psychiatry") ~ 1,
                                str_detect(PrimarySpecialty,"Behavior") ~ 1,
                                str_detect(SecondSpecialty,"Behavior") ~ 1,
                                str_detect(PrimarySpecialty,"Mental\\s") ~ 1,
                                str_detect(SecondSpecialty,"Mental\\s") ~ 1,
                                TRUE ~ 0),
         PrimaryCare = case_when(PrimarySpecialty %in% c("Family Medicine","Internal Medicine","Endocrinology",
                                                         "Nurse Practitioner","General Practice","Primary Care",
                                                         "Geriatric Medicine","Diabetes Services","Gerontology",
                                                         "Endocrinology, Diabetes & Metabolism","Preventative Medicine") ~ 1,
                                 SecondSpecialty %in% c("Family Medicine","Internal Medicine","Endocrinology",
                                                        "Nurse Practitioner","General Practice","Primary Care",
                                                        "Geriatric Medicine","Diabetes Services","Gerontology",
                                                        "Endocrinology, Diabetes & Metabolism","Preventative Medicine") ~ 1,
                                 TRUE ~ 0)) %>% 
  dplyr::filter(!is.na(type_wl)) %>% 
  collect()


prescriptions_wlrx_after <- exposure_arrow %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey) %>% 
  inner_join(bind_rows(atcoh10a,
                       atcoh10b) ,
             by = c("PatientDurableKey"="PatientDurableKey")) %>%
  mutate(
    AAPMonthYearKey_ymd = ymd(AAPMonthYearKey,tz = NULL),
    # Add 15 days so that it crosses over to next month after subtraction
    # 20230501 - dmonths(3) is 20221230, we ideally want this to be after 20200101 since all our dates start on 01
    AAPMonthYearKey_ymd_minus4mo = ymd(AAPMonthYearKey,tz = NULL) - dmonths(4) + ddays(15),
    AAPMonthYearKey_ymd_plus2mo = ymd(AAPMonthYearKey,tz = NULL) + dmonths(2) + ddays(15),
  ) %>%  # There are some rows where the SimpleGenericName, PrimarySpecialty, SecondSpecialty and AAPMonthYearKey are same
  mutate(WLYearMonthKey_ymd = ymd(WLYearMonthKey),
         WLYearMonthKey_ymd_plusmonths = case_when(is.na(Months) ~ ymd(WLYearMonthKey,tz = NULL),
                                                   TRUE ~ ymd(WLYearMonthKey,tz = NULL) + dmonths(Months) + ddays(15))) %>%
  collect() %>% 
  mutate(across(contains("ymd"),~as_date(.))) %>% 
  mutate(include = case_when(
    (AAPMonthYearKey_ymd <= WLYearMonthKey_ymd &
       AAPMonthYearKey_ymd_plus2mo >= WLYearMonthKey_ymd &
       AAPMonthYearKey_ymd_plus2mo <= WLYearMonthKey_ymd_plusmonths) ~ 1,
    
    (AAPMonthYearKey_ymd <= WLYearMonthKey_ymd &
       AAPMonthYearKey_ymd_plus2mo >= WLYearMonthKey_ymd &
       AAPMonthYearKey_ymd_plus2mo >= WLYearMonthKey_ymd_plusmonths) ~ 2,
    TRUE ~ NA_real_
    
  )
  ) %>%   
  # Should meet either Case 1 or Case 2
  dplyr::filter(!is.na(include)) %>% 
  mutate(include = as.numeric(include)) %>% 
  mutate(diff_AAP_WL = abs(as.numeric(difftime(AAPMonthYearKey_ymd,WLYearMonthKey_ymd,units = "days"))),
         prescriber = case_when(Psychiatry >= 1 & PrimaryCare >= 1 ~ "PsychiatryPrimaryCare",
                                Psychiatry >= 1 ~ "Psychiatry",
                                PrimaryCare >= 1 ~ "PrimaryCare",
                                TRUE ~ "Other")) %>% 
  # Should be initiated within 2 months of AAPMonthYearKey
  # There might be medication initiated ever after 
  dplyr::filter(diff_AAP_WL <=62)

## Prescriptions before ------
# We need to exclude people who received something in the 3 months prior from those who received something in the month + 2 months after
prescriptions_wlrx_before <- exposure_arrow %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey) %>% 
  inner_join(bind_rows(atcoh10a,
                       atcoh10b) ,
             by = c("PatientDurableKey"="PatientDurableKey")) %>%
  mutate(
    AAPMonthYearKey_ymd = ymd(AAPMonthYearKey,tz = NULL),
    # Add 15 days so that it crosses over to next month after subtraction
    # 20230501 - dmonths(3) is 20221230, we ideally want this to be after 20200101 since all our dates start on 01
    AAPMonthYearKey_ymd_minus4mo = ymd(AAPMonthYearKey,tz = NULL) - dmonths(4) + ddays(15),
    AAPMonthYearKey_ymd_plus2mo = ymd(AAPMonthYearKey,tz = NULL) + dmonths(2) + ddays(15),
  ) %>%  # There are some rows where the SimpleGenericName, PrimarySpecialty, SecondSpecialty and AAPMonthYearKey are same
  mutate(WLYearMonthKey_ymd = ymd(WLYearMonthKey),
         WLYearMonthKey_ymd_plusmonths = case_when(is.na(Months) ~ ymd(WLYearMonthKey,tz = NULL),
                                                   TRUE ~ ymd(WLYearMonthKey,tz = NULL) + dmonths(Months) + ddays(15))) %>%
  collect() %>% 
  mutate(across(contains("ymd"),~as_date(.))) %>% 
  mutate(# The completition of the previous WLRX should be before 3 months of SGA
    wlrx_before = case_when(

      (AAPMonthYearKey_ymd_minus4mo >= WLYearMonthKey_ymd & # Previously >=
         AAPMonthYearKey_ymd > WLYearMonthKey_ymd & # Previously >=
         AAPMonthYearKey_ymd_minus4mo <= WLYearMonthKey_ymd_plusmonths &
         AAPMonthYearKey_ymd > WLYearMonthKey_ymd_plusmonths) ~ 1, # Previously >=

      (AAPMonthYearKey_ymd_minus4mo <= WLYearMonthKey_ymd &
         AAPMonthYearKey_ymd > WLYearMonthKey_ymd & # Previously >=
         AAPMonthYearKey_ymd > WLYearMonthKey_ymd_plusmonths) ~ 2, # Previously >=

      (AAPMonthYearKey_ymd_minus4mo <= WLYearMonthKey_ymd &
         AAPMonthYearKey_ymd > WLYearMonthKey_ymd & # Previously >=
         AAPMonthYearKey_ymd <= WLYearMonthKey_ymd_plusmonths) ~ 3, # Not changing this because it's a <= condition

      (AAPMonthYearKey_ymd_minus4mo >= WLYearMonthKey_ymd &
         AAPMonthYearKey_ymd > WLYearMonthKey_ymd & # Previously >=
         AAPMonthYearKey_ymd <= WLYearMonthKey_ymd_plusmonths) ~ 4,
      
      # Unmerged 
      # Anything older than the criteria here
      # Anything newer than the criteria here
      TRUE ~ NA_real_)
      
  ) %>%   
  dplyr::filter(!is.na(wlrx_before)) %>% 
  group_by(PatientDurableKey,AAPMonthYearKey) %>% 
  # Should meet either Case 1 or Case 2
  distinct(PatientDurableKey,AAPMonthYearKey)

## ANTI-JOIN ------
prescriptions_wlrx = prescriptions_wlrx_after %>% 
  anti_join(prescriptions_wlrx_before %>% 
               dplyr::select(PatientDurableKey,AAPMonthYearKey),
             by=c("PatientDurableKey","AAPMonthYearKey"))


## Closest prescriptions ------
closest_wlrx_by_sga <- prescriptions_wlrx  %>% 
  dplyr::select(-contains("ymd")) %>% 
  # distinct(PatientDurableKey,WLYearMonthKey,AAPMonthYearKey,TherapeuticClass,PharmaceuticalClass,SimpleGenericName,type_wl,.keep_all = TRUE) %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,type_wl) %>% 
  mutate(WLgenericnames = paste0("(",paste0(SimpleGenericName,collapse=";"),")"),
         WLprescribedmonths = paste0("(",paste0(WLYearMonthKey,collapse = ";"),")"),
         WLprescriptionduration = paste0("(",paste0(Months,collapse=";"),")"),
         maxWLprescriptionduration = max(Months),
         WLprescribers = paste0("(",paste0(prescriber,collapse = ";"),")"),
         diff_AAP_WL_concat = paste0("(",paste0(diff_AAP_WL,collapse=";"),")"),
         n_wlrx = n()) %>% 
  # Take the closest type_wl for each AAPMonthYearKey
  # This may have same WLYearMonthKey mapped to different AAPMonthYearKey
  dplyr::filter(diff_AAP_WL == min(diff_AAP_WL)) %>% 
  # If 2 WLYearMonthKey are equidistant from an AAPMonthYearKey, take any one of them
  slice(1) %>% 
  ungroup() %>% 
  arrange(PatientDurableKey,AAPMonthYearKey) %>% 
  group_by(PatientDurableKey,WLYearMonthKey,type_wl) %>% 
  mutate(outcome_duplicate = n(),
         index = 1:n()) %>% 
  ungroup()



unique_wlrx <- prescriptions_wlrx %>% 
  group_by(PatientDurableKey,WLYearMonthKey,type_wl) %>% 
  summarize(n = n(),
            include = paste0(include,collapse=";"),
            AAPprescribedmonths = paste0("(",paste0(AAPMonthYearKey,collapse=";"),")"),
            WLgenericnames = paste0(SimpleGenericName,collapse=";"),
            WLprescribers = paste0("(",paste0(prescriber,collapse = ";"),")")) %>% 
  ungroup() %>% 
  rename(n_aapevents = n)

str(prescriptions_wlrx)

write_parquet(closest_wlrx_by_sga,paste0(path_sga_bmi_folder,"/working/sba001/closest_prescriptions wlrx.parquet"))
write_parquet(unique_wlrx,paste0(path_sga_bmi_folder,"/working/sba001/unique_prescriptions wlrx.parquet"))
write_parquet(prescriptions_wlrx,paste0(path_sga_bmi_folder,"/working/sba001/prescriptions wlrx.parquet"))
write_parquet(prescriptions_wlrx_after,paste0(path_sga_bmi_folder,"/working/sba001/interim prescriptions wlrx after 2mo.parquet"))
write_parquet(prescriptions_wlrx_before,paste0(path_sga_bmi_folder,"/working/sba001/interim prescriptions wlrx before 3mo.parquet"))

rm(prescriptions_wlrx_after,
   prescriptions_wlrx,closest_wlrx_by_sga,unique_wlrx)

# UNEXPOSED WLRX ----------------

## Prescriptions after 5 months ------
# From the unexposed, we need to exclude people who received something within 5 months after
# This will include those exposed
prescriptions_wlrx_after_5mo <- exposure_arrow %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey) %>% 
  inner_join(bind_rows(atcoh10a,
                       atcoh10b),
             by = c("PatientDurableKey"="PatientDurableKey")) %>%
  mutate(
    AAPMonthYearKey_ymd = ymd(AAPMonthYearKey,tz = NULL),
    # Add 15 days so that it crosses over to next month after subtraction
    # 20230501 - dmonths(3) is 20221230, we ideally want this to be after 20200101 since all our dates start on 01
    AAPMonthYearKey_ymd_minus4mo = ymd(AAPMonthYearKey,tz = NULL) - dmonths(4) + ddays(15),
    AAPMonthYearKey_ymd_plus5mo = ymd(AAPMonthYearKey,tz = NULL) + dmonths(5) + ddays(15)
  ) %>%  # There are some rows where the SimpleGenericName, PrimarySpecialty, SecondSpecialty and AAPMonthYearKey are same
  mutate(WLYearMonthKey_ymd = ymd(WLYearMonthKey),
         WLYearMonthKey_ymd_plusmonths = case_when(is.na(Months) ~ ymd(WLYearMonthKey,tz = NULL),
                                                   TRUE ~ ymd(WLYearMonthKey,tz = NULL) + dmonths(Months) + ddays(15))) %>%
  collect() %>% 
  mutate(across(contains("ymd"),~as_date(.))) %>% 
  mutate(wlrx_after_5mo = case_when(
    (AAPMonthYearKey_ymd <= WLYearMonthKey_ymd &
       AAPMonthYearKey_ymd_plus5mo >= WLYearMonthKey_ymd &
       AAPMonthYearKey_ymd_plus5mo <= WLYearMonthKey_ymd_plusmonths) ~ 1,
    
    (AAPMonthYearKey_ymd <= WLYearMonthKey_ymd &
       AAPMonthYearKey_ymd_plus5mo >= WLYearMonthKey_ymd &
       AAPMonthYearKey_ymd_plus5mo >= WLYearMonthKey_ymd_plusmonths) ~ 2,
    TRUE ~ NA_real_
    
  )
  ) %>%   
  # Should meet either Case 1 or Case 2
  dplyr::filter(!is.na(wlrx_after_5mo)) %>% 
  mutate(wlrx_after_5mo = as.numeric(wlrx_after_5mo)) %>% 
  mutate(diff_AAP_WL = abs(as.numeric(difftime(AAPMonthYearKey_ymd,WLYearMonthKey_ymd,units = "days"))),
         prescriber = case_when(Psychiatry >= 1 & PrimaryCare >= 1 ~ "PsychiatryPrimaryCare",
                                Psychiatry >= 1 ~ "Psychiatry",
                                PrimaryCare >= 1 ~ "PrimaryCare",
                                TRUE ~ "Other")) %>% 
  distinct(PatientDurableKey,AAPMonthYearKey)


## ANTI-JOIN ------
prescriptions_wlrx_before <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/interim prescriptions wlrx before 3mo.parquet"))

unexposed_wlrx <- exposure_arrow %>% 
  anti_join(
    bind_rows(prescriptions_wlrx_after_5mo,
              prescriptions_wlrx_before),
    by=c("PatientDurableKey","AAPMonthYearKey")
  ) %>% 
  collect()
  

write_parquet(unexposed_wlrx,paste0(path_sga_bmi_folder,"/working/sba001/unexposed to prescriptions wlrx.parquet"))
write_parquet(prescriptions_wlrx_after_5mo,paste0(path_sga_bmi_folder,"/working/sba001/interim prescriptions wlrx after 5mo.parquet"))

unexposed_wlrx <- read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/unexposed to prescriptions wlrx.parquet"))


# AFTER EXPOSURE --------------
closest_wlrx_by_sga_arrow <- open_dataset(paste0(path_sga_bmi_folder,"/working/sba001/closest_prescriptions wlrx.parquet")) %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey,type_wl) %>% 
  mutate(AAPMonthYearKey_ymd_plus5mo = ymd(AAPMonthYearKey,tz = NULL) + dmonths(5) + ddays(15),
         AAPMonthYearKey_ymd = ymd(AAPMonthYearKey,tz = NULL))

## atcoh01b. BMI -----------
bmi_after = open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
            by = c("PatientDurableKey")) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >60 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  mutate(Weight_kg = case_when(is.na(BodyMassIndex) ~ NA_real_,
                               TRUE ~ Weight*0.453592),
         Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  # 0|1|2|3|4|5|
  # 0 to dmonths(5)
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(BodyMassIndex))   %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(Bmi_after = mean(BodyMassIndex),
            Weight_after = mean(Weight_kg),
            Height_after = mean(Height_cm),
            n_Bmi_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_Bmi_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_Bmi = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                            ymd(YearMonthKey_Bmi_after,tz=NULL),units = "days"))))

write_parquet(bmi_after,paste0(path_sga_bmi_folder,"/working/sba001/all bmi 6months after exposure date.parquet"))

## atcoh02ab. HbA1c -----------

hba1c_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02ab"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hba1c = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(hba1c)) %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(Hba1c_after = mean(hba1c),
            n_Hba1c_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_Hba1c_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_Hba1c = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                            ymd(YearMonthKey_Hba1c_after,tz=NULL),units = "days"))))

write_parquet(hba1c_after,paste0(path_sga_bmi_folder,"/working/sba001/all hba1c 6months after exposure date.parquet"))

## atcoh02fb. Tgl -----------

tgl_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02fb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(tgl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(tgl))   %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(tgl_after = mean(tgl),
            n_tgl_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_tgl_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_tgl = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                  ymd(YearMonthKey_tgl_after,tz=NULL),units = "days"))))

write_parquet(tgl_after,paste0(path_sga_bmi_folder,"/working/sba001/all tgl 6months after exposure date.parquet"))

## atcoh02fb. Ldlc -----------

ldl_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02db"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(ldl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(ldl))   %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(ldl_after = mean(ldl),
            n_ldl_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_ldl_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_ldl = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                ymd(YearMonthKey_ldl_after,tz=NULL),units = "days"))))

write_parquet(ldl_after,paste0(path_sga_bmi_folder,"/working/sba001/all ldl 6months after exposure date.parquet"))

## atcoh02fb. Hdlc -----------

hdl_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02eb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hdl = case_when(NumericValue >= 5 & NumericValue <= 300 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(hdl))   %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(hdl_after = mean(hdl),
            n_hdl_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_hdl_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_hdl = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                ymd(YearMonthKey_hdl_after,tz=NULL),units = "days"))))

write_parquet(hdl_after,paste0(path_sga_bmi_folder,"/working/sba001/all hdl 6months after exposure date.parquet"))


## atcoh01b. Blood Pressure ------------
blood_pressure_after = open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(Sbp_after = mean(SystolicBloodPressure),
            Dbp_after = mean(DiastolicBloodPressure),
            n_Sbp_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_Sbp_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_Sbp = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                ymd(YearMonthKey_Sbp_after,tz=NULL),units = "days"))))

write_parquet(blood_pressure_after,paste0(path_sga_bmi_folder,"/working/sba001/all Sbp 6months after exposure date.parquet"))

## atcoh02kb. Neutrophils -----------

neutro_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02kb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(neutro = case_when(NumericValue >= 50 ~ NumericValue/1000,
                            NumericValue > 0 & NumericValue < 50 ~ NumericValue,
                            TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(neutro))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(neutro_after = mean(neutro),
            n_neutro_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_neutro_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_neutro = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                ymd(YearMonthKey_neutro_after,tz=NULL),units = "days"))))

write_parquet(neutro_after,paste0(path_sga_bmi_folder,"/working/sba001/all neutro 6months after exposure date.parquet"))

## atcoh02lb. Lymphocytes -----------

lympho_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02lb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(lympho = case_when(NumericValue >= 50 ~ NumericValue/1000,
                            NumericValue > 0 & NumericValue < 50 ~ NumericValue,
                            TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(lympho))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(lympho_after = mean(lympho),
            n_lympho_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_lympho_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_lympho = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                   ymd(YearMonthKey_lympho_after,tz=NULL),units = "days"))))

write_parquet(lympho_after,paste0(path_sga_bmi_folder,"/working/sba001/all lympho 6months after exposure date.parquet"))


## atcoh02mb. Neutrophils:Leukocytes -----------

neutleuk_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02mb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(neutleuk = case_when(
    NumericValue >= 0 & NumericValue <= 100 ~ NumericValue,
    TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(neutleuk))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(neutleuk_after = mean(neutleuk),
            n_neutleuk_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_neutleuk_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_neutleuk = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                   ymd(YearMonthKey_neutleuk_after,tz=NULL),units = "days"))))

write_parquet(neutleuk_after,paste0(path_sga_bmi_folder,"/working/sba001/all neutrophil leukocyte 6months after exposure date.parquet"))

## atcoh02nb. Lymphocytes:Leukocytes -----------

lymphleuk_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02nb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(closest_wlrx_by_sga_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(lymphleuk = case_when(
    NumericValue >= 0 & NumericValue <= 100 ~ NumericValue,
    TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(lymphleuk))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(lymphleuk_after = mean(lymphleuk),
            n_lymphleuk_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_lymphleuk_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_lymphleuk = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                     ymd(YearMonthKey_lymphleuk_after,tz=NULL),units = "days"))))

write_parquet(lymphleuk_after,paste0(path_sga_bmi_folder,"/working/sba001/all lymphocyte leukocyte 6months after exposure date.parquet"))


after <- closest_wlrx_by_sga_arrow  %>% 
  dplyr::select(-type_wl) %>% 
  left_join(hba1c_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_Hba1c == min(diff_AAP_Hba1c))  %>% 
              ungroup() %>% 
              dplyr::select(PatientDurableKey,AAPMonthYearKey,Hba1c_after,diff_AAP_Hba1c),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(tgl_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_tgl == min(diff_AAP_tgl))  %>% 
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,tgl_after,diff_AAP_tgl),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(ldl_after%>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_ldl == min(diff_AAP_ldl))  %>% 
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,ldl_after,diff_AAP_ldl),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(hdl_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_hdl == min(diff_AAP_hdl)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,hdl_after,diff_AAP_hdl),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(blood_pressure_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_Sbp == min(diff_AAP_Sbp)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,Sbp_after,Dbp_after,diff_AAP_Sbp),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(bmi_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey)%>% 
              dplyr::filter(diff_AAP_Bmi == min(diff_AAP_Bmi)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,Bmi_after,Height_after,Weight_after,diff_AAP_Bmi),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(neutro_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey)%>% 
              dplyr::filter(diff_AAP_neutro == min(diff_AAP_neutro)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,neutro_after,diff_AAP_neutro),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(lympho_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey)%>% 
              dplyr::filter(diff_AAP_lympho == min(diff_AAP_lympho)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,lympho_after,diff_AAP_lympho),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(neutleuk_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_neutleuk == min(diff_AAP_neutleuk)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,neutleuk_after,diff_AAP_neutleuk),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(lymphleuk_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_lymphleuk == min(diff_AAP_lymphleuk)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,lymphleuk_after,diff_AAP_lymphleuk),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  collect() %>% 
  distinct(PatientDurableKey,AAPMonthYearKey,.keep_all=TRUE) 

write_parquet(after,paste0(path_sga_bmi_folder,"/working/sba001/earliest predictors 6months after exposure date.parquet"))
rm(hba1c_after,tgl_after,ldl_after,hdl_after,blood_pressure_after,bmi_after,
   neutro_after,lympho_after,neutleuk_after,lymphleuk_after);gc()



# AFTER UNEXPOSED --------------


unexposed_wlrx_arrow <- open_dataset(paste0(path_sga_bmi_folder,"/working/sba001/unexposed to prescriptions wlrx.parquet")) %>% 
  dplyr::select(PatientDurableKey,AAPMonthYearKey) %>% 
  mutate(AAPMonthYearKey_ymd_plus5mo = ymd(AAPMonthYearKey,tz = NULL) + dmonths(5) + ddays(15),
         AAPMonthYearKey_ymd = ymd(AAPMonthYearKey,tz = NULL))

## atcoh01b. BMI -----------
bmi_unexposed_after = open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(
    BodyMassIndex = case_when(BodyMassIndex >60 | BodyMassIndex <12 ~ NA_real_,
                              TRUE ~ BodyMassIndex)) %>% 
  mutate(Weight_kg = case_when(is.na(BodyMassIndex) ~ NA_real_,
                               TRUE ~ Weight*0.453592),
         Height_cm = Height*2.54) %>% 
  to_duckdb()  %>%
  # 0|1|2|3|4|5|
  # 0 to dmonths(5)
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(BodyMassIndex))   %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(Bmi_after = mean(BodyMassIndex),
            Weight_after = mean(Weight_kg),
            Height_after = mean(Height_cm),
            n_Bmi_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_Bmi_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_Bmi = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                ymd(YearMonthKey_Bmi_after,tz=NULL),units = "days"))))

write_parquet(bmi_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all bmi 6months after unexposed date.parquet"))

## atcoh02ab. HbA1c -----------

hba1c_unexposed_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02ab"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hba1c = case_when(NumericValue >= 3 & NumericValue <= 20 ~ NumericValue,
                           TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(hba1c)) %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(Hba1c_after = mean(hba1c),
            n_Hba1c_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_Hba1c_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_Hba1c = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                  ymd(YearMonthKey_Hba1c_after,tz=NULL),units = "days"))))

write_parquet(hba1c_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all hba1c 6months after unexposed date.parquet"))

## atcoh02fb. Tgl -----------

tgl_unexposed_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02fb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(tgl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(tgl))   %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(tgl_after = mean(tgl),
            n_tgl_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_tgl_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_tgl = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                ymd(YearMonthKey_tgl_after,tz=NULL),units = "days"))))

write_parquet(tgl_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all tgl 6months after unexposed date.parquet"))

## atcoh02fb. Ldlc -----------

ldl_unexposed_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02db"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(ldl = case_when(NumericValue >= 10 & NumericValue <= 600 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(ldl))   %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(ldl_after = mean(ldl),
            n_ldl_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_ldl_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_ldl = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                ymd(YearMonthKey_ldl_after,tz=NULL),units = "days"))))

write_parquet(ldl_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all ldl 6months after unexposed date.parquet"))

## atcoh02fb. Hdlc -----------

hdl_unexposed_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02eb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(hdl = case_when(NumericValue >= 5 & NumericValue <= 300 ~ NumericValue,
                         TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(hdl))   %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(hdl_after = mean(hdl),
            n_hdl_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_hdl_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_hdl = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                ymd(YearMonthKey_hdl_after,tz=NULL),units = "days"))))

write_parquet(hdl_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all hdl 6months after unexposed date.parquet"))


## atcoh01b. Blood Pressure ------------
blood_pressure_unexposed_after = open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh01b"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(
    SystolicBloodPressure = case_when(SystolicBloodPressure > 300 | SystolicBloodPressure < 50 ~ NA_real_,
                                      TRUE ~ SystolicBloodPressure),
    DiastolicBloodPressure = case_when(DiastolicBloodPressure > 300 | DiastolicBloodPressure < 30 ~ NA_real_,
                                       TRUE ~ DiastolicBloodPressure)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(SystolicBloodPressure),!is.na(DiastolicBloodPressure))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(Sbp_after = mean(SystolicBloodPressure),
            Dbp_after = mean(DiastolicBloodPressure),
            n_Sbp_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_Sbp_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_Sbp = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                ymd(YearMonthKey_Sbp_after,tz=NULL),units = "days"))))

write_parquet(blood_pressure_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all Sbp 6months after unexposed date.parquet"))

## atcoh02kb. Neutrophils -----------

neutro_unexposed_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02kb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(neutro = case_when(NumericValue >= 50 ~ NumericValue/1000,
                            NumericValue > 0 & NumericValue < 50 ~ NumericValue,
                            TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(neutro))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(neutro_after = mean(neutro),
            n_neutro_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_neutro_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_neutro = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                   ymd(YearMonthKey_neutro_after,tz=NULL),units = "days"))))

write_parquet(neutro_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all neutro 6months after unexposed date.parquet"))

## atcoh02lb. Lymphocytes -----------

lympho_unexposed_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02lb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(lympho = case_when(NumericValue >= 50 ~ NumericValue/1000,
                            NumericValue > 0 & NumericValue < 50 ~ NumericValue,
                            TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(lympho))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(lympho_after = mean(lympho),
            n_lympho_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_lympho_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_lympho = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                   ymd(YearMonthKey_lympho_after,tz=NULL),units = "days"))))

write_parquet(lympho_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all lympho 6months after unexposed date.parquet"))


## atcoh02mb. Neutrophils:Leukocytes -----------

neutleuk_unexposed_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02mb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(neutleuk = case_when(
    NumericValue >= 0 & NumericValue <= 100 ~ NumericValue,
    TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(neutleuk))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(neutleuk_after = mean(neutleuk),
            n_neutleuk_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_neutleuk_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_neutleuk = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                     ymd(YearMonthKey_neutleuk_after,tz=NULL),units = "days"))))

write_parquet(neutleuk_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all neutrophil leukocyte 6months after unexposed date.parquet"))

## atcoh02nb. Lymphocytes:Leukocytes -----------

lymphleuk_unexposed_after <- open_dataset(paste0(path_aap_t2dm_folder,"/working/atcoh02nb"),partitioning = c("Year")) %>% 
  mutate(PatientDurableKey = as.numeric(PatientDurableKey),
         YearMonthKey = paste0(YearMonth,"01")) %>% 
  mutate(YearMonthKey = as.numeric(YearMonthKey),
         YearMonthKey_ymd = ymd(YearMonthKey,tz=NULL)) %>% 
  inner_join(unexposed_wlrx_arrow,
             by = c("PatientDurableKey")) %>% 
  mutate(NumericValue = as.numeric(NumericValue)) %>% 
  mutate(lymphleuk = case_when(
    NumericValue >= 0 & NumericValue <= 100 ~ NumericValue,
    TRUE ~ NA_real_)) %>% 
  to_duckdb()  %>%
  mutate(period = case_when(YearMonthKey_ymd >= AAPMonthYearKey_ymd_plus5mo ~ "M6 or later",
                            TRUE ~ "M5 or earlier")) %>% 
  dplyr::filter(period %in% c("M6 or later"),!is.na(lymphleuk))  %>% 
  group_by(PatientDurableKey,AAPMonthYearKey,YearMonthKey) %>% 
  summarize(lymphleuk_after = mean(lymphleuk),
            n_lymphleuk_YearMonthKey = n()) %>%   
  ungroup()  %>% 
  rename(YearMonthKey_lymphleuk_after = YearMonthKey) %>% 
  collect() %>% 
  mutate(diff_AAP_lymphleuk = abs(as.numeric(difftime(ymd(AAPMonthYearKey,tz=NULL),
                                                      ymd(YearMonthKey_lymphleuk_after,tz=NULL),units = "days"))))

write_parquet(lymphleuk_unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/all lymphocyte leukocyte 6months after unexposed date.parquet"))


unexposed_after <- unexposed_wlrx_arrow  %>% 
  left_join(hba1c_unexposed_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_Hba1c == min(diff_AAP_Hba1c))  %>% 
              ungroup() %>% 
              dplyr::select(PatientDurableKey,AAPMonthYearKey,Hba1c_after,diff_AAP_Hba1c),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(tgl_unexposed_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_tgl == min(diff_AAP_tgl))  %>% 
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,tgl_after,diff_AAP_tgl),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(ldl_unexposed_after%>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_ldl == min(diff_AAP_ldl))  %>% 
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,ldl_after,diff_AAP_ldl),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(hdl_unexposed_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_hdl == min(diff_AAP_hdl)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,hdl_after,diff_AAP_hdl),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(blood_pressure_unexposed_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_Sbp == min(diff_AAP_Sbp)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,Sbp_after,Dbp_after,diff_AAP_Sbp),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(bmi_unexposed_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey)%>% 
              dplyr::filter(diff_AAP_Bmi == min(diff_AAP_Bmi)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,Bmi_after,Height_after,Weight_after,diff_AAP_Bmi),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(neutro_unexposed_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey)%>% 
              dplyr::filter(diff_AAP_neutro == min(diff_AAP_neutro)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,neutro_after,diff_AAP_neutro),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(lympho_unexposed_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey)%>% 
              dplyr::filter(diff_AAP_lympho == min(diff_AAP_lympho)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,lympho_after,diff_AAP_lympho),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(neutleuk_unexposed_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_neutleuk == min(diff_AAP_neutleuk)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,neutleuk_after,diff_AAP_neutleuk),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  left_join(lymphleuk_unexposed_after %>% 
              group_by(PatientDurableKey,AAPMonthYearKey) %>% 
              dplyr::filter(diff_AAP_lymphleuk == min(diff_AAP_lymphleuk)) %>%   
              ungroup() %>%   
              dplyr::select(PatientDurableKey,AAPMonthYearKey,lymphleuk_after,diff_AAP_lymphleuk),
            by=c("PatientDurableKey","AAPMonthYearKey")) %>% 
  collect() %>% 
  distinct(PatientDurableKey,AAPMonthYearKey,.keep_all=TRUE)

write_parquet(unexposed_after,paste0(path_sga_bmi_folder,"/working/sba001/earliest predictors 6months after unexposed date.parquet"))
rm(hba1c_unexposed_after,tgl_unexposed_after,ldl_unexposed_after,hdl_unexposed_after,blood_pressure_unexposed_after,bmi_unexposed_after,
   neutro_unexposed_after,lympho_unexposed_after,neutleuk_unexposed_after,lymphleuk_unexposed_after);gc()

