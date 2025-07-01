

predictors_after <- bind_rows(
  read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/earliest predictors 6months after exposure date.parquet")),
  read_parquet(paste0(path_sga_bmi_folder,"/working/sba001/earliest predictors 6months after unexposed date.parquet")))


analytic_sample <- read_parquet(paste0(path_sga_bmi_folder,"/working/analytic sample.parquet")) 


