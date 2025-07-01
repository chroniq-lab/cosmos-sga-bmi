rm(list=ls());gc();source(".Rprofile")


Bmi_after_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS")) %>% 
  dplyr::filter(!is.na(Bmi_after)) 

Bmi_after_df %>% 
  bind_rows(.,
            {.} %>% mutate(exposure_binary = 10)) %>% 
  group_by(exposure_binary) %>% 
  summarize(n = n(),
            mean_bmi = paste0(round(mean(Bmi_after),1)," (",round(sd(Bmi_after),1)),
            mean_weight = paste0(round(mean(Weight_after),1)," (",round(sd(Bmi_after),1)),
            achieved_ge5 = mean(pctchange_Weight_after_ge_minus5),
            median_pctchange = paste0(round(median(pctchange_Weight_after)*100,1)," (",
                                      round(quantile(pctchange_Weight_after,0.25)*100,1),", ",
                                      round(quantile(pctchange_Weight_after,0.75)*100,1),")")
            ) %>% 
  write_csv(.,"paper/text_follow up information.csv")


# From rfa_analytic sample preprocessing.R
analytic <- readRDS(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS"))   %>% 
  mutate(
    type_sga = factor(type_sga,levels=c("low","intermediate","high"),labels=c("Low","Intermediate","High")))  %>% 
  mutate(year = as.character(AAPMonthYearKey %/% 10000))


summary(analytic[analytic$exposure_binary==1,]$months_diff_WL_Bmi)


Bmi_after_df %>% 
  dplyr::filter(exposure_binary == 1) %>% 
  mutate(time_from_sga_WL = (months_diff_AAP_Bmi-months_diff_WL_Bmi)) %>% 
  summarize(m = median(time_from_sga_WL),
            min = min(time_from_sga_WL),
            q25 = quantile(time_from_sga_WL,0.25),
            q75 = quantile(time_from_sga_WL,0.75),
            max = max(time_from_sga_WL))

