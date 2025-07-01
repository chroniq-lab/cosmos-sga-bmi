# library(boot)
# set.seed(1234)
# # model_fit and e_var are global variables that are required for att_standardization()
# model_fit = model_m0
# e_var = "exposure_binary"
# 
# att_standardization_boot <- function(df,i,model_fit,e_var){
#   d = df[i,]
#   att_estimation(df_treated = d,model_obj = model_fit,exposure_var = e_var) %>% 
#     return(.)
# }
# 
# results_m0 <- boot(data = treated_df,stype="i", statistic = att_standardization_boot, R = 100,model_fit = model_m0,e_var =  "exposure_binary")
# results_m0

