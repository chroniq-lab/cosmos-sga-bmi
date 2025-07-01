# Borrowed from ~aap_t2dm/functions/run_geeglm.R

source("H:/code/functions/imputation/save_mi_geeglm.R")

run_geeglm_bmi <- function(f,df,type="ipw"){
  
  if(type == "matchit"){
    df$finalweights = df$ipcw_Bmi
    print("ipcw weights")
  }else if(str_detect(f,"\\*type_sga")){
    
    df$finalweights = df$stabilizedweights_Bmi_sga
    print("sga weights")
  }else{
    
    df$finalweights = df$stabilizedweights_Bmi
  }
  
  
  if(str_detect(f,"Bmi_after")|str_detect(f,"pctchange_Weight_after\\s")){
    print("Bmi")
    model_fit <- geeglm(as.formula(f),family = gaussian(),
                        weights = finalweights,
                        data=df,id=PatientDurableKey,corstr="exchangeable")
  } else if(str_detect(f,"pctchange_Weight_after_ge_minus5")) {
    print("pctchange")
    
    model_fit <- geeglm(as.formula(f),family = poisson(),
                        weights = finalweights,
                        data=df,id=PatientDurableKey,corstr="exchangeable")
  } else if(str_detect(f,"Weight_after ")){
    print("Weight_after")
    model_fit <- geeglm(as.formula(f),family = gaussian(),
                        weights = finalweights,
                        data=df,id=PatientDurableKey,corstr="exchangeable")
    
  }
  
  geeglm_output = save_geeglm(model_fit)
  # https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html
  # emmip(model_fit,exposure_binary ~ months_diff_AAP_Bmi,at=list(months_diff_AAP_Bmi=c(0,1,2,3,4,5,6)))

  rm(model_fit);gc()
  return(geeglm_output)
}
