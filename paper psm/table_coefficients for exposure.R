rm(list=ls());gc();source(".Rprofile")


weight <- read_csv("psm/sbp006_coefficients of linear regression for weight.csv")
pctchange <- read_csv("psm/sbp003_coefficients of linear regression for pct weight change.csv") 
pct5change <- read_csv("psm/sbp004_coefficients of binomial regression.csv")



table_coefs <- bind_rows(
                         weight %>% mutate(coef = estimate,outcome = "Weight_after"),
                         pctchange %>% mutate(coef=estimate,outcome = "pctchange_Weight_after") %>% 
                           mutate(coef = estimate*100,
                                  lci = lci*100,
                                  uci = uci*100),
                         pct5change ) %>% 
  dplyr::filter(str_detect(model,"(0|3)"),str_detect(term,"exposure")) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")"),
         model = case_when(str_detect(model,"0") ~ "Imbalanced",
                           str_detect(model,"3") ~ "Imbalanced + Covariates",
                           TRUE ~ NA_character_)) %>% 
  dplyr::select(model,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from=coef_ci)

write_csv(table_coefs,"paper psm/table_coefficients for exposure_binary.csv")

