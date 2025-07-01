rm(list=ls());gc();source(".Rprofile")

df = read_csv("psm/sbp009_coefficients without people older than 75 years.csv")


table_coefs <- df %>% 
  mutate(coef = case_when(is.na(coef) ~ estimate,
                          TRUE ~ coef)) %>% 
  mutate(coef = case_when(outcome == "pctchange_Weight_after" ~ coef*100,
                          TRUE ~ coef),
         lci = case_when(outcome == "pctchange_Weight_after" ~ lci*100,
                         TRUE ~ lci),
         uci = case_when(outcome == "pctchange_Weight_after" ~ uci*100,
                         TRUE ~ uci)
         ) %>% 
  dplyr::filter(str_detect(term,"exposure")) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")"),
         model = case_when(str_detect(model,"0") ~ "Imbalanced",
                           TRUE ~ "Imbalanced + Covariates")) %>% 
  dplyr::select(model,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from=coef_ci)

write_csv(table_coefs,"paper psm/table_coefficients excluding people older than 75 years.csv")
