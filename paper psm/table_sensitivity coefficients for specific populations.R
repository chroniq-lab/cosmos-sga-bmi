rm(list=ls());gc();source(".Rprofile")

df = bind_rows(    read_csv("sensitivity/sbsens001_regression coefficients of linear and binomial regression by disorder.csv"),
                   read_csv("sensitivity/sbsens002_regression coefficients of linear and binomial regression by sex.csv"),
                   read_csv("sensitivity/sbsens003_regression coefficients of linear and binomial regression by exposure category.csv"),
                   read_csv("sensitivity/sbsens004_regression coefficients of linear and binomial regression among younger.csv"))


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
                           str_detect(model,"5") ~ "Imbalanced by SGA Category + Covariates",
                           TRUE ~ "Imbalanced + Covariates")) %>% 
  dplyr::select(population,model,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from=coef_ci)

write_csv(table_coefs,"paper psm/table_sensitivity coefficients.csv")

