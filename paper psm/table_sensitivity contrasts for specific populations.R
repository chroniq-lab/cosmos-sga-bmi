rm(list=ls());gc();source(".Rprofile")

df = bind_rows(    read_csv("sensitivity/sbsens001_contrasts for sga by disorder.csv"),
                   read_csv("sensitivity/sbsens002_contrasts for sga by sex.csv"),
                   read_csv("sensitivity/sbsens003_contrasts for sga by exposure category.csv"),
                   read_csv("sensitivity/sbsens004_contrasts for sga among younger.csv"))


table_coefs <- df %>% 

  mutate(coef = case_when(outcome == "pctchange_Weight_after" ~ coef*100,
                          TRUE ~ coef),
         lci = case_when(outcome == "pctchange_Weight_after" ~ lci*100,
                         TRUE ~ lci),
         uci = case_when(outcome == "pctchange_Weight_after" ~ uci*100,
                         TRUE ~ uci)
  ) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  dplyr::select(modifier,population,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from=coef_ci) %>% 
  arrange(population)

write_csv(table_coefs,"paper psm/table_sensitivity coefficients.csv")
