rm(list=ls());gc();source(".Rprofile")

main_coefs <- bind_rows(
  read_csv("psm/sbp003_coefficients of linear regression for pct weight change.csv") %>% 
    mutate(coef = estimate*100,
           lci = lci*100,
           uci = uci*100),
  read_csv("psm/sbp006_coefficients of linear regression for weight.csv") %>% mutate(coef=estimate,outcome = "Weight_after"),
  read_csv("psm/sbp004_coefficients of binomial regression.csv") ) %>% 
  dplyr::filter(str_detect(model,"(0)"),str_detect(term,"exposure")) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")"))  %>% 
  dplyr::select(outcome,coef,lci,uci) %>% 
  mutate(modifier = "Total")




contrasts <- read_csv("psm/sbp007_contrasts for sga.csv") %>% 
  dplyr::select(modifier,outcome,coef,lci,uci) %>% 
  mutate(coef = case_when(outcome == "pctchange_Weight_after" ~ coef*100,
                          TRUE ~ coef),
         lci = case_when(outcome == "pctchange_Weight_after" ~ lci*100,
                          TRUE ~ lci),
         uci = case_when(outcome == "pctchange_Weight_after" ~ uci*100,
                          TRUE ~ uci)
         )

fig_df = bind_rows(main_coefs,
                   contrasts) %>% 
  mutate(modifier = factor(modifier,levels=c("Total","type_sgalow","type_sgaintermediate","type_sgahigh"),
                           labels=c("Total","Low Risk","Intermediate Risk","High Risk")))

fig_df%>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  dplyr::select(modifier,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from=coef_ci) %>% 
  dplyr::select(modifier,Weight_after,pctchange_Weight_after,pctchange_Weight_after_ge_minus5) %>% 

write_csv(.,"paper psm/table_coefficients for exposure by sga categories.csv")

fig_A = fig_df %>% 
  dplyr::filter(outcome == "Weight_after") %>% 
  ggplot(data=.,aes(x=coef,xmin=lci,xmax=uci,col=modifier,y=modifier)) +
  geom_point() +
  geom_errorbarh(height=0.1) +
  theme_bw() +
  scale_color_manual(name="",values=c("black",rgb(90/255,188/255,214/255),rgb(1/255,160/255,138/255),rgb(114/255,148/255,212/255))) +
  scale_y_discrete(limits=rev) +
  xlab("Change in Weight (kg)") +
  ylab("") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  geom_vline(xintercept=0,col="red",linetype = 2,linewidth = 1.2) +
  scale_x_continuous(limits=c(-3.5,1.5),breaks=seq(-3,1,by=1))
  


fig_B = fig_df %>% 
  dplyr::filter(outcome == "pctchange_Weight_after") %>% 
  ggplot(data=.,aes(x=coef,xmin=lci,xmax=uci,col=modifier,y=modifier)) +
  geom_point() +
  geom_errorbarh(height=0.1) +
  theme_bw() +
  scale_color_manual(name="",values=c("black",rgb(90/255,188/255,214/255),rgb(1/255,160/255,138/255),rgb(114/255,148/255,212/255))) +
  scale_y_discrete(limits=rev) +
  xlab("Change in Weight \n(percentage points)") +
  ylab("") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  geom_vline(xintercept=0,col="red",linetype = 2,linewidth = 1.2) +
  scale_x_continuous(limits=c(-3.5,1.5),breaks=seq(-3,1,by=1))

fig_C = fig_df %>% 
  dplyr::filter(outcome == "pctchange_Weight_after_ge_minus5") %>% 
  ggplot(data=.,aes(x=coef,xmin=lci,xmax=uci,col=modifier,y=modifier)) +
  geom_point() +
  geom_errorbarh(height=0.1) +
  theme_bw() +
  scale_color_manual(name="",values=c("black",rgb(90/255,188/255,214/255),rgb(1/255,160/255,138/255),rgb(114/255,148/255,212/255))) +
  scale_y_discrete(limits=rev) +
  xlab("Risk Ratio for \nAchieving 5% Weight Loss") +
  ylab("") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  geom_vline(xintercept=1,col="red",linetype = 2,linewidth = 1.2) +
  scale_x_continuous(limits=c(0.75,2.25),breaks=seq(0.5,2.0,by=0.5))


library(ggpubr)

ggarrange(fig_A,
          fig_B,
          fig_C,
          nrow = 1,ncol=3,
          labels=LETTERS[1:3],
          legend="bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_sga_bmi_folder,"/figures/psm_coefficients for exposure by sga categories.jpg"),width=12,height = 5)

