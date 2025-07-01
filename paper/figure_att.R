rm(list=ls());gc();source(".Rprofile")


fig_df <- read_csv("analysis/sba008_average treatment effect among treated.csv") %>% 
  dplyr::select(level,outcome,baseline,weighted_ptt,weighted_ptu) %>% 
  pivot_longer(cols=c(baseline,weighted_ptt,weighted_ptu),names_to="variable",values_to="value") %>% 
  mutate(level = factor(level,levels=c("total","low","intermediate","high"),
                        labels=c("Total","Low","Intermediate","High")),
         variable = factor(variable,levels=c("baseline","weighted_ptu","weighted_ptt"),
                           labels=c("Baseline","Counterfactual Untreated","Observed Treated")))

fig_A = fig_df %>% 
  dplyr::filter(outcome =="Bmi_after") %>% 
  ggplot(data=.,aes(x=level,y=value,fill=variable)) +
  geom_col(position=position_dodge(width=0.9)) +
  geom_text(aes(x=level,y=value+2,label=round(value,1)),position = position_dodge(width=0.9))+
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("Body Masss Index (kg/m2)") +
  scale_fill_manual(name="",values=c(rgb(242/255,173/255,0/255),rgb(90/255,188/255,214/255),rgb(1/255,160/255,138/255),rgb(114/255,148/255,212/255)))

fig_B = fig_df %>% 
  dplyr::filter(outcome =="Weight_after") %>% 
  ggplot(data=.,aes(x=level,y=value,fill=variable)) +
  geom_col(position=position_dodge(width=0.9)) +
  geom_text(aes(x=level,y=value+2,label=round(value,1)),position = position_dodge(width=0.9))+
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("Weight (kg)") +
  scale_fill_manual(name="",values=c(rgb(242/255,173/255,0/255),rgb(90/255,188/255,214/255),rgb(1/255,160/255,138/255),rgb(114/255,148/255,212/255)))

library(ggpubr)

ggarrange(fig_A,
          fig_B,
          nrow = 1,ncol=2,
          labels=LETTERS[1:2],
          legend="bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_sga_bmi_folder,"/figures/att.jpg"),width=12,height = 6)

