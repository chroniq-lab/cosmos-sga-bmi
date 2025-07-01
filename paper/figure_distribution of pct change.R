rm(list=ls());gc();source(".Rprofile")

analytic_df <- readRDS(paste0(path_sga_bmi_folder,"/working/sba002/analytic sample with ipw.RDS")) 

fig_A <- analytic_df %>% 
  mutate(exposure_binary = factor(exposure_binary,levels=c(0,1),labels=c("No Prescription","Semaglutide or Tirzepatide"))) %>% 
  bind_rows(
    .,
    {.} %>% mutate(type_sga = "Total")
  ) %>% 
  mutate(type_sga = factor(type_sga,
                           levels=c("Total","low","intermediate","high"),
                           labels=c("Total","Low Risk","Intermediate Risk","High Risk")),
         pctchange_Weight_after = pctchange_Weight_after*100) %>% 
  ggplot(data=.,aes(x=type_sga,y=pctchange_Weight_after,fill=exposure_binary)) +
    geom_boxplot(outlier.colour = "grey95") +
    theme_bw() 



fig_A_update <- fig_A +
  theme(legend.position = "bottom") +
  xlab("") + ylab("Percentage Change in Weight (%)") +
  scale_fill_manual(name="",values=c("#FF7968",rgb(242/255,173/255,0/255))) +
  scale_y_continuous(limits=c(-30,30),breaks=seq(-25,25,by=5))

fig_A_update %>% 
  ggsave(.,filename =paste0(path_sga_bmi_folder,"/figures/distribution of pct change.jpg"),width = 8, height = 6)

