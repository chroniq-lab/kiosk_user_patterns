rm(list=ls());gc();source(".Rprofile")

source("sensitivity/kupsens_county prevalence comparison.R")


cor.test(df$pursuant_ob,df$pursuant_ob_rc)
cor.test(df$pursuant_ow,df$pursuant_ow_rc)



# FIGURE 2 --------------------

library(ggpubr)

source("functions/plot_map_estimates.R")  

figure4_theme = theme(legend.key.width = unit(3, "cm"))


fig4a = plot_map_estimates(df,level="county",plot_var = "pursuant_ow_rc",fips_var = "strata",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20)) + 
  figure4_theme
fig4b = plot_map_estimates(df,level="county",plot_var = "pursuant_ob_rc",fips_var = "strata",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20))+ 
  figure4_theme

fig4c = ggplot(data=df,aes(x=pursuant_ow,y=pursuant_ow_rc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(0,60)) +
  scale_y_continuous(limits =c(0,60))+
  theme_bw() +
  xlab("Pursuant 2024-2025 (%)") +
  ylab("Pursuant (Corrected) 2024-2025 (%)")
  
fig4d = ggplot(data=df,aes(x=pursuant_ob,y=pursuant_ob_rc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(0,60)) +
  scale_y_continuous(limits =c(0,60))+
  theme_bw() +
  xlab("Pursuant 2024-2025 (%)") +
  ylab("Pursuant (Corrected) 2024-2025 (%)")


ggarrange(fig4a,fig4b,fig4c,fig4d,
          labels=LETTERS[1:4],
          common.legend = TRUE,
          legend="bottom") %>% 
  ggsave(.,filename=paste0(path_kiosk_user_patterns_folder,"/figures/sensitivity/figure_pursuant regression calibration county prevalence of overweight and obesity.jpg"),
         width = 10,height = 8)


