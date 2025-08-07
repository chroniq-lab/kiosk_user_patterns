rm(list=ls());gc();source(".Rprofile")

source("analysis/kupana_county prevalence.R")


cor.test(df$cosmos_ob,df$places_ob)
cor.test(df$pursuant_ob,df$places_ob)



# FIGURE 2 --------------------

library(ggpubr)

source("functions/plot_map_estimates.R")  

figure2_theme = theme(legend.key.width = unit(3, "cm"))


fig2a = plot_map_estimates(df,level="county",plot_var = "cosmos_ow",fips_var = "fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20)) + 
  figure2_theme
fig2b = plot_map_estimates(df,level="county",plot_var = "pursuant_ow",fips_var = "fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20))+ 
  figure2_theme
fig2c = plot_map_estimates(df,level="county",plot_var = "cosmos_ob",fips_var = "fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20))+ 
  figure2_theme
fig2d = plot_map_estimates(df,level="county",plot_var = "pursuant_ob",fips_var = "fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20))+ 
  figure2_theme


ggarrange(fig2a,fig2b,fig2c,fig2d,
          labels=LETTERS[1:4],
          common.legend = TRUE,
          legend="bottom") %>% 
  ggsave(.,filename=paste0(path_kiosk_user_patterns_folder,"/figures/qc/figure_2_county prevalence of overweight and obesity.jpg"),
         width = 10,height = 8)


# FIGURE 3 --------------------

figure3_theme = theme(legend.key.width = unit(1.5, "cm"))


fig3a = plot_map_estimates(df,level="county",plot_var = "places_ob",fips_var = "fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20)) + 
  figure3_theme

fig3b = ggplot(data=df,
               aes(x=places_ob,
                   y=cosmos_ob)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(0,60)) +
  scale_y_continuous(limits =c(0,60))+
  theme_bw() +
  xlab("PLACES 2024-2025 (%)") +
  ylab("Cosmos 2024-2025 (%)")


fig3c = ggplot(data=df,
               aes(x=places_ob,
                   y=pursuant_ob)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(0,60)) +
  scale_y_continuous(limits =c(0,60))+
  theme_bw() +
  xlab("PLACES 2024-2025 (%)") +
  ylab("Pursuant 2024-2025 (%)")

fig3c

ggarrange(fig3a,
          ggarrange(fig3b,fig3c,labels=LETTERS[2:3],nrow=1,ncol=2),
          labels=c("A",""),nrow=2,ncol=1) %>% 
  ggsave(.,filename=paste0(path_kiosk_user_patterns_folder,"/figures/qc/figure_3_correlation of obesity between data sources.jpg"),
         width = 8,height = 8)


# SUPPLEMENTARY FIGURE 4 -----------------

sfig4a = plot_map_estimates(df,level="county",plot_var = "places_ob_se",fips_var = "fips",plot_var_label="",min=0,max=10,breaks=seq(0,10,by=2),palette = "YlOrRd") + 
  figure2_theme
sfig4b = plot_map_estimates(df,level="county",plot_var = "pursuant_ow_se",fips_var = "fips",plot_var_label="",min=0,max=10,breaks=seq(0,10,by=2),palette = "YlOrRd")+ 
  figure2_theme
sfig4c = plot_map_estimates(df,level="county",plot_var = "pursuant_ob_se",fips_var = "fips",plot_var_label="",min=0,max=10,breaks=seq(0,10,by=2),palette = "YlOrRd")+ 
  figure2_theme



ggarrange(sfig4a,sfig4b,sfig4c,
          nrow = 1, ncol = 3,
          labels=LETTERS[1:3],
          common.legend = TRUE,
          legend="bottom") %>% 
  ggsave(.,filename=paste0(path_kiosk_user_patterns_folder,"/figures/qc/figure_2_county standard errors of overweight and obesity.jpg"),
         width = 12,height = 3)
