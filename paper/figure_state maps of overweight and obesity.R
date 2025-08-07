rm(list=ls());gc();source(".Rprofile")


source("functions/plot_map_estimates.R")  

cosmos = read_csv("data/kupdat08_cosmos states.csv")  %>% 
  rename(obesity = bmi_ge30,
         overweight = bmi_25to30)


brfss = read_csv("hyperc3/kuphyc302_brfss stratified estimates of overweight and obesity in parallel.csv") %>% 
  dplyr::filter(Stratification == "state_names") %>% 
  dplyr::select(strata, variable,estimate) %>% 
  pivot_wider(names_from="variable",values_from=estimate) %>% 
  rename(brfss_ob = obesity,
         brfss_ow = overweight)

pursuant = read_csv("analysis/kupana05_pursuant model estimates.csv") %>% 
  dplyr::filter(year == "2024-2025",stratification == "state_code") %>% 
  dplyr::select(stratification, strata, measure,prevalence) %>% 
  pivot_wider(names_from="measure",values_from=prevalence) %>% 
  mutate(Overweight = Overweight*100,
         Obesity = Obesity*100)



gbd = read_csv("data/kupdat10_gbd data.csv") %>% 
  dplyr::select(location_name,gbd_ow,gbd_ob)


df = brfss %>% 
  left_join(cosmos %>% 
              dplyr::select(state_fips,State,obesity,overweight) %>% 
              rename(cosmos_ob = obesity,
                     cosmos_ow = overweight),
            by = c("strata" = "State")) %>% 
  left_join(pursuant %>% 
              dplyr::select(-stratification) %>% 
              rename(pursuant_ob = Obesity,
                     pursuant_ow = Overweight),
            by = c("state_fips"="strata")) %>% 
  left_join(gbd,
            by = c("strata" = "location_name"))


figure1_theme = theme(legend.key.width = unit(3, "cm"))

fig1a = plot_map_estimates(df,level="state",plot_var = "brfss_ow",fips_var = "state_fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20)) + 
  figure1_theme

fig1b = plot_map_estimates(df,level="state",plot_var = "cosmos_ow",fips_var = "state_fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20)) + 
  figure1_theme

fig1c = plot_map_estimates(df,level="state",plot_var = "pursuant_ow",fips_var = "state_fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20)) + 
  figure1_theme

fig1d = plot_map_estimates(df,level="state",plot_var = "brfss_ob",fips_var = "state_fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20)) + 
  figure1_theme

fig1e = plot_map_estimates(df,level="state",plot_var = "cosmos_ob",fips_var = "state_fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20)) + 
  figure1_theme

fig1f = plot_map_estimates(df,level="state",plot_var = "pursuant_ob",fips_var = "state_fips",plot_var_label="",min=0,max=100,breaks=seq(0,100,by=20)) + 
  figure1_theme


library(ggpubr)

ggarrange(fig1a,fig1b,fig1c,fig1d,fig1e,fig1f,
          labels=LETTERS[1:6],
          common.legend = TRUE,
          legend="bottom") %>% 
  ggsave(.,filename=paste0(path_kiosk_user_patterns_folder,"/figures/qc/figure_1_state prevalence of overweight and obesity.jpg"),
         width = 12,height = 8)


# Supplementary Figure - Correlation with BRFSS 2022 and GBD 2024 --------------

figs2a = ggplot(data=df,
               aes(x=brfss_ow,
                   y=cosmos_ow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits =c(20,60))+
  theme_bw() +
  ggtitle("Overweight") +
  stat_cor(method = "spearman",cor.coef.name = "rho",r.digits=2,p.digits=2) +
  xlab("BRFSS 2022 (%)") +
  ylab("Cosmos 2024-2025 (%)")


figs2b = ggplot(data=df,
                aes(x=brfss_ow,
                    y=pursuant_ow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits =c(20,60))+
  theme_bw() +
  ggtitle("Overweight") +
  stat_cor(method = "spearman",cor.coef.name = "rho",r.digits=2,p.digits=2) +
  
  xlab("BRFSS 2022 (%)") +
  ylab("Pursuant 2024-2025 (%)")


figs2c = ggplot(data=df,
                aes(x=brfss_ob,
                    y=cosmos_ob)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits =c(20,60))+
  theme_bw() +
  ggtitle("Obesity") +
  stat_cor(method = "spearman",cor.coef.name = "rho",r.digits=2,p.digits=2) +
  
  xlab("BRFSS 2022 (%)") +
  ylab("Cosmos 2024-2025 (%)")


figs2d = ggplot(data=df,
                aes(x=brfss_ob,
                    y=pursuant_ob)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits =c(20,60))+
  theme_bw() +
  ggtitle("Obesity") +
  stat_cor(method = "spearman",cor.coef.name = "rho",r.digits=2,p.digits=2) +
  
  xlab("BRFSS 2022 (%)") +
  ylab("Pursuant 2024-2025 (%)")


figs2e = ggplot(data=df,
                aes(x=gbd_ow,
                    y=cosmos_ow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits =c(20,60))+
  theme_bw() +
  stat_cor(method = "spearman",cor.coef.name = "rho",r.digits=2,p.digits=2) +
  
  ggtitle("Overweight") +
  xlab("GBD 2024 Projection (%)") +
  ylab("Cosmos 2024-2025 (%)")


figs2f = ggplot(data=df,
                aes(x=gbd_ow,
                    y=pursuant_ow)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits =c(20,60))+
  theme_bw() +
  stat_cor(method = "spearman",cor.coef.name = "rho",r.digits=2,p.digits=2) +
  
  ggtitle("Overweight") +
  xlab("GBD 2024 Projection (%)") +
  ylab("Pursuant 2024-2025 (%)")


figs2g = ggplot(data=df,
                aes(x=gbd_ob,
                    y=cosmos_ob)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits =c(20,60))+
  theme_bw() +
  stat_cor(method = "spearman",cor.coef.name = "rho",r.digits=2,p.digits=2) +
  
  ggtitle("Obesity") +
  xlab("GBD 2024 Projection (%)") +
  ylab("Cosmos 2024-2025 (%)")


figs2h = ggplot(data=df,
                aes(x=gbd_ob,
                    y=pursuant_ob)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits =c(20,60))+
  theme_bw() +
  stat_cor(method = "spearman",cor.coef.name = "rho",r.digits=2,p.digits=2) +
  
  ggtitle("Obesity") +
  xlab("GBD 2024 Projection (%)") +
  ylab("Pursuant 2024-2025 (%)")


ggarrange(figs2a,figs2b,figs2c,figs2d,
          figs2e,figs2f,figs2g,figs2h,
          labels=LETTERS[1:8],
          nrow = 2,ncol=4,
          common.legend = TRUE,
          legend="bottom") %>% 
  ggsave(.,filename=paste0(path_kiosk_user_patterns_folder,"/figures/qc/figure_correlation of state prevalence of overweight and obesity.jpg"),
         width = 12,height = 8)
