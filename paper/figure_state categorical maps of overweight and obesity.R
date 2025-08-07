rm(list=ls());gc();source(".Rprofile")



cosmos = read_csv("data/kupdat08_cosmos state stratified estimates.csv") %>% 
  dplyr::filter(strata_new == "total")

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


fig_df = brfss %>% 
  left_join(cosmos %>% 
              dplyr::select(state_fips,State,obesity,overweight) %>% 
              rename(cosmos_ob = obesity,
                     cosmos_ow = overweight),
            by = c("strata" = "State")) %>% 
  left_join(pursuant %>% 
              dplyr::select(-stratification) %>% 
              rename(pursuant_ob = Obesity,
                     pursuant_ow = Overweight),
            by = c("state_fips"="strata"))

source("functions/plot_map_estimates_categorical.R")  


figure1_theme = theme(legend.key.width = unit(3, "cm"))

fig1a = plot_map_estimates_categorical(fig_df,level="state",plot_var = "brfss_ow",fips_var = "state_fips",plot_var_label="") + 
  figure1_theme

fig1b = plot_map_estimates_categorical(fig_df,level="state",plot_var = "cosmos_ow",fips_var = "state_fips",plot_var_label="") + 
  figure1_theme

fig1c = plot_map_estimates_categorical(fig_df,level="state",plot_var = "pursuant_ow",fips_var = "state_fips",plot_var_label="") + 
  figure1_theme

fig1d = plot_map_estimates_categorical(fig_df,level="state",plot_var = "brfss_ob",fips_var = "state_fips",plot_var_label="") + 
  figure1_theme

fig1e = plot_map_estimates_categorical(fig_df,level="state",plot_var = "cosmos_ob",fips_var = "state_fips",plot_var_label="") + 
  figure1_theme

fig1f = plot_map_estimates_categorical(fig_df,level="state",plot_var = "pursuant_ob",fips_var = "state_fips",plot_var_label="") + 
  figure1_theme


library(ggpubr)

ggarrange(fig1a,fig1b,fig1c,fig1d,fig1e,fig1f,
          labels=LETTERS[1:6],
          common.legend = TRUE,
          legend="bottom") %>% 
  ggsave(.,filename=paste0(path_kiosk_user_patterns_folder,"/figures/qc/figure_1_state categorical prevalence of overweight and obesity.jpg"),
         width = 12,height = 8)

