rm(list=ls());gc();source(".Rprofile")


gbd_ow = read_csv(paste0(path_kiosk_user_patterns_folder,
                         "/working/raw/GBD/IHME_USA_OVERWEIGHT_OBESITY_PREVALENCE_1990_2050/IHME_USA_OVERWEIGHT_OBESITY_PREVALENCE_1990_2050_SUMMARY_OW_ONLY_Y2024M11D07.csv")) %>% 
  dplyr::filter(sex == "Both",year_id == 2024, age_group == "25 to 125",location_level=="State") %>% 
  dplyr::select(location_name, mean_AA_prev,lower_AA_prev,upper_AA_prev) %>% 
  mutate(across(contains("_prev"),~.*100)) %>% 
  rename(gbd_ow = mean_AA_prev,
         gbd_ow_lci = lower_AA_prev,
         gbd_ow_uci = upper_AA_prev)

gbd_ob = read_csv(paste0(path_kiosk_user_patterns_folder,
                         "/working/raw/GBD/IHME_USA_OVERWEIGHT_OBESITY_PREVALENCE_1990_2050/IHME_USA_OVERWEIGHT_OBESITY_PREVALENCE_1990_2050_SUMMARY_OB_Y2024M11D07.csv")) %>% 
  dplyr::filter(sex == "Both",year_id == 2024, age_group == "25 to 125",location_level=="State") %>% 
  dplyr::select(location_name, mean_AA_prev,lower_AA_prev,upper_AA_prev) %>% 
  mutate(across(contains("_prev"),~.*100)) %>% 
  rename(gbd_ob = mean_AA_prev,
         gbd_ob_lci = lower_AA_prev,
         gbd_ob_uci = upper_AA_prev)


left_join(gbd_ow,
          gbd_ob,
          by=c("location_name")) %>% 

write_csv("data/kupdat10_gbd data.csv")
