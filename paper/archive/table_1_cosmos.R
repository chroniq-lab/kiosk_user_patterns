rm(list=ls());gc();source(".Rprofile")


state_stratified = read_csv(paste0(path_kiosk_user_patterns_repo,"/data/kupdat08_cosmos state stratified estimates.csv")) %>% 
  dplyr::filter(State == "United States of America",Stratification %in% c("state","age_group","race_ethnicity","legalsex","urban")) %>% 
  group_by(Stratification) %>% 
  mutate(p = NcountBMI/sum(NcountBMI)) %>% 
  dplyr::select(Stratification,strata_new,p) 
