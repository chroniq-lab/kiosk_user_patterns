rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/kinetic-t2d/functions/cosmos_fips.R")

counties = readxl::read_xlsx(paste0(path_kiosk_user_patterns_folder,"/working/raw/Cosmos/Cosmos Table Harmonized.xlsx"),sheet="county") %>% 
  dplyr::filter(Stratification == "county") %>% 
  cosmos_fips(.,var_name="strata",region = "county") %>% 
  dplyr::filter(!County_StateAbb %in% c("None of the above")) %>% 
  mutate(bmi_30to35 = bmi_ge30 - rowSums(.[,c("bmi_35to40","bmi_ge40")],na.rm=TRUE),
         bmi_185to25 = case_when(is.na(bmi_ge30) & is.na(bmi_lt185) & is.na(bmi_25to30)  ~ NA_real_,
                                 TRUE ~ 100 - rowSums(.[,c("bmi_ge30","bmi_lt185","bmi_25to30")],na.rm=TRUE)),
         n = case_when(n == "10 or fewer" ~ NA_real_,
                       TRUE ~ as.numeric(n)))

write_csv(counties, "data/kupdat08_cosmos counties.csv")


states = readxl::read_xlsx(paste0(path_kiosk_user_patterns_folder,"/working/raw/Cosmos/Cosmos Table Harmonized.xlsx"),sheet="other") %>% 
  dplyr::filter(Stratification == "state_name") %>% 
  cosmos_fips(.,var_name="strata",region = "state") %>% 
  dplyr::filter(!State %in% c("None of the above")) %>% 
  mutate(bmi_35to40 = bmi_ge30 - rowSums(.[,c("bmi_30to35","bmi_ge40")],na.rm=TRUE),
         bmi_185to25 = case_when(is.na(bmi_ge30) & is.na(bmi_lt185) & is.na(bmi_25to30)  ~ NA_real_,
                                 TRUE ~ 100 - rowSums(.[,c("bmi_ge30","bmi_lt185","bmi_25to30")],na.rm=TRUE)),
         n = case_when(n == "10 or fewer" ~ NA_real_,
                       TRUE ~ as.numeric(n)))

write_csv(states, "data/kupdat08_cosmos states.csv")



stratified = readxl::read_xlsx(paste0(path_kiosk_user_patterns_folder,"/working/raw/Cosmos/Cosmos Table Harmonized.xlsx"),sheet="other") %>% 
  dplyr::filter(!Stratification == "state_name") %>% 
  mutate(bmi_35to40 = bmi_ge30 - rowSums(.[,c("bmi_30to35","bmi_ge40")],na.rm=TRUE),
         bmi_185to25 = case_when(is.na(bmi_ge30) & is.na(bmi_lt185) & is.na(bmi_25to30)  ~ NA_real_,
                                 TRUE ~ 100 - rowSums(.[,c("bmi_ge30","bmi_lt185","bmi_25to30")],na.rm=TRUE)),
         n = case_when(n == "10 or fewer" ~ NA_real_,
                       TRUE ~ as.numeric(n))) 
write_csv(stratified, "data/kupdat08_cosmos stratified.csv")
