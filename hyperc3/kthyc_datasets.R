


# 2024-2025
# ktana07 = open_dataset(paste0(path_kinetic_ssd_folder,"/local/working/ktdat07"))   %>% 

# 2022-2023, 2024-2025
ktana07 = open_dataset(paste0(path_kiosk_user_patterns_folder, "/working/processed/kupdat07_pursuant data"), 
                format = "parquet", partitioning = c("year", "month")) %>% 
  dplyr::filter(age %in% c(18:99),!is.na(gender), !is.na(ethnicity_updated)) %>% 
  collect() %>% 
  distinct(session_id_mask, .keep_all = TRUE) %>% 
  left_join(read_csv("https://raw.githubusercontent.com/jvargh7/spatial_kiosks/main/data/reference/model_covariates.csv") %>% 
              dplyr::select(-state) %>% 
              rename(urban_char = urban),by=c("FIPS")) %>% 
  dplyr::filter(!is.na(median_income))



  # 
  # mutate(age_group = factor(age_group),
  #        ethnicity_updated = factor(ethnicity_updated,levels=c("white", "black", "hispanic", "asian", "other")),
  #        gender = factor(gender,levels=c("male","female")),
  #        state_region = factor(state_region,levels = c("Northeast", "South", "North Central", "West")),
  #        state = factor(state),
  #        FIPS = factor(FIPS))
  

  

ktana07 %>% 
  saveRDS(.,paste0(path_kinetic_ssd_folder,"/kinetic-t2d-hyperc3/working/input/ktana07 dataset.RDS"))


# 2022-2023, 2024-2025
ktana08 = open_dataset(paste0(path_kiosk_user_patterns_folder, "/working/processed/kupdat07_pursuant data"), 
                       format = "parquet", partitioning = c("year", "month")) %>% 
  dplyr::filter(age %in% c(18:99),!is.na(gender), !is.na(ethnicity_updated)) %>% 
  collect() %>% 
  distinct(session_id_mask, .keep_all = TRUE) %>% 
  left_join(read_csv("https://raw.githubusercontent.com/jvargh7/spatial_kiosks/main/data/reference/model_covariates.csv") %>% 
              dplyr::select(-state) %>% 
              rename(urban_char = urban),by=c("FIPS")) %>% 
  dplyr::filter(!is.na(median_income))  %>% 
  mutate(age_group_gbd = case_when(age >=18 & age <25 ~ "18-24",
                                   age >=25 & age <45 ~ "25-44",
                                   age >=45 & age <65 ~ "45-64",
                                   TRUE ~ "65plus"))

ktana08 %>% 
  saveRDS(.,paste0(path_kinetic_ssd_folder,"/kinetic-t2d-hyperc3/working/input/ktana08 gbd comparison dataset.RDS"))


# Regression calibration

kupsens01_coefs_df = read_csv("C:/code/external/kiosk_user_patterns/sensitivity/kupsens01_regression calibration equation.csv")

kupsens01_coefs = kupsens01_coefs_df$estimate
names(kupsens01_coefs) = kupsens01_coefs_df$term

ktsens02 = open_dataset(paste0(path_kiosk_user_patterns_folder, "/working/processed/kupdat07_pursuant data"), 
                        format = "parquet", partitioning = c("year", "month")) %>% 
  dplyr::filter(age %in% c(18:99),!is.na(gender), !is.na(ethnicity_updated)) %>% 
  collect() %>% 
  distinct(session_id_mask, .keep_all = TRUE) %>% 
  left_join(read_csv("https://raw.githubusercontent.com/jvargh7/spatial_kiosks/main/data/reference/model_covariates.csv") %>% 
              dplyr::select(-state) %>% 
              rename(urban_char = urban),by=c("FIPS")) %>% 
  dplyr::filter(!is.na(median_income)) %>% 
  mutate(self_height = height_inches*2.54) %>% 
  mutate(self_height = case_when(is.na(self_height) ~ 100*sqrt((weight_lbs*0.453592)/bmi),
                                 TRUE ~ self_height)) %>% 
  mutate(rc_self_height = kupsens01_coefs["(Intercept)"] + kupsens01_coefs["self_height"]*self_height,
         rc_female = case_when(gender == "male" ~ 0,
                               gender == "female" ~ kupsens01_coefs["female"],
                               TRUE ~ 0),
         rc_race = case_when(ethnicity_updated == "hispanic" ~ 0,
                             ethnicity_updated == "white" ~ kupsens01_coefs["race3NH White"],
                             ethnicity_updated == "black" ~ kupsens01_coefs["race3NH Black"],
                             ethnicity_updated == "asian" ~ kupsens01_coefs["race3NH Asian"],
                             ethnicity_updated == "other" ~ kupsens01_coefs["race3NH Other"],
                             TRUE ~ 0
                             ),
         rc_age_group = case_when(age_group == "18-19" ~ 0,
                                  age_group == "20-44" ~ kupsens01_coefs["age_group_pursuant20-44"],
                                  age_group == "45-64" ~ kupsens01_coefs["age_group_pursuant45-64"],
                                  age_group == "65plus" ~ kupsens01_coefs["age_group_pursuant65+"],
                                  TRUE ~ 0
                                  )
         ) %>% 
  mutate(height_corrected = rc_self_height + rc_female + rc_race + rc_age_group) %>% 
  mutate(bmi_corrected =  case_when(is.na(height_corrected) ~ NA_real_,
                                    TRUE ~ (weight_lbs*0.453592)/(height_corrected/100)^2)) %>% 
  mutate(obesity_corrected = case_when(bmi_corrected >= 30 ~ 1,
                             bmi_corrected < 30 ~ 0,
                             TRUE ~ NA_real_),
         overweight_corrected = case_when(bmi_corrected >= 25 & bmi_corrected < 30 ~ 1,
                                !is.na(bmi_corrected) ~ 0,
                                TRUE ~ NA_real_))


ktsens02 %>% 
  saveRDS(.,paste0(path_kinetic_ssd_folder,"/kinetic-t2d-hyperc3/working/input/ktsens02 regression calibration dataset.RDS"))



