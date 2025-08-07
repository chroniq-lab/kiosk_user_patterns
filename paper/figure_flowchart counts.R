rm(list=ls());gc();source(".Rprofile")

# BRFSS setup ------------

source("data/kupdat06_brfss 2022 data.R")
# Pursuant setup -----------

pregnant_yes = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02-supplemental"), 
                            format="parquet", partitioning=c("data_label_name","year", "month"))  %>% 
  dplyr::filter(data_label_name %in% c("currently_pregnant_yes")) %>%
  dplyr::filter(year >= 2022) %>%
  dplyr::select(session_id_mask)  %>% 
  collect()



p0 = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02"), partitioning = c("year", "month")) %>%
  dplyr::filter(year >= 2024)  %>% 
  dplyr::filter(!is.na(age),age>=18)

nrow(p0)

  
n0 = readRDS(paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.RDS")) %>% 
  dplyr::filter(year == "20212023") %>% 
  dplyr::filter(age >=18)


b0 = brfss2022 %>%
  dplyr::filter(`_AGE80` >= 18) %>% 
  nrow()


## Non-pregnant -----------

p1 = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02"), partitioning = c("year", "month")) %>%
  dplyr::filter(year >= 2024)  %>% 
  dplyr::filter(!is.na(age),age>=18) %>% 
  dplyr::filter(!session_id_mask %in% pregnant_yes$session_id_mask) 
nrow(p1)

n1 = readRDS(paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.RDS")) %>% 
  dplyr::filter(year == "20212023") %>% 
  dplyr::filter(age >=18) %>%
  dplyr::filter((pregnant %in% c(2,3) | is.na(pregnant)))


b1 = brfss2022 %>%
  dplyr::filter(`_AGE80` >= 18) %>%
  dplyr::filter(sex == "Male" |(sex == "Female" & PREGNANT %in% c(2,7,9)) | (sex == "Female" & is.na(PREGNANT))) %>% 
  nrow()

## With BMI -----------
p2 = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02"), partitioning = c("year", "month")) %>%
  dplyr::filter(year >= 2024)  %>% 
  dplyr::filter(!is.na(age),age>=18) %>% 
  dplyr::filter(!session_id_mask %in% pregnant_yes$session_id_mask) %>% 
  dplyr::filter(!is.na(bmi), bmi>=12, bmi<=60)

nrow(p2)

n2 = readRDS(paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.RDS")) %>% 
  dplyr::filter(year == "20212023") %>% 
  dplyr::filter(age >=18) %>% 
  dplyr::filter((pregnant %in% c(2,3) | is.na(pregnant))) %>% 
  dplyr::filter(!is.na(bmi))

b2 = brfss2022 %>%
  dplyr::filter(`_AGE80` >= 18) %>%
  dplyr::filter(sex == "Male" |(sex == "Female" & PREGNANT %in% c(2,7,9)) | (sex == "Female" & is.na(PREGNANT))) %>% 
  dplyr::filter(state != 66 & state != 72 & state != 78) %>%
  dplyr::filter(!is.na(bmi)) %>% 
  nrow()
