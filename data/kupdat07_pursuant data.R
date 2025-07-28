rm(list = ls());gc(); source(".Rprofile");

# Dataset of users - 2022-2023 and 2024-2025

# Supplemental Data: Pregnant -- exclude from analysis ----------
pregnant_yes = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02-supplemental"), 
                            format="parquet", partitioning=c("data_label_name","year", "month"))  %>% 
  dplyr::filter(data_label_name %in% c("currently_pregnant_yes")) %>%
  dplyr::filter(year >= 2022) %>%
  dplyr::select(session_id_mask)  %>% 
  collect()


# Supplemental Data: Diagnosed Hypertension -- create covariate ----------

diagnosed_htn_yes = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02-supplemental"), 
                                 format="parquet", partitioning=c("data_label_name","year", "month"))  %>% 
  dplyr::filter(data_label_name %in% c("high_blood_pressure_diagnosis_yes")) %>%
  dplyr::filter(year >= 2022) %>%
  dplyr::select(session_id_mask)  %>% 
  collect()

diagnosed_htn_no = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02-supplemental"), 
                                format="parquet", partitioning=c("data_label_name","year", "month"))  %>% 
  dplyr::filter(data_label_name %in% c("high_blood_pressure_diagnosis_no")) %>%
  dplyr::filter(year >= 2022) %>%
  dplyr::select(session_id_mask)  %>% 
  collect()

# Supplemental Data: Diabetes -- create covariate ----------
diagnosed_dm_yes = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02-supplemental"), 
                                format="parquet", partitioning=c("data_label_name","year", "month"))  %>% 
  dplyr::filter(data_label_name %in% c("diabetes_yes")) %>%
  dplyr::filter(year >= 2022) %>%
  dplyr::select(session_id_mask)  %>% 
  collect()


diagnosed_dm_no = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02-supplemental"), 
                               format="parquet", partitioning=c("data_label_name","year", "month"))  %>% 
  dplyr::filter(data_label_name %in% c("diabetes_no")) %>%
  dplyr::filter(year >= 2022) %>%
  dplyr::select(session_id_mask) %>% 
  collect()





# map  <- read_csv(paste0(path_spatial_kiosks_repo,"/data/reference/pursuant_public_kiosk_address_w_county_CT_adj.csv")) %>% 
map  <- read_csv("https://raw.githubusercontent.com/jvargh7/spatial_kiosks/main/data/reference/pursuant_public_kiosk_address_w_county_CT_adj.csv") %>% 
  mutate(across(.cols=c("street1", "street2", "city", "state", "zipcode", 
                        "FIPS", "county", "urban"),.fns=~as.character(.))) %>% 
  distinct(FIPS,street1,.keep_all=TRUE)
# For the purpose of the proposal, I am not attempting any duplicate exclusion
kiosks = open_dataset(paste0(path_exploratory_kiosks_folder, "/working/ekdat02"), partitioning = c("year", "month")) %>%
  dplyr::filter(year >= 2022)  %>% 
  dplyr::filter(!is.na(bmi), bmi>=12, bmi<=60)  %>% 
  dplyr::filter(!is.na(age),age>=18)  %>% 
  dplyr::filter(!session_id_mask %in% pregnant_yes$session_id_mask) %>%
  mutate(age_group = case_when(age >=18  & age <= 19 ~ "18-19",
                               age >= 20 & age <= 44 ~ "20-44",
                               age >= 45 & age <= 64 ~ "45-64",
                               age >= 65 ~ "65plus",
                               TRUE ~ NA_character_)) |>
  mutate(obesity = case_when(bmi >= 30 ~ 1,
                             bmi < 30 ~ 0,
                             TRUE ~ NA_real_),
         overweight = case_when(bmi >= 25 & bmi < 30 ~ 1,
                                !is.na(bmi) ~ 0,
                                TRUE ~ NA_real_),
         year_group = case_when(year %in% c(2022,2023) ~ "2022-2023",
                                year %in% c(2024,2025) ~ "2024-2025",
                                TRUE ~ NA_character_)) |>
  mutate(diagnosed_htn = case_when(session_id_mask %in% diagnosed_htn_yes$session_id_mask ~ 1,
                                   session_id_mask %in% diagnosed_htn_no$session_id_mask ~ 0,
                                   year == 2025 ~ 0,
                                   year ==2024 & month >= 5 ~ 0,
                                   TRUE ~ NA_real_)) |>
  mutate(diagnosed_dm = case_when(session_id_mask %in% diagnosed_dm_yes$session_id_mask ~ 1,
                                  session_id_mask %in% diagnosed_dm_no$session_id_mask ~ 0, 
                                  year == 2025 ~ 0,
                                  year ==2024 & month >= 5 ~ 0,
                                  TRUE ~ NA_real_)) |>
  mutate(street1 = gsub("^\\s+|\\s+$", "", street1)) |> # Remove whitespace
  mutate(street1 = case_when(street1 == "602 Shelia St" ~ "602 SHELIA STREET",
                             street1 == "3101 W Kimberly Road" ~ "3101 W Kimberly Rd",
                             street1 == "9820 Callabridge Court" ~ "9820 Callabridge Ct",
                             street1 == "1600 E Tipton Street" ~ "1600 E Tipton St",
                             TRUE ~ street1),
         
         street2 = case_when(street1 %in% c("3209 PINEVILLE MATTHEWS RD", "900 PLEASANT GROVE BLVD") ~ "",
                             TRUE ~ street2)) |>
  mutate(state = ifelse(state == "Ak", "AK", state)) |>
  left_join(map %>% 
              dplyr::select(street1,state,urban,FIPS), by = c("street1","state"))


kiosks  %>% 
  write_dataset(paste0(path_kiosk_user_patterns_folder, "/working/processed/kupdat07_pursuant data"), 
                format = "parquet", partitioning = c("year", "month"))