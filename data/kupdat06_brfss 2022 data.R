# rm(list=ls());gc();source(".Rprofile")


# "_RACE1": Race/ethnicity
# "_RACEPR1": Race
# "_HISPANC": Hispanic


brfss2022 = open_dataset(paste0(path_brfss_parquet_folder, "/data/year=2022/"), format = "parquet") %>%
  dplyr::select(state, year, one_of("_PSU", "_STSTR", "_LLCPWT",
                                    "PREGNANT", "WEIGHT2", "HEIGHT3", "WTKG3", "HTM4", "_BMI5", "_BMI5CAT",
                                    "_SEX", "_AGEG5YR", "_AGE65YR", "_AGE80", "_RACE1","_RACEPR1", "_HISPANC",
                                    "MARITAL", "EDUCA", "INCOME3", "_HLTHPLN","_URBSTAT","DIABETE4")) %>%
  dplyr::rename(PSU = "_PSU",
                STSTR = "_STSTR",
                LLCPWT = "_LLCPWT")  |>
  mutate(
    state_names = case_when(
      state == 1 ~ "Alabama",
      state == 2 ~ "Alaska",
      state == 4 ~ "Arizona",
      state == 5 ~ "Arkansas",
      state == 6 ~ "California",
      state == 8 ~ "Colorado",
      state == 9 ~ "Connecticut",
      state == 10 ~ "Delaware",
      state == 11 ~ "District of Columbia",
      state == 12 ~ "Florida",
      state == 13 ~ "Georgia",
      state == 15 ~ "Hawaii",
      state == 16 ~ "Idaho",
      state == 17 ~ "Illinois",
      state == 18 ~ "Indiana",
      state == 19 ~ "Iowa",
      state == 20 ~ "Kansas",
      state == 21 ~ "Kentucky",
      state == 22 ~ "Louisiana",
      state == 23 ~ "Maine",
      state == 24 ~ "Maryland",
      state == 25 ~ "Massachusetts",
      state == 26 ~ "Michigan",
      state == 27 ~ "Minnesota",
      state == 28 ~ "Mississippi",
      state == 29 ~ "Missouri",
      state == 30 ~ "Montana",
      state == 31 ~ "Nebraska",
      state == 32 ~ "Nevada",
      state == 33 ~ "New Hampshire",
      state == 34 ~ "New Jersey",
      state == 35 ~ "New Mexico",
      state == 36 ~ "New York",
      state == 37 ~ "North Carolina",
      state == 38 ~ "North Dakota",
      state == 39 ~ "Ohio",
      state == 40 ~ "Oklahoma",
      state == 41 ~ "Oregon",
      state == 42 ~ "Pennsylvania",
      state == 44 ~ "Rhode Island",
      state == 45 ~ "South Carolina",
      state == 46 ~ "South Dakota",
      state == 47 ~ "Tennessee",
      state == 48 ~ "Texas",
      state == 49 ~ "Utah",
      state == 50 ~ "Vermont",
      state == 51 ~ "Virginia",
      state == 53 ~ "Washington",
      state == 54 ~ "West Virginia",
      state == 55 ~ "Wisconsin",
      state == 56 ~ "Wyoming",
      state == 66 ~ "Guam",
      state == 72 ~ "Puerto Rico",
      state == 78 ~ "Virgin Islands",
      TRUE ~ NA_character_),
    
    urban = case_when(
      
      `_URBSTAT` == 1 ~ "urban",
      `_URBSTAT` == 2 ~ "rural",
      TRUE ~ NA_character_
    ),
    
    sex = case_when( 
      `_SEX` == 1 ~ "Male",
      `_SEX` == 2 ~ "Female"),
    hispanic = case_when(
      `_HISPANC` == 1 ~ "Hispanic",
      `_HISPANC` == 2 ~ "Non-Hispanic",
      `_HISPANC` == 9 ~ "Non-Hispanic", # Don't know/Refused/Missing
      TRUE ~ NA_character_
    ),
    raceeth = case_when(
      `_RACE1` == 1 ~ "NH white",
      `_RACE1` == 2 ~ "NH black",
      `_RACE1` %in% c(3,5,6,7,9) ~ "NH other", # 9: Don't know/Refused/Missing
      `_RACE1` == 4 ~ "NH asian",
      `_RACE1` == 8 ~ "Hispanic",
      TRUE ~ NA_character_
    ),
    height_cm = HTM4/100,
    weight_kg = WTKG3/100,
    bmi = `_BMI5`/100,
    bmi_category = case_when(
      `_BMI5CAT` == 1 ~ "Underweight",
      `_BMI5CAT` == 2 ~ "Normal weight",
      `_BMI5CAT` == 3 ~ "Overweight",
      `_BMI5CAT` == 4 ~ "Obese",
      `_BMI5CAT` == 5 ~ "Missing",
      TRUE ~ NA_character_
    ),
    
    overweight = case_when(`_BMI5CAT` == 3 ~ 1,
                           `_BMI5CAT` %in% c(1,2,4) ~ 0,
                           TRUE ~ NA_real_),
    
    obesity = case_when(`_BMI5CAT` == 4 ~ 1,
                           `_BMI5CAT` %in% c(1,2,3) ~ 0,
                           TRUE ~ NA_real_),
    
    age_group = case_when(
      `_AGEG5YR` %in% sprintf("%02d", 1:5) ~ "18-44",
      `_AGEG5YR` %in% sprintf("%02d", 6:9) ~ "45-64", 
      `_AGEG5YR` %in% sprintf("%02d", 10:13) ~ "65plus",
      `_AGEG5YR` %in% 14 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    diagnosed_dm = case_when(DIABETE4 == 1 ~ 1,
                             DIABETE4 %in% c(2:7) ~ 0,
                             TRUE ~ NA_real_)
    
    
    ) %>%
  collect() %>% 
  mutate(age_group_pursuant = case_when(
           `_AGE80` %in% c(18:19) ~ "18-19",
           `_AGE80` >= 20 & `_AGE80` < 45 ~ "20-44", 
           `_AGE80` >= 45 & `_AGE80` < 65 ~ "45-64", 
           `_AGE80` >= 65  ~ "65plus",
           TRUE ~ NA_character_
         ))



nrow(brfss2022 %>%
       dplyr::filter(`_AGE80` >= 18, (PREGNANT %in% c(2,7,9) | is.na(PREGNANT))))

