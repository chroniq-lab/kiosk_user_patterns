rm(list=ls());gc();source(".Rprofile")



nhanes_20212023 = readRDS(paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.RDS")) %>% 
  dplyr::filter(year == "20212023")

nhanes_20212023_analytic = nhanes_20212023 %>% 
  dplyr::filter(age >= 18,(pregnant %in% c(2,3) | is.na(pregnant))) %>%
  dplyr::filter(!is.na(bmi))

c("age_group_pursuant","female","race3")


bind_rows(
  nhanes_20212023_analytic %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "national") %>% 
    mutate(strata = NA_character_),
  
  nhanes_20212023_analytic %>% 
    mutate(female = as.character(female)) %>% 
    group_by(female) %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "female") %>% 
    rename(strata = female),
  
  nhanes_20212023_analytic %>% 
    group_by(age_group_pursuant) %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "age_group_pursuant") %>% 
    rename(strata = age_group_pursuant),
  
  
  
  nhanes_20212023_analytic %>% 
    group_by(race3) %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "race") %>% 
    rename(strata = race3)) %>% 
  write_csv("analysis/kupana03_nhanes counts.csv")