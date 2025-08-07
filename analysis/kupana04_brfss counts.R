rm(list=ls());gc();source(".Rprofile")

source("data/kupdat06_brfss 2022 data.R")

c("age_group_pursuant","sex","raceeth","urban","state_names")

brfss2022_analytic = brfss2022 %>% 
  dplyr::filter(!is.na(age_group_pursuant)) %>%
  dplyr::filter(sex == "Male" |(sex == "Female" & PREGNANT %in% c(2,7,9)) | (sex == "Female" & is.na(PREGNANT))) %>%
  dplyr::filter(state != 66 & state != 72 & state != 78) %>%
  dplyr::filter(!is.na(bmi))


bind_rows(
  brfss2022_analytic %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "national") %>% 
    mutate(strata = NA_character_),
  brfss2022_analytic %>% 
    group_by(sex) %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "sex") %>% 
    rename(strata = sex),
  
  brfss2022_analytic %>% 
    group_by(age_group_pursuant) %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "age_group_pursuant") %>% 
    rename(strata = age_group_pursuant),
  
  
  
  brfss2022_analytic %>% 
    group_by(raceeth) %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "raceeth") %>% 
    rename(strata = raceeth),
  
  brfss2022_analytic %>% 
    group_by(urban) %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "urban") %>% 
    rename(strata = urban),
  
  brfss2022_analytic %>% 
    group_by(state_names) %>% 
    summarize(n = n(),
              overweight = mean(overweight),
              obesity = mean(obesity)) %>%
    mutate(stratification = "state_names") %>% 
    rename(strata = state_names)
  
) %>% 
  write_csv("analysis/kupana04_brfss counts.csv")