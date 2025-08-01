rm(list=ls());gc();source(".Rprofile")


kupdat07 = open_dataset(paste0(path_kiosk_user_patterns_folder, "/working/processed/kupdat07_pursuant data"), 
                format = "parquet", partitioning = c("year", "month")) %>% 
  dplyr::filter(age %in% c(18:99),!is.na(gender), !is.na(ethnicity_updated),year>=2024) %>% 
  collect() %>% 
  distinct(session_id_mask, .keep_all = TRUE) 



bind_rows(
kupdat07 %>% 
     summarize(n = n(),
               overweight = mean(overweight),
                obesity = mean(obesity)) %>%
     mutate(stratification = "national") %>% 
     mutate(strata = NA_character_),
kupdat07 %>% 
     group_by(gender) %>% 
     summarize(n = n(),
               overweight = mean(overweight),
                obesity = mean(obesity)) %>%
     mutate(stratification = "gender") %>% 
     rename(strata = gender),

    kupdat07 %>% 
     group_by(age_group) %>% 
     summarize(n = n(),
               overweight = mean(overweight),
               obesity = mean(obesity)) %>%
     mutate(stratification = "age_group") %>% 
     rename(strata = age_group),

    

    kupdat07 %>% 
     group_by(ethnicity_updated) %>% 
     summarize(n = n(),
               overweight = mean(overweight),
               obesity = mean(obesity)) %>%
     mutate(stratification = "ethnicity_updated") %>% 
     rename(strata = ethnicity_updated),

     kupdat07 %>% 
     group_by(urban) %>% 
     summarize(n = n(),
               overweight = mean(overweight),
               obesity = mean(obesity)) %>%
     mutate(stratification = "urban") %>% 
     rename(strata = urban)

) %>% 
write_csv("analysis/kupana06_pursuant counts.csv")
