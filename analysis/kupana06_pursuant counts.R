rm(list=ls());gc();source(".Rprofile")


kupdat07 = open_dataset(paste0(path_kiosk_user_patterns_folder, "/working/processed/kupdat07_pursuant data"), 
                                             format = "parquet", partitioning = c("year", "month")) %>% 
  dplyr::filter(age %in% c(18:99),year>=2024,!is.na(gender), !is.na(ethnicity_updated)) %>% 
  collect() %>% 
  distinct(session_id_mask, .keep_all = TRUE) %>% 
  mutate(normal = case_when(bmi >= 18.5 & bmi < 25 ~ 1,
                                !is.na(bmi) ~ 0,
                                TRUE ~ NA_real_),
         underweight = case_when(bmi < 18.5 ~ 1,
                            !is.na(bmi) ~ 0,
                            TRUE ~ NA_real_))

mean(kupdat07$bmi)
sd(kupdat07$bmi)


kupdat07 %>% 
  summarize(n = n(),
            weight = mean(weight_lbs*0.453592,na.rm=TRUE),
            sd_weight = sd(weight_lbs*0.453592,na.rm=TRUE),
            height = mean(height_inches*2.54,na.rm=TRUE),
            sd_height = sd(height_inches*2.54,na.rm=TRUE),
            underweight = mean(underweight),
            normal = mean(normal),
            overweight = mean(overweight),
            obesity = mean(obesity)) %>%
  mutate(stratification = "national") %>% 
  mutate(strata = NA_character_)


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
