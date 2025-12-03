rm(list=ls());gc();source(".Rprofile")


library(haven)
library(survey)
whq_l = read_xpt("C:/Cloud/OneDrive - Emory University/Papers/NHANES Subtypes Mortality/working/raw/2021-2023/WHQ_L.xpt")


nhanes_20212023_sens = readRDS(paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.RDS")) %>% 
  dplyr::filter(year == "20212023") %>% 
  left_join(whq_l %>% 
              mutate(self_height = WHD010*2.54) %>% 
              dplyr::select(SEQN,self_height),
            by = c("respondentid" = "SEQN"))


nhanes_20212023_sens_svy = nhanes_20212023_sens %>% 
  as_survey_design(ids = psu, strata = pseudostratum, 
                   weights  = mec2yweight, nest =TRUE) %>% 
  srvyr::filter(age >= 18,(pregnant %in% c(2,3) | is.na(pregnant))) %>%
  srvyr::filter(!is.na(height),!is.na(self_height))


m1 = svyglm(height ~ self_height + race3 + age_group_pursuant + female,design = nhanes_20212023_sens_svy,
            family=gaussian())

summary(m1)

broom::tidy(m1) %>% 
  write_csv("sensitivity/kupsens01_regression calibration equation.csv")
