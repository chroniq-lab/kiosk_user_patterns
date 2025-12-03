rm(list=ls());gc();source(".Rprofile")


path_nhanes_ckm_folder = "C:/Cloud/OneDrive - Emory University/Papers/NHANES Subtypes Mortality"
path_nhanes_ckm_newdm = "C:/Cloud/OneDrive - Emory University/Papers/NHANES Subtypes Mortality/working/new diabetes"

source("C:/code/external/nhanes_ckm/functions/combine_nhanes.R")
years_to_load <- c("2017Mar2020","20212023")

pre_filtered <- combine_nhanes(path_nhanes_ckm_folder, years_to_load)

write_csv(pre_filtered,paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_pre filtering nhanes data.csv"))
saveRDS(pre_filtered,paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_pre filtering nhanes data.RDS"))


combined_nhanes <- pre_filtered %>%
  dplyr::mutate(
    sbp = rowMeans(select(., systolic1, systolic2, systolic3), na.rm = TRUE),  # Calculate mean systolic blood pressure
    dbp = rowMeans(select(., diastolic1, diastolic2, diastolic3), na.rm = TRUE),  # Calculate mean diastolic blood pressure
  ) %>% 
  mutate(rx_chol = case_when(chol_med_taking == 1 ~ 1,
                             TRUE ~ 0),
         rx_htn = case_when(htn_med_taking == 1 ~ 1,
                            TRUE ~ 0),
         rx_insulin=  case_when(dm_insulin_taking == 1 ~ 1,
                                TRUE ~ 0),
         rx_otherdm = case_when(dm_bloodsugar_taking == 1 ~ 1,
                                TRUE ~ 0)) %>% 
  
  # https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/overviewbrief.aspx?Cycle=2017-2020 >> 
  # If done, the survey weights should be adjusted to reflect the longer period and larger population represented by the 2017-March 2020 files. 
  # For example, combining the 2015-2016 and 2017-March 2020 files would result in a data file representing a 5.2-year period, and the survey weights should be adjusted as follows: 
  # 2015-2016 survey weights should be multiplied by 2/5.2 (the fraction of the 5.2-year period represented by the 2015-2016 cycle) and 
  # likewise, the 2017-March 2020 survey weights should be multiplied by 3.2/5.2.
  mutate(
    # New smoking status based on combination of variables
    smoke_current = case_when(
      smoke_currently %in% c(1, 2) ~ 1,  # currently smokes every day or some days
      smoke_currently == 3 ~ 0,         # does not currently smoke
      smoke_history == 2 ~ 0,           # never smoked (even if smoke_currently is NA)
      smoke_history == 1 ~ 1,
      is.na(smoke_currently) ~ 0,  # missing but has history → likely a smoker
      TRUE ~ 0
    ),
    overweight = case_when(bmi >= 25 & bmi < 30 ~ 1,
                           bmi <12 | bmi > 60 ~ NA_real_,
                           !is.na(bmi) ~ 0,
                           TRUE ~ NA_real_),
    obesity = case_when(bmi >= 30 ~ 1,
                           bmi <12 | bmi > 60 ~ NA_real_,
                           !is.na(bmi) ~ 0,
                           TRUE ~ NA_real_),
    bmi_category = case_when(bmi <12 | bmi > 60 ~ NA_character_,
                             bmi < 18.5 ~ "underweight",
                             bmi < 25 ~ "normal",
                             bmi < 30 ~ "overweight",
                             bmi >= 30 ~ "obesity",
                             TRUE ~ NA_character_
                             ),
    
    age_group = case_when(age %in% c(18:44) ~ "18-44",
                          age %in% c(45:64) ~ "45-64",
                          age >= 64 ~ "65+",
                          age < 18 ~ NA_character_),
    
    age_group_pursuant = case_when(age %in% c(18:19) ~ "18-19",
                          age %in% c(20:44) ~ "20-44",
                          age %in% c(45:64) ~ "45-64",
                          age >= 64 ~ "65+",
                          age < 18 ~ NA_character_),
    
    female = gender - 1,
    race = factor(race,levels=c(1:5),labels=c("Hispanic","Hispanic","NH White","NH Black","NH Other")),
    race3 = factor(race3,levels=c(1,2,3,4,6,7),labels=c("Hispanic","Hispanic","NH White","NH Black","NH Asian","NH Other")),
    insured_any = case_when(insured == 1 ~ 1,
                        insured == 2 ~ 0,
                        TRUE ~ NA_real_),
    dm_self_reported = case_when(dm_doc_told == 1 ~ 1,
                                TRUE ~ 0)
  ) %>% 
  dplyr::select(year, respondentid,weight, height, bmi, bmi_category, pregnant, overweight, obesity, waistcircumference,
                psu, pseudostratum, mec2yweight,
                female, race,race3, age,age_group,age_group_pursuant, insured_any, dm_self_reported)

write_csv(combined_nhanes,paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.csv"))
saveRDS(combined_nhanes,paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.RDS"))

combined_nhanes = readRDS(paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.RDS"))

table(combined_nhanes$race3)
