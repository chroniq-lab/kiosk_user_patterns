rm(list=ls());gc();source(".Rprofile")


path_nhanes_ckm_folder = "C:/Cloud/OneDrive - Emory University/Papers/NHANES Subtypes Mortality"
path_nhanes_ckm_newdm = "C:/Cloud/OneDrive - Emory University/Papers/NHANES Subtypes Mortality/working/new diabetes"

source("C:/code/external/nhanes_ckm/functions/combine_nhanes.R")
years_to_load <- c("2017Mar2020","20212023")

combined_nhanes <- combine_nhanes(path_nhanes_ckm_folder, years_to_load) %>%
  dplyr::filter(age >= 20,(pregnant %in% c(2,3) | is.na(pregnant))) %>%  
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
    female = gender - 1,
    race = factor(race,levels=c(1:5),labels=c("Hispanic","Hispanic","NH White","NH Black","NH Other")),
    insured_any = case_when(insured == 1 ~ 1,
                        insured == 2 ~ 0,
                        TRUE ~ NA_real_),
    dm_self_reported = case_when(dm_doc_told == 1 ~ 1,
                                TRUE ~ 0)
  ) %>% 
  dplyr::select(year, respondentid,weight, height, bmi, waistcircumference,
                psu, pseudostratum, mec2yweight,
                female, race, age, insured_any, dm_self_reported)

write_csv(combined_nhanes,paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.csv"))
saveRDS(combined_nhanes,paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.RDS"))
