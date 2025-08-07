rm(list=ls());gc();source(".Rprofile")

source("https://raw.githubusercontent.com/jvargh7/functions/main/survey/svysummary.R")


nhanes_20212023 = readRDS(paste0(path_kiosk_user_patterns_folder,"/working/processed/kupdat05_nhanes data.RDS")) %>% 
  dplyr::filter(year == "20212023")


library(srvyr)

nhanes_20212023_svy = nhanes_20212023 %>% 
  as_survey_design(ids = psu, strata = pseudostratum, 
                   weights  = mec2yweight, nest =TRUE) %>% 
  srvyr::filter(age >= 18,(pregnant %in% c(2,3) | is.na(pregnant))) %>%
  srvyr::filter(!is.na(bmi))


# NATIONAL ESTIMATES ----------------
# "urban",
national_estimates = svysummary(
  nhanes_20212023_svy,
  c_vars = c("height","weight","bmi"),
  p_vars = c("dm_self_reported"),
  g_vars=c("age_group_pursuant","female","age_group",
           "race","bmi_category"))

write_csv(national_estimates,"analysis/kupana10_nhanes national estimates.csv")




# STRATIFIED ESTIMATES ----------------
# "urban",,"state_names"

stratified_estimates = map_dfr(c("age_group_pursuant","female","race"),
                               
                               function(v){
                                 print(v)
                                 se_df = svysummary(
                                   nhanes_20212023_svy,
                                   p_vars = c("overweight","obesity"),
                                   id_vars=v) %>% 
                                   mutate(Stratification = v) %>% 
                                   dplyr::rename(strata = v) %>% 
                                   mutate(strata = as.character(strata))
                                 
                                 return(se_df)
                                 
                                 
                               }
                               
)



write_csv(stratified_estimates,"analysis/kupana10_nhanes stratified estimates of overweight and obesity.csv")
