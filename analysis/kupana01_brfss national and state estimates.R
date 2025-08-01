rm(list=ls());gc();source(".Rprofile")

source("data/kupdat06_brfss 2022 data.R")


library(srvyr)
options(survey.lonely.psu = "adjust")
brfss2022_svy <- brfss2022 %>% 
  as_survey_design(.data = .,
                   ids = PSU,
                   strata = STSTR,
                   weights = LLCPWT, 
                   nest = TRUE, 
                   ps = "brewer",variance = "YG") %>% 
  srvyr::filter(!is.na(age_group_pursuant)) %>%
  srvyr::filter(sex == "Male" |(sex == "Female" & PREGNANT %in% c(2,7,9))) %>%
  srvyr::filter(state != 66 & state != 72 & state != 78) %>%
  srvyr::filter(!is.na(bmi))
  


# Method 3 of https://github.com/chroniq-lab/tutorials/blob/main/NHANES/tutnha03_descriptive%20characteristics.R
# source("C:/code/external/functions/survey/svysummary.R")

source("https://raw.githubusercontent.com/jvargh7/functions/main/survey/svysummary.R")


# NATIONAL ESTIMATES ----------------

national_estimates = svysummary(
  brfss2022_svy,
  c_vars = c("height_cm","weight_kg","bmi"),
  # p_vars = c("diagnosed_dm"),
  g_vars=c("age_group_pursuant","sex",
           "urban","raceeth","age_group","bmi_category"))

write_csv(national_estimates,"analysis/kupana01_brfss national estimates.csv")




# STRATIFIED ESTIMATES ----------------


stratified_estimates = map_dfr(c("age_group_pursuant","sex","raceeth","urban","state_names"),
                               
                               function(v){
                                 print(v)
                                 se_df = svysummary(
                                   brfss2022_svy,
                                   p_vars = c("overweight","obesity"),
                                   id_vars=v) %>% 
                                    mutate(Stratification = v) %>% 
                                    dplyr::rename(strata = substitute(v))
                                 
                                 return(se_df)
                                 
                                 
                               }
                               
                               )



write_csv(stratified_estimates,"analysis/kupana01_brfss stratified estimates of overweight and obesity.csv")

