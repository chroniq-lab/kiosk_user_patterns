rm(list=ls());gc();source(".Rprofile")


brfss = read_csv("hyperc3/kuphyc301_brfss national estimates.csv") %>%
  mutate(estimate = case_when(variable == "height_cm" ~ estimate*100,
                              TRUE ~ estimate),
         lci = case_when(variable == "height_cm" ~ lci*100,
                              TRUE ~ lci),
         uci = case_when(variable == "height_cm" ~ uci*100,
                              TRUE ~ uci),
         ) %>%
  mutate(variable = case_when(variable == "raceeth" ~ "race",
                              variable == "sex" ~ "female",
                              TRUE ~ variable),
         group = case_when(group == "NH asian" ~ "NH Asian",
                           group == "NH other" ~ "NH Other",
                           group == "NH black" ~"NH Black",
                           group == "NH white" ~"NH White",
                           group == "hispanic" ~ "Hispanic",
                           group == "white" ~ "NH White",
                           group == "65plus" ~ "65+",
                           variable == "sex" & group == "Female" ~ "1",
                           variable == "sex" & group == "Male" ~ "0",
                           variable == "urban" & group == "urban" ~ "1",
                           variable == "urban" & group == "rural" ~ "0",
                           TRUE ~ group)) %>% 
  mutate(brfss_prop = paste0(round(estimate,2), " (",round(lci,2),", ",round(uci,2),")")) %>% 
  dplyr::select(variable,group,brfss_prop)

nhanes = read_csv("analysis/kupana10_nhanes national estimates.csv") %>% 
  mutate(prevalence_95ci = paste0(round(estimate,2), " (",round(lci,2),", ",round(uci,2),")")) %>%
  dplyr::select(variable, group, prevalence_95ci) %>% 
  rename(nhanes_prop = prevalence_95ci)

cosmos = read_csv("data/kupdat08_cosmos stratified.csv") %>% 
  dplyr::filter(Stratification %in% c("age_group_pursuant","legalsex","raceeth","urban")) %>% 
  
  dplyr::select(Stratification,strata,strata2,n) %>% 
  mutate(variable = case_when(Stratification == "raceeth" ~ "race",
                              Stratification == "legalsex" ~ "female",
                              TRUE ~ Stratification),
         group = case_when(strata == "Less than 18" ~ "<18",
                           strata == "18 or more and less than 20" ~ "18-19",
                           strata == "20 or more and less than 45" ~ "20-44",
                           strata == "45 or more and less than 65" ~ "45-64",
                           strata == "65 or more" ~ "65+",
                           
                           Stratification == "legalsex" & strata == "Female" ~ "1",
                           Stratification == "legalsex" & strata == "Male" ~ "0",
                           Stratification == "legalsex"  ~ "unknown_sex",
                           
                           
                           Stratification == "raceeth" & strata2 == "Hispanic or Latino" ~ "Hispanic",
                           Stratification == "raceeth" &  strata == "White" ~ "NH White",
                           Stratification == "raceeth" & strata == "Black or African American" ~"NH Black",
                           Stratification == "raceeth" & strata == "Asian" ~ "NH Asian",
                           Stratification == "raceeth" & strata == "American Indian or Alaska Native" ~ "NH Other",
                           Stratification == "raceeth" & strata == "Native Hawaiian or Other Pacific Islander" ~ "NH Other",
                           Stratification == "raceeth" & strata == "Other Race" ~ "NH Other",
                           Stratification == "raceeth" & strata == "None of the above" ~ "NH Other",
                           
                           Stratification == "urban" & strata == "10 Rural areas" ~ "0",
                           Stratification == "urban" & strata == "None of the above" ~ "unknown_urban",
                           Stratification == "urban" ~ "1",
                           TRUE ~ strata)) %>% 
  group_by(variable,group) %>% 
  summarize(group_total = sum(n)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(group_total)) %>% 
  group_by(variable) %>% 
  mutate(cosmos_prop = group_total/sum(group_total)) %>% 
  ungroup() %>% 
  dplyr::select(variable, group, cosmos_prop) %>% 
  mutate(cosmos_prop = round(cosmos_prop*100,1))

pursuant = read_csv("analysis/kupana06_pursuant counts.csv")  %>% 
  dplyr::select(stratification,strata,n) %>% 
  group_by(stratification) %>% 
  mutate(prop = round(n/sum(n),3)*100) %>% 
  ungroup() %>% 
  dplyr::rename(pursuant_prop = prop) %>% 
  dplyr::select(-n) %>% 
  mutate(variable = case_when(stratification == "gender" ~ "female",
                              stratification == "ethnicity_updated" ~ "race",
                              stratification == "age_group" ~ "age_group_pursuant",
                              TRUE ~ stratification),
         
         group = case_when(strata == "asian" ~ "NH Asian",
                           strata == "other" ~ "NH Other",
                           strata == "black" ~"NH Black",
                           strata == "hispanic" ~ "Hispanic",
                           strata == "white" ~ "NH White",
                           stratification == "gender" & strata == "female" ~ "1",
                           stratification == "gender" & strata == "male" ~ "0",
                           strata == "65plus" ~ "65+",
                           TRUE ~ strata)
         
         
         ) %>% 
  dplyr::select(variable,group,pursuant_prop)



table_df = nhanes %>% 
  full_join(brfss,
            by=c("variable","group")) %>%
  full_join(cosmos,
            by=c("variable","group")) %>% 
  full_join(pursuant,
            by=c("variable","group"))


write_csv(table_df,"paper/table_demographic_characteristics.csv")


cosmos = data_frame(wt_mean = 188,
                    wt_var = 23870,
                    bmi_mean = 30.5,
                    bmi_var = 18914,
                    ht_mean = 67,
                    ht_var = 32) %>% 
  mutate(wt_mean = 188*0.453592,
         wt_sd = sqrt(wt_var)*0.453592,
         ht_mean = 67*2.54,
         ht_sd = sqrt(ht_var)*2.54)

