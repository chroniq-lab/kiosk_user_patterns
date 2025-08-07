rm(list=ls());gc();source(".Rprofile")


kupana10 = read_csv("analysis/kupana10_nhanes stratified estimates of overweight and obesity.csv") %>%
  mutate(prevalence_95ci = paste0(round(estimate,2), " (",round(lci,2),", ",round(uci,2),")")) %>%
  dplyr::select(Stratification, strata, variable, prevalence_95ci) %>% 
  pivot_wider(names_from=variable,values_from=prevalence_95ci) %>% 
  left_join(read_csv("analysis/kupana03_nhanes counts.csv") %>% 
              dplyr::select(stratification,strata,n) %>% 
              rename(nhanes_n = n),
            by = c("Stratification"="stratification","strata")) %>% 
  rename(nhanes_overweight = overweight,
         nhanes_obesity = obesity,
         variable = Stratification,
         group = strata)


kuphyc301 = read_csv("hyperc3/kuphyc302_brfss stratified estimates of overweight and obesity in parallel.csv") %>%
  mutate(prevalence_95ci = paste0(round(estimate,2), " (",round(lci,2),", ",round(uci,2),")")) %>% 
  dplyr::filter(Stratification != "state_names") %>% 
  dplyr::select(Stratification, strata, variable, prevalence_95ci) %>% 
  pivot_wider(names_from=variable,values_from=prevalence_95ci) %>% 
  left_join(read_csv("analysis/kupana04_brfss counts.csv") %>% 
              dplyr::select(stratification, strata, n) %>% 
              rename(brfss_n = n),
            by = c("Stratification" = "stratification","strata")) %>%
  mutate(variable = case_when(Stratification == "raceeth" ~ "race",
                              Stratification == "sex" ~ "female",
                              TRUE ~ Stratification),
         group = case_when(strata == "NH asian" ~ "NH Asian",
                           strata == "NH other" ~ "NH Other",
                           strata == "NH black" ~"NH Black",
                           strata == "NH white" ~"NH White",
                           strata == "hispanic" ~ "Hispanic",
                           strata == "white" ~ "NH White",
                           strata == "65plus" ~ "65+",
                           Stratification == "sex" & strata == "Female" ~ "1",
                           Stratification == "sex" & strata == "Male" ~ "0",
                           Stratification == "urban" & strata == "urban" ~ "1",
                           Stratification == "urban" & strata == "rural" ~ "0",
                           TRUE ~ strata))  %>% 
  rename(brfss_overweight = overweight,
         brfss_obesity = obesity) %>% 
  dplyr::select(variable,group,brfss_n,brfss_overweight,brfss_obesity)



kupana05 = read_csv("analysis/kupana05_pursuant model estimates.csv") %>% 
dplyr::filter(year == "2024-2025",stratification %in% c("national","age_group","ethnicity_updated","gender","urban")) %>% 
mutate(across(prevalence:prevalence_uci, ~ round(.*100, 1))) %>%
mutate(prevalence_95ci = paste0(prevalence, " (",round(prevalence_lci,2),", ",round(prevalence_uci,2),")"))  %>%
  dplyr::select(stratification, strata, measure, prevalence_95ci) %>% 
  pivot_wider(names_from = measure, values_from = prevalence_95ci) %>% 
  rename(model_overweight  = Overweight,
         model_obesity = Obesity) %>% 
  left_join(read_csv("analysis/kupana06_pursuant counts.csv") %>% 
              dplyr::select(stratification, strata, n,overweight,obesity) %>% 
              mutate(across(overweight:obesity, ~ round(.*100, 1))) %>%
              rename(direct_overweight = overweight, direct_obesity = obesity),
            by = c("stratification","strata"))  %>% 
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
         
         
  )   %>%
  dplyr::select(variable, group, n, direct_overweight, direct_obesity, model_overweight, model_obesity)




kupdat08 = read_csv(paste0(path_kiosk_user_patterns_repo,"/data/kupdat08_cosmos stratified.csv")) %>% 
  dplyr::filter(Stratification %in% c("age_group_pursuant","legalsex","raceeth","urban")) %>% 
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
  rename(obesity = bmi_ge30,
         overweight = bmi_25to30) %>% 
  group_by(variable,group) %>% 
  summarize(overweight = sum(overweight*n)/sum(n),
            obesity = sum(obesity*n)/sum(n),
            n = sum(n)) %>% 
  dplyr::select(variable, group, overweight, obesity,n) %>% 
  mutate(across(overweight:obesity, ~ round(., 1))) %>%
  dplyr::rename(cosmos_overweight = overweight, 
                cosmos_obesity = obesity,
                cosmos_n = n)




paper_df = kupana05  %>% 
  full_join(kupdat08 ,
        by = c("variable","group"))  %>% 
  left_join(kupana10,
            by=c("variable","group")) %>%
  full_join(kuphyc301,
            by=c("variable","group"))


paper_df %>% 
  write_csv("paper/table_prevalence of overweight and obesity.csv")
