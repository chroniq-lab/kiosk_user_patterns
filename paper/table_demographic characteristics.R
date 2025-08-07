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

cosmos = read_csv("data/kupdat08_cosmos state stratified estimates.csv") %>% 
  dplyr::filter(State == "United States of America") %>% 
  dplyr::select(Stratification,strata_new,NcountBMI) %>% 
  group_by(Stratification) %>% 
  mutate(prop = round(NcountBMI/sum(NcountBMI),3)*100) %>% 
  ungroup() %>% 
  dplyr::rename(cosmos_prop = prop) %>% 
  dplyr::select(-NcountBMI) %>% 
  mutate(variable = case_when(Stratification == "race_ethnicity" ~ "race",
                              Stratification == "legalsex" ~ "female",
                              TRUE ~ Stratification),
         group = case_when(strata_new == "asian" ~ "NH Asian",
                           strata_new == "other" ~ "NH Other",
                           strata_new == "black" ~"NH Black",
                           strata_new == "hispanic" ~ "Hispanic",
                           strata_new == "white" ~ "NH White",
                           Stratification == "legalsex" & strata_new == "female" ~ "1",
                           Stratification == "legalsex" & strata_new == "male" ~ "0",
                           Stratification == "legalsex" & strata_new == "unknown_sex" ~ NA_character_,
                           Stratification == "urban" & strata_new == "urban" ~ "1",
                           Stratification == "urban" & strata_new == "rural" ~ "0",
                           TRUE ~ strata_new)) %>% 
  dplyr::select(variable, group, cosmos_prop) 

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
