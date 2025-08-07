
cosmos = read_csv("data/kupdat08_cosmos counties.csv") %>% 
  distinct(fips,.keep_all=TRUE) %>% 
  rename(Obesity = bmi_ge30,
         Overweight = bmi_25to30)

places = read_csv("data/kupdat09_places county estimates.csv") %>% 
  mutate(obesity_se = (obesity - obesity_lci)/1.96)

pursuant = read_csv("analysis/kupana05_pursuant model estimates.csv") %>% 
  dplyr::filter(year == "2024-2025") %>% 
  dplyr::select(stratification, strata, measure,prevalence) %>% 
  pivot_wider(names_from="measure",values_from=prevalence) %>% 
  mutate(Overweight = Overweight*100,
         Obesity = Obesity*100)


pursuant_se = read_csv("analysis/kupana05_pursuant model estimates.csv") %>% 
  dplyr::filter(year == "2024-2025") %>% 
  mutate(se = (prevalence - prevalence_lci)/1.96) %>% 
  dplyr::select(stratification, strata, measure,se) %>% 
  pivot_wider(names_from="measure",values_from=se) %>% 
  mutate(Overweight_se = Overweight*100,
         Obesity_se = Obesity*100) %>%
  # These are standard errors but not multiplied by 100
  dplyr::select(-Overweight,-Obesity)


df = places %>% 
  dplyr::rename(places_ob = obesity,
                places_ob_se = obesity_se) %>% 
  left_join(cosmos %>% 
              dplyr::select(fips,County_StateAbb,Obesity,Overweight) %>% 
              rename(cosmos_ob = Obesity,
                     cosmos_ow = Overweight),
            by = c("fips")) %>% 
  left_join(pursuant %>% 
              dplyr::select(-stratification) %>% 
              rename(pursuant_ob = Obesity,
                     pursuant_ow = Overweight),
            by = c("fips"="strata")) %>% 
  left_join(pursuant_se %>% 
              dplyr::select(-stratification) %>% 
              rename(pursuant_ob_se = Obesity_se,
                     pursuant_ow_se = Overweight_se),
            by = c("fips"="strata")) 
