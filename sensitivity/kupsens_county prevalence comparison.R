kupana05 = read_csv(paste0(path_kiosk_user_patterns_repo,"/analysis/kupana05_pursuant model estimates.csv")) %>% 
  dplyr::filter(year == "2024-2025") %>% 
  dplyr::select(stratification, strata, measure,prevalence) %>% 
  pivot_wider(names_from="measure",values_from=prevalence) %>% 
  mutate(Overweight = Overweight*100,
         Obesity = Obesity*100)

kupsens02 = read_csv(paste0(path_kiosk_user_patterns_repo,"/sensitivity/kupsens02_pursuant model estimates after regression calibration.csv")) %>% 
  dplyr::filter(year == "2024-2025") %>% 
  dplyr::select(stratification, strata, measure,prevalence) %>% 
  pivot_wider(names_from="measure",values_from=prevalence) %>% 
  mutate(Overweight = Overweight*100,
         Obesity = Obesity*100)


df = kupana05 %>% 
              dplyr::select(-stratification) %>% 
              rename(pursuant_ob = Obesity,
                     pursuant_ow = Overweight) %>% 
  left_join(kupsens02 %>% 
              dplyr::select(-stratification) %>% 
              rename(pursuant_ob_rc = Obesity,
                     pursuant_ow_rc = Overweight),
            by = c("strata"="strata")) 


