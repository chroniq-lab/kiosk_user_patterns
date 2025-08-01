rm(list=ls());gc();source(".Rprofile")

kupana05 = read_csv("analysis/kupana05_pursuant model estimates.csv") %>% 
dplyr::filter(year == "2024-2025",stratification %in% c("national","age_group","ethnicity_updated","gender","urban")) %>% 
mutate(across(prevalence:prevalence_uci, ~ round(.*100, 1))) %>%
mutate(prevalence_95ci = paste0(prevalence, " (",round(prevalence_lci,2),", ",round(prevalence_uci,2),")")) %>%
dplyr::select(stratification, strata, measure, prevalence_95ci)




kupdat08 = read_csv(paste0(path_kiosk_user_patterns_repo,"/data/kupdat08_cosmos state stratified estimates.csv")) %>% 
  dplyr::filter(State == "United States of America",!Stratification %in% c("age_group","sex","sex_gender")) %>% 
  mutate(strata = case_when(strata_new == "65+" ~ "65plus",
                            strata_new == "rural" & Stratification == "urban" ~ "0",
                            strata_new == "urban" & Stratification == "urban" ~ "1",
                            strata_new == "other" & Stratification %in% c("urban") ~ "other_urban",
                            TRUE ~ strata_new))




paper_df = kupana05  %>% 
    pivot_wider(names_from = measure, values_from = prevalence_95ci) %>% 
    left_join(read_csv("analysis/kupana06_pursuant counts.csv") %>% 
          dplyr::select(stratification, strata, n,overweight,obesity) %>% 
          mutate(across(overweight:obesity, ~ round(.*100, 1))) %>%
          rename(direct_overweight = overweight, direct_obesity = obesity),
          by = c("stratification","strata")) %>%
    dplyr::select(stratification, strata, n, direct_overweight, direct_obesity, Overweight, Obesity) %>%

full_join(kupdat08 %>% 
dplyr::select(strata, overweight, obesity, NcountBMI) %>% 
mutate(across(overweight:obesity, ~ round(., 1))) %>%
        dplyr::rename(cosmos_overweight = overweight, cosmos_obesity = obesity),
        by = c("strata" = "strata"))  


paper_df %>% 
  write_csv("paper/table_prevalence of overweight and obesity.csv")
