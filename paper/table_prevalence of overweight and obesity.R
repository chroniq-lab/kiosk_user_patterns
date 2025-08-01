rm(list=ls());gc();source(".Rprofile")

kupana05 = read_csv("analysis/kupana05_pursuant model estimates.csv") %>% 
dplyr::filter(year == "2024-2025",stratification %in% c("national","age_group","ethnicity_updated","gender","urban")) %>% 
mutate(across(prevalence:prevalence_uci, ~ round(.*100, 1))) %>%
mutate(prevalence_95ci = paste0(prevalence, " (",round(prevalence_lci,2),", ",round(prevalence_uci,2),")")) %>%
dplyr::select(stratification, strata, measure, prevalence_95ci)







kupana05  %>% 
pivot_wider(names_from = measure, values_from = prevalence_95ci) %>% 
left_join(read_csv("analysis/kupana06_pursuant counts.csv") %>% 
          dplyr::select(stratification, strata, n,overweight,obesity) %>% 
          mutate(across(overweight:obesity, ~ round(.*100, 1))) %>%
          rename(direct_overweight = overweight, direct_obesity = obesity),
          by = c("stratification","strata")) %>%
dplyr::select(stratification, strata, n, direct_overweight, direct_obesity, Overweight, Obesity) %>%
write_csv("paper/table_prevalence of overweight and obesity.csv")
