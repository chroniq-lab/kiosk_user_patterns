rm(list = ls());gc(); source(".Rprofile");

kiosks = open_dataset(paste0(path_kinetic_ssd_folder, "/working/ktdat07"),format = "parquet", partitioning = c("year", "month"))

kiosks %>% 
group_by(urban) %>% 
tally() %>%
ungroup() %>%  
collect() %>% 
mutate(p = n/sum(n))


kiosks  %>% 
summarize(mean_bmi = mean(bmi),
        min_bmi = min(bmi),
        max_bmi = max(bmi),
          count = n()) %>%
  collect() %>%
  print()

kiosks %>% 
group_by(age_group) %>% 
tally() %>%
ungroup() %>%  
collect() %>% 
mutate(p = n/sum(n))

kiosks %>% 
  group_by(ethnicity) %>% 
  tally() %>%
  ungroup() %>%  
  collect() %>% 
  mutate(p = n/sum(n))

kiosks %>% 
group_by(ethnicity_updated) %>% 
tally() %>%
ungroup() %>%  
collect() %>% 
mutate(p = n/sum(n))

kiosks %>% 
group_by(gender) %>% 
tally() %>%
ungroup() %>%  
collect() %>% 
mutate(p = n/sum(n))



prop_diabetes_self = kiosks %>%
    group_by(diagnosed_dm) %>%
    summarize(count_diabetes = n(),
              .groups = "drop") %>%
    collect() %>% 
  mutate(prop_diabetes = count_diabetes/sum(count_diabetes))

prop_diabetes_self


prop_htn_self = kiosks %>%
  group_by(diagnosed_htn) %>%
  summarize(count_htn = n(),
            .groups = "drop") %>%
  collect() %>% 
  mutate(prop_htn = count_htn/sum(count_htn))

prop_htn_self
