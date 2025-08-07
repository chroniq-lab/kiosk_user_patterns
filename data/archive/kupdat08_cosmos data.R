rm(list=ls());gc();source(".Rprofile")

read_csv("C:/code/external/kinetic-t2d/data/ktdat06_cosmos county estimates.csv") %>% 
  dplyr::select(Start,Stop, fips, County_StateAbb, County, StateAbb, Obesity, Overweight, Ncount) %>%
  write_csv(paste0(path_kiosk_user_patterns_repo,"/data/kupdat08_cosmos county estimates.csv"))


state_stratified = read_csv("C:/code/external/kinetic-t2d/data/ktdat06_cosmos state stratified estimates.csv") %>% 
    dplyr::filter(!is.na(state_fips))  %>% 
    dplyr::filter(!strata_new %in% c("0-4","5-18"))
    
state_stratified %>% 
    bind_rows(.,
        {.} %>% 
    group_by(Start,Stop, strata_new, Stratification) %>% 
    summarize(obesity = sum(obesity*NcountBMI,na.rm=TRUE)/sum(NcountBMI,na.rm=TRUE),
    
                overweight = sum(overweight*NcountBMI,na.rm=TRUE)/sum(NcountBMI,na.rm=TRUE),
                NcountBMI = sum(NcountBMI,na.rm=TRUE)) %>%
    mutate(state_fips = NA_character_,
    State = "United States of America")
    ) %>% 
    write_csv(paste0(path_kiosk_user_patterns_repo,"/data/kupdat08_cosmos state stratified estimates.csv"))

