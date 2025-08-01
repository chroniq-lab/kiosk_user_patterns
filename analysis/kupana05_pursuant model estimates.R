rm(list=ls());gc();source(".Rprofile")


strata_files = c("national","county","state","age_group","ethnicity_updated","gender","urban","state_region")
names(strata_files) = c("national","FIPS","state_code","agegroup","ethnicity_updated","gender","urban","state_region")

pursuant_files = list.files(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana07"),full.names = TRUE) %>% 
  .[str_detect(.,paste0("(",paste0(strata_files,collapse="|"),")"))]

stratified_df = map_dfr(pursuant_files,
  function(f){
    # Extract which stratification variable is used in this file
    strat_file = str_extract(f, paste0("_(", paste0(strata_files, collapse="|"), ")_(overweight|obesity)")) %>% str_replace("(overweight|obesity)","") %>% str_replace_all("(^_|_$)", "")
    strat_var = attr(strata_files[which(strata_files == strat_file)], "names")
    print(strat_file)
    print(strat_var)

    df = read_csv(f) %>% 
     dplyr::select(one_of(strat_var),prevalence, median_prevalence, prevalence_lci,prevalence_uci) %>% 
      mutate(file_name = f) %>% 
      mutate(year = case_when(str_detect(f, "20222023") ~ "2022-2023",
                             str_detect(f, "20242025") ~ "2024-2025",
                             TRUE ~ NA_character_),
             measure = case_when(str_detect(f, "overweight") ~ "Overweight",
                             str_detect(f, "obesity") ~ "Obesity",
                             TRUE ~ NA_character_),
             stratification = strat_var)
    
    # Only rename if the stratification variable exists in the dataframe
    if(strat_var %in% names(df) & strat_var != "national") {
      # Create a named vector for renaming
      df = df %>% 
      dplyr::rename(strata = !!strat_var) %>% 
      mutate(strata = as.character(strata))
    }
    
    return(df)
  }) %>% 
  dplyr::select(stratification, strata, year, measure, 
  prevalence, median_prevalence, prevalence_lci,prevalence_uci,file_name) 


View(stratified_df)

write_csv(stratified_df,"analysis/kupana05_pursuant model estimates.csv")
