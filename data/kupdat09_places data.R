rm(list=ls());gc();source(".Rprofile")


places = read_csv(paste0(path_kiosk_user_patterns_folder,"/working/raw/PLACES/PLACES__Local_Data_for_Better_Health__County_Data_2024_release_20250801.csv")) %>% 
  dplyr::filter(MeasureId == "OBESITY",Data_Value_Type == "Crude prevalence") %>% 
  dplyr::select(StateAbbr,StateDesc,LocationID, LocationName,Data_Value,TotalPop18plus, Low_Confidence_Limit, High_Confidence_Limit) %>%
  rename(County = LocationName,
         fips = LocationID,
         obesity = Data_Value,
         obesity_lci = Low_Confidence_Limit, 
         obesity_uci = High_Confidence_Limit)

write_csv(places,"data/kupdat09_places county estimates.csv")
