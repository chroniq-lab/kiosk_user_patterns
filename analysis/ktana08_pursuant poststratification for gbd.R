rm(list=ls());gc();source(".Rprofile")

ps = read_csv("data/ktdat08_gbd poststratification table.csv") %>% 
  rename(ethnicity_updated = ethnicity,
         age_group_gbd = age_group) %>% 
  mutate(across(one_of(c("state","FIPS","age_group_gbd","ethnicity_updated","gender","urban","state_region")),~as.factor(.))) %>% 
  dplyr::filter(!is.na(state)) %>% 
  mutate(age_group_gbd_consolidated = case_when(age_group_gbd == "18-24" ~ "18-24",
                                               TRUE ~ "25plus"))


source("functions/direct_poststratification.R")


# MODELS --------------
obesity_model_20242025 = readRDS(paste0(path_kinetic_hyperc3_folder,"/working/output/kthyc302/glmer_gbdcompare_20242025_obesity.RDS"))
overweight_model_20242025 = readRDS(paste0(path_kinetic_hyperc3_folder,"/working/output/kthyc302/glmer_gbdcompare_20242025_overweight.RDS"))
obesity_model_20222023 = readRDS(paste0(path_kinetic_hyperc3_folder,"/working/output/kthyc302/glmer_gbdcompare_20222023_obesity.RDS"))
overweight_model_20222023 = readRDS(paste0(path_kinetic_hyperc3_folder,"/working/output/kthyc302/glmer_gbdcompare_20222023_overweight.RDS"))


# # COUNTY -------------------
# 
# obesity_county_estimates_20242025 = direct_poststratification(model = obesity_model_20242025,group.vars = c("FIPS","age_group_gbd_consolidated"))
# write_csv(obesity_county_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20242025_county_obesity estimates.csv"))
# 
# 
# overweight_county_estimates_20242025 = direct_poststratification(model = overweight_model_20242025,group.vars = c("FIPS","age_group_gbd_consolidated"))
# write_csv(overweight_county_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20242025_county_overweight estimates.csv"))
# 
# 
# obesity_county_estimates_20222023 = direct_poststratification(model = obesity_model_20222023,group.vars = c("FIPS","age_group_gbd_consolidated"))
# write_csv(obesity_county_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20222023_county_obesity estimates.csv"))
# 
# 
# overweight_county_estimates_20222023 = direct_poststratification(model = overweight_model_20222023,group.vars = c("FIPS","age_group_gbd_consolidated"))
# write_csv(overweight_county_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20222023_county_overweight estimates.csv"))
# 
# 
# 
# 
# if(!dir.exists(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08"))){
#   dir.create(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08"))
# }
# 
# file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20242025_county_obesity estimates.csv"),
#           paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08/glmer_gbdcompare_20242025_county_obesity estimates.csv"),overwrite = TRUE)
# 
# file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20242025_county_overweight estimates.csv"),
#           paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08/glmer_gbdcompare_20242025_county_overweight estimates.csv"),overwrite = TRUE)
# 
# 
# file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20222023_county_obesity estimates.csv"),
#           paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08/glmer_gbdcompare_20222023_county_obesity estimates.csv"),overwrite = TRUE)
# 
# file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20222023_county_overweight estimates.csv"),
#           paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08/glmer_gbdcompare_20222023_county_overweight estimates.csv"),overwrite = TRUE)
# 

# STATE -------------------

obesity_state_estimates_20242025 = direct_poststratification(model = obesity_model_20242025,group.vars = c("state_code","age_group_gbd_consolidated"))
write_csv(obesity_state_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20242025_state_obesity estimates.csv"))


overweight_state_estimates_20242025 = direct_poststratification(model = overweight_model_20242025,group.vars = c("state_code","age_group_gbd_consolidated"))
write_csv(overweight_state_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20242025_state_overweight estimates.csv"))


obesity_state_estimates_20222023 = direct_poststratification(model = obesity_model_20222023,group.vars = c("state_code","age_group_gbd_consolidated"))
write_csv(obesity_state_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20222023_state_obesity estimates.csv"))


overweight_state_estimates_20222023 = direct_poststratification(model = overweight_model_20222023,group.vars = c("state_code","age_group_gbd_consolidated"))
write_csv(overweight_state_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20222023_state_overweight estimates.csv"))




if(!dir.exists(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08"))){
  dir.create(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08"))
}

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20242025_state_obesity estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08/glmer_gbdcompare_20242025_state_obesity estimates.csv"),overwrite = TRUE)

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20242025_state_overweight estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08/glmer_gbdcompare_20242025_state_overweight estimates.csv"),overwrite = TRUE)


file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20222023_state_obesity estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08/glmer_gbdcompare_20222023_state_obesity estimates.csv"),overwrite = TRUE)

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana08/glmer_gbdcompare_20222023_state_overweight estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana08/glmer_gbdcompare_20222023_state_overweight estimates.csv"),overwrite = TRUE)

