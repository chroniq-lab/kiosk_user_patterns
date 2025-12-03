rm(list=ls());gc();source(".Rprofile")

ps = read_csv("https://raw.githubusercontent.com/jvargh7/spatial_kiosks/main/data/reference/poststratification_table.csv") %>%
  rename(ethnicity_updated = ethnicity) %>% 
  mutate(urban = case_when(urban == "urban" ~ 1,
                           urban == "rural" ~ 0,
                           TRUE ~ NA_real_)) %>% 
  mutate(across(one_of(c("state","FIPS","age_group","ethnicity_updated","gender","urban","state_region")),~as.factor(.)))


source("functions/direct_poststratification.R")

# MODELS ----------
obesity_model_20242025 = readRDS(paste0(path_kinetic_hyperc3_folder,"/working/output/kthyc303/glmer_20242025_obesity_corrected.RDS"))
overweight_model_20242025 = readRDS(paste0(path_kinetic_hyperc3_folder,"/working/output/kthyc303/glmer_20242025_overweight_corrected.RDS"))
obesity_model_20222023 = readRDS(paste0(path_kinetic_hyperc3_folder,"/working/output/kthyc303/glmer_20222023_obesity_corrected.RDS"))
overweight_model_20222023 = readRDS(paste0(path_kinetic_hyperc3_folder,"/working/output/kthyc303/glmer_20222023_overweight_corrected.RDS"))


if(!dir.exists(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10"))){
  dir.create(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10"))
}

# COUNTY --------------
obesity_county_estimates_20242025 = direct_poststratification(model = obesity_model_20242025)
write_csv(obesity_county_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_county_obesity estimates.csv"))


overweight_county_estimates_20242025 = direct_poststratification(model = overweight_model_20242025)
write_csv(overweight_county_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_county_overweight estimates.csv"))


obesity_county_estimates_20222023 = direct_poststratification(model = obesity_model_20222023)
write_csv(obesity_county_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_county_obesity estimates.csv"))


overweight_county_estimates_20222023 = direct_poststratification(model = overweight_model_20222023)
write_csv(overweight_county_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_county_overweight estimates.csv"))





file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_county_obesity estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20242025_county_obesity estimates.csv"),overwrite = TRUE)

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_county_overweight estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20242025_county_overweight estimates.csv"),overwrite = TRUE)


file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_county_obesity estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20222023_county_obesity estimates.csv"),overwrite = TRUE)

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_county_overweight estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20222023_county_overweight estimates.csv"),overwrite = TRUE)



# STATE --------------
obesity_state_estimates_20242025 = direct_poststratification(model = obesity_model_20242025,group.vars="state_code")
write_csv(obesity_state_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_state_obesity estimates.csv"))


overweight_state_estimates_20242025 = direct_poststratification(model = overweight_model_20242025,group.vars="state_code")
write_csv(overweight_state_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_state_overweight estimates.csv"))


obesity_state_estimates_20222023 = direct_poststratification(model = obesity_model_20222023,group.vars="state_code")
write_csv(obesity_state_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_state_obesity estimates.csv"))


overweight_state_estimates_20222023 = direct_poststratification(model = overweight_model_20222023,group.vars="state_code")
write_csv(overweight_state_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_state_overweight estimates.csv"))




if(!dir.exists(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10"))){
  dir.create(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10"))
}

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_state_obesity estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20242025_state_obesity estimates.csv"),overwrite = TRUE)

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_state_overweight estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20242025_state_overweight estimates.csv"),overwrite = TRUE)


file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_state_obesity estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20222023_state_obesity estimates.csv"),overwrite = TRUE)

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_state_overweight estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20222023_state_overweight estimates.csv"),overwrite = TRUE)


# SOCIO-DEMOGRAPHIC --------------

# for(v in c("age_group","ethnicity_updated","gender","urban","state_region")){
for(v in c("age_group","ethnicity_updated","gender","urban","state_region")){
  
  obesity_stratified_estimates_20242025 = direct_poststratification(model = obesity_model_20242025,group.vars=v)
  write_csv(obesity_stratified_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_stratified_",v,"_obesity estimates.csv"))
  
  
  overweight_stratified_estimates_20242025 = direct_poststratification(model = overweight_model_20242025,group.vars=v)
  write_csv(overweight_stratified_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_stratified_",v,"_overweight estimates.csv"))
  
  
  obesity_stratified_estimates_20222023 = direct_poststratification(model = obesity_model_20222023,group.vars=v)
  write_csv(obesity_stratified_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_stratified_",v,"_obesity estimates.csv"))
  
  
  overweight_stratified_estimates_20222023 = direct_poststratification(model = overweight_model_20222023,group.vars=v)
  write_csv(overweight_stratified_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_stratified_",v,"_overweight estimates.csv"))
  
  
  
  
  if(!dir.exists(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10"))){
    dir.create(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10"))
  }
  
  file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_stratified_",v,"_obesity estimates.csv"),
            paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20242025_stratified_",v,"_obesity estimates.csv"),overwrite = TRUE)
  
  file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_stratified_",v,"_overweight estimates.csv"),
            paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20242025_stratified_",v,"_overweight estimates.csv"),overwrite = TRUE)
  
  
  file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_stratified_",v,"_obesity estimates.csv"),
            paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20222023_stratified_",v,"_obesity estimates.csv"),overwrite = TRUE)
  
  file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_stratified_",v,"_overweight estimates.csv"),
            paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20222023_stratified_",v,"_overweight estimates.csv"),overwrite = TRUE)
  
  
  
}


# NATIONAL --------------
obesity_national_estimates_20242025 = direct_poststratification(model = obesity_model_20242025,group.vars="")
write_csv(obesity_national_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_national_obesity estimates.csv"))


overweight_national_estimates_20242025 = direct_poststratification(model = overweight_model_20242025,group.vars="")
write_csv(overweight_national_estimates_20242025,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_national_overweight estimates.csv"))


obesity_national_estimates_20222023 = direct_poststratification(model = obesity_model_20222023,group.vars="national_code")
write_csv(obesity_national_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_national_obesity estimates.csv"))


overweight_national_estimates_20222023 = direct_poststratification(model = overweight_model_20222023,group.vars="")
write_csv(overweight_national_estimates_20222023,paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_national_overweight estimates.csv"))



if(!dir.exists(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10"))){
  dir.create(paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10"))
}

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_national_obesity estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20242025_national_obesity estimates.csv"),overwrite = TRUE)

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20242025_national_overweight estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20242025_national_overweight estimates.csv"),overwrite = TRUE)


file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_national_obesity estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20222023_national_obesity estimates.csv"),overwrite = TRUE)

file.copy(paste0(path_kinetic_t2d_folder,"/working/ktana10/glmer_20222023_national_overweight estimates.csv"),
          paste0(path_kiosk_user_patterns_folder,"/working/model/ktana10/glmer_20222023_national_overweight estimates.csv"),overwrite = TRUE)

