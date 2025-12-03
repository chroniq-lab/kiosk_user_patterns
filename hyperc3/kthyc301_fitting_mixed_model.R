rm(list=ls());gc();source("kthyc3_config.R")

# Add to beginning of your R script
library(parallel)
options(mc.cores = parallel::detectCores())

spatial_covariates = " + UE_rate + no_HS_rate + median_income + HI_coverage + state_region"


# YEAR_RANGE = list(c(2022,2023), c(2024,2025))
YEAR_RANGE = list(c(2022,2023))
TYPE = c("obesity","overweight")

for (y_r in YEAR_RANGE){
  
  dt = readRDS(paste0(path_kinetic_t2d_hyperc3_folder,"/working/input/ktana07 dataset.RDS")) %>%
    dplyr::filter(year %in% y_r) %>% 
    distinct(session_id_mask,pseudo_member_id,.keep_all=TRUE) 

  
  for(outcome_var in TYPE){
    
    print(paste0(paste0(y_r,collapse=""),
                 "-",
                 outcome_var,
                 "-",
                 Sys.time()
                 ))
    
    
    
    
    formula = as.formula(paste0(outcome_var," ~ (1 |state/FIPS) + age_group + ethnicity_updated*gender + urban",spatial_covariates))
    
    fit.glmer <- glmer(
      formula,
      family = binomial(link = "logit"),
      data = dt,
      # nAGQ = 1L,
      # verbose = 1,
      control = glmerControl(optimizer = "nloptwrap")
    )
    
    path_results = paste0(path_kinetic_t2d_hyperc3_folder,"/working/output/kthyc301")
    if(!dir.exists(path_results)){
      dir.create(path_results)
    }
    
    filename <- paste0("glmer_",paste0(y_r,collapse=""), "_", outcome_var, ".RDS")
    saveRDS(fit.glmer, file = paste0(path_results,"/",filename))
    
  }
}


