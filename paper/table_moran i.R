
rm(list=ls());gc();source(".Rprofile")


lmoran_places_ob = readRDS(paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_places_ob.rds"))
lmoran_cosmos_ow= readRDS(paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_cosmos_ow.rds"))
lmoran_pursuant_ow= readRDS(paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_pursuant_ow.rds"))
lmoran_cosmos_ob= readRDS(paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_cosmos_ob.rds"))
lmoran_pursuant_ob= readRDS(paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_pursuant_ob.rds"))

df = lmoran_places_ob %>% 
  dplyr::select(index, fips) %>% 
  mutate(places_ob_quad_median = attr(lmoran_places_ob$Ii,"quad")$median,
         pursuant_ow_quad_median = attr(lmoran_pursuant_ow$Ii,"quad")$median,
         pursuant_ob_quad_median = attr(lmoran_pursuant_ob$Ii,"quad")$median) %>% 
  
  
  left_join(
    
    lmoran_cosmos_ow %>% 
      dplyr::select(index,fips) %>% 
      mutate(cosmos_ow_quad_median = attr(lmoran_cosmos_ow$Ii,"quad")$median),
    
    by = c("index","fips")
    
  ) %>% 
  
  
  left_join(
    
    lmoran_cosmos_ob %>% 
      dplyr::select(index,fips) %>% 
      mutate(cosmos_ob_quad_median = attr(lmoran_cosmos_ob$Ii,"quad")$median),
    
    by = c("index","fips")
    
  )



with(df,table(places_ob_quad_median,cosmos_ob_quad_median)) %>% 
  as_tibble() %>% 
  pivot_wider(names_from=cosmos_ob_quad_median,values_from=n) %>% 
  write_csv("paper/table_moral i_cosmos.csv")
with(df,table(places_ob_quad_median,pursuant_ob_quad_median)) %>% 
  as_tibble() %>% 
  pivot_wider(names_from=pursuant_ob_quad_median,values_from=n) %>% 
  write_csv("paper/table_moral i_pursuant.csv")




