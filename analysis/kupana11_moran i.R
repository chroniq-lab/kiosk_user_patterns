
rm(list=ls());gc();source(".Rprofile")
library(sf)
source("analysis/kupana_county prevalence.R")

boundaries <- readRDS(paste0(path_kiosk_user_patterns_folder,"/working/shapefiles/county_boundaries_2022.rds"))|>
  left_join(df, by = c("GEOID"="fips")) 
  

library(spdep)

nb = poly2nb(boundaries,queen=TRUE)

nb[1]

nbw = nb2listw(nb,style ="W", zero.policy=TRUE)

moran(boundaries$places_ob, nbw, length(nb), Szero(nbw))[1]
moran(boundaries$cosmos_ow, nbw, length(nb), Szero(nbw),  NAOK = TRUE)[1]
moran(boundaries$pursuant_ow, nbw, length(nb), Szero(nbw),  NAOK = TRUE)[1]
moran(boundaries$cosmos_ob, nbw, length(nb), Szero(nbw),  NAOK = TRUE)[1]
moran(boundaries$pursuant_ob, nbw, length(nb), Szero(nbw),  NAOK = TRUE)[1]


moran.test(boundaries$places_ob, nbw)
moran.test(boundaries$cosmos_ow, nbw,na.action = na.omit)
moran.test(boundaries$pursuant_ow, nbw,na.action = na.omit)
moran.test(boundaries$cosmos_ob, nbw,na.action = na.omit)
moran.test(boundaries$pursuant_ob, nbw,na.action = na.omit)



# First remove NA values from the dataset before computing Moran's I
# Create a vector of valid indices for cosmos_ob (non-NA values)
valid_indices_cosmos_ob <- which(!is.na(boundaries$cosmos_ob))
valid_indices_cosmos_ow <- which(!is.na(boundaries$cosmos_ow))

# Calculate local Moran's I for each variable
lmoran_places_ob = localmoran(boundaries$places_ob, nbw, alternative = "two.sided", zero.policy = TRUE) %>% 
      as_tibble() %>% 
      mutate(index = 1:nrow(boundaries),
      fips = boundaries$GEOID)
lmoran_pursuant_ow = localmoran(boundaries$pursuant_ow, nbw, alternative = "two.sided", zero.policy = TRUE) %>% 
      as_tibble() %>% 
      mutate(index = 1:nrow(boundaries),
      fips = boundaries$GEOID)


lmoran_cosmos_ow = localmoran(boundaries$cosmos_ow[valid_indices_cosmos_ow], 
                             nb2listw(subset.nb(nb, !is.na(boundaries$cosmos_ow)), style="W", zero.policy=TRUE),
                             alternative = "two.sided", 
                             zero.policy = TRUE) %>% 
      as_tibble() %>% 
      mutate(index = valid_indices_cosmos_ow,
      fips = boundaries$GEOID[valid_indices_cosmos_ow])

# For cosmos_ob, subset both the variable and the weights list
lmoran_cosmos_ob = localmoran(boundaries$cosmos_ob[valid_indices_cosmos_ob], 
                             nb2listw(subset.nb(nb, !is.na(boundaries$cosmos_ob)), style="W", zero.policy=TRUE),
                             alternative = "two.sided", 
                             zero.policy = TRUE) %>% 
      as_tibble() %>% 
      mutate(index = valid_indices_cosmos_ob,
      fips = boundaries$GEOID[valid_indices_cosmos_ob])

lmoran_pursuant_ob = localmoran(boundaries$pursuant_ob, nbw, alternative = "two.sided", zero.policy = TRUE) %>% 
      as_tibble() %>% 
      mutate(index = 1:nrow(boundaries),
      fips = boundaries$GEOID)

if(!dir.exists(paste0(path_kiosk_user_patterns_folder,"/working/moran_i"))){
  dir.create(paste0(path_kiosk_user_patterns_folder,"/working/moran_i"))
}

saveRDS(lmoran_places_ob, paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_places_ob.rds"))
saveRDS(lmoran_cosmos_ow, paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_cosmos_ow.rds"))
saveRDS(lmoran_pursuant_ow, paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_pursuant_ow.rds"))
saveRDS(lmoran_cosmos_ob, paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_cosmos_ob.rds"))
saveRDS(lmoran_pursuant_ob, paste0(path_kiosk_user_patterns_folder,"/working/moran_i/lmoran_pursuant_ob.rds"))
