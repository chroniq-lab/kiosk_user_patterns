plot_map_estimates <- function(df,level="state"){
  
  # Read in shapefiles 
  if(level %in% c("county","brfss-county")){
    # Added for brfss-county
    boundaries <- readRDS(paste0(path_kiosk_user_patterns_folder,"/working/shapefiles/county_boundaries_2022.rds"))
    state_boundaries <- readRDS(paste0(path_kiosk_user_patterns_folder,"/working/shapefiles/state_boundaries_2022.rds"))
    
    merge_vars = c("GEOID" = "FIPS")
  }else{
    boundaries <- readRDS(paste0(path_kiosk_user_patterns_folder,"/working/shapefiles/state_boundaries_2022.rds"))
    merge_vars = c("GEOID" = "state_fips")
  }
  
  
  
  # Merge with dataset
  if(level %in% c("county","state")){
    size       <- nrow(boundaries)
    boundaries <- boundaries |>
      left_join(df, by = merge_vars)
  }else{
    # Added for brfss-county
    boundaries <- boundaries |>
      left_join(df, by = merge_vars)
  }
  
  # Create ggplot object 
 
    plt <- ggplot() +
      geom_sf(data=boundaries,col=boundary_col,aes(fill=mean))  +
      # scale_fill_distiller(palette = palette, direction = -1, limits = c(min, max), breaks = c(25,35,45,55)) +
      scale_fill_distiller(palette = palette, direction = 1, limits = c(min, max), breaks = breaks) +
      theme_map +
      labs(fill = paste0("Prevalence (%)")) 
  
  
  
  
  if(level %in% c("county","brfss-county")){
    plt <- plt + geom_sf(data=state_boundaries,col="black",fill=NA)
  }
  
    return(plt)
  
}