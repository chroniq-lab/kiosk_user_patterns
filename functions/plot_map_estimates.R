library(sf)

plot_map_estimates <- function(df,level="state",plot_var = "mean",fips_var=NA,min=0,max=100,breaks=seq(0,100,by=20),
                               plot_var_label = "Prevalence (%)", palette = "YlGnBu"){
  
  # Read in shapefiles 
  if(level %in% c("county","brfss-county")){
    
    fips_var = if_else(is.na(fips_var),"FIPS",fips_var)
    
    # Added for brfss-county
    boundaries <- readRDS(paste0(path_kiosk_user_patterns_folder,"/working/shapefiles/county_boundaries_2022.rds"))
    state_boundaries <- readRDS(paste0(path_kiosk_user_patterns_folder,"/working/shapefiles/state_boundaries_2022.rds"))
    
    merge_vars = c("GEOID" = fips_var)
  }else{
    
    fips_var = if_else(is.na(fips_var),"state_fips",fips_var)
    
    
    boundaries <- readRDS(paste0(path_kiosk_user_patterns_folder,"/working/shapefiles/state_boundaries_2022.rds"))
    merge_vars = c("GEOID" = fips_var)
    
    
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
  boundary_col = "grey80"
    plt <- ggplot() +
      geom_sf(data=boundaries,col=boundary_col,aes_string(fill=plot_var))  +
      # scale_fill_distiller(palette = palette, direction = -1, limits = c(min, max), breaks = c(25,35,45,55)) +
      scale_fill_distiller(palette = palette, direction = 1, limits = c(min, max), breaks = breaks) +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom", 
            axis.text = element_blank(),
            legend.text = element_text(size = 14),
            panel.grid.major = element_line(colour = "transparent")) +
      labs(fill = plot_var_label) + 
      guides(colour = guide_legend(override.aes = list(size = 1.2))) 
  
  
  
  
  if(level %in% c("county","brfss-county")){
    plt <- plt + geom_sf(data=state_boundaries,col="black",fill=NA)
  }
  
    return(plt)
  
}