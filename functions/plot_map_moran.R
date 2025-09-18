library(sf)
library(RColorBrewer)
plot_map_moran <- function(df,level="state",plot_var = "mean",fips_var=NA,palette = "YlGnBu",
                                           plot_var_label = ".", boundary_col = "grey80"
){
  
  
  categories = 4
  palette_categories = brewer.pal(categories, palette)
  names(palette_categories) = c("Low-Low","Low-High","High-Low","High-High")
  
  print(palette_categories)
  
  df = df %>%
    rename(var = !!plot_var) 
  
  print(unique(df$var_category))
  
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
  plt <- ggplot() +
    # https://stackoverflow.com/questions/33765710/force-ggplot-legend-to-show-all-categories-when-no-values-are-present
    geom_sf(data=boundaries,col=boundary_col,aes_string(fill=paste0("var")),show.legend=TRUE)  +
    # scale_fill_discrete("",type = palette_categories,drop = FALSE) +
    scale_fill_manual("",values = palette_categories,drop = FALSE) +
    
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