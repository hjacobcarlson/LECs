

# ggplot2 maps ####

plot_map <- function(geodata, mapvar, addcoops = TRUE, 
                     title = "") {
  
  mapvar <- enquo(mapvar)
  
  p <- ggplot() +  
    
    # Second layer: Plot the population data
    geom_sf(data = geodata, aes(fill = !!mapvar)) +  
    
    # Use a viridis color scale for the population data, setting limits
    scale_fill_viridis_c(option = "C", #limits = c(0, 10000), # Let limits be free
                         direction = -1,
                         name = title) +
    
    # Minimalistic theme for the plot
    theme_minimal() +
    
    # # Add labels for the title and legend
    # Removing this for now, since the legend indicator tells us the variable
    # labs(title = title) +
    
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  if(addcoops == TRUE) {
    p <- p +
      geom_sf(data = coops_shp, color = "black", size = 0.5, na.rm = TRUE)
    
    p
    
  } else {
    p
    
  }
}



