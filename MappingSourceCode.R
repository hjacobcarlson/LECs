plot_population_map <- function(census_data, population_data, tract_data, title = "Population by Region") {
  
  ggplot() + 
    # First layer: Plot the census tract shapefile with borders only (no fill)
    geom_sf(data = census_data, fill = NA, color = "black", size = 0.3) +
    
    # Second layer: Plot the population data
    geom_sf(data = population_data, aes(fill = pop)) +  
    
    # Use a viridis color scale for the population data, setting limits
    scale_fill_viridis_c(option = "C", limits = c(0, 20000), 
                         direction = -1,
                         name = "Population") +  
    
    # Minimalistic theme for the plot
    theme_minimal() +
    
    # Add labels for the title and legend
    labs(title = title) +
    
    # Third layer: Add tract boundaries (if needed)
    geom_sf(data = tract_data, color = "white", size = 0.5) +  # White borders
    
    # Apply minimal theme again
    theme_minimal()
}

plot_blk_map <- function(census_data, population_data, tract_data, title = "Black Race By Region") {
  
  ggplot() + 
    # First layer: Plot the census tract shapefile with borders only (no fill)
    geom_sf(data = census_data, fill = NA, color = "black", size = 0.3) +
    
    # Second layer: Plot the population data
    geom_sf(data = population_data, aes(fill = pblk)) +  
    
    # Use a viridis color scale for the population data, setting limits
    scale_fill_viridis_c(option = "C", limits = c(0, 100), 
                         direction = -1,
                         name = "Percent Black") +  
    
    # Minimalistic theme for the plot
    theme_minimal() +
    
    # Add labels for the title and legend
    labs(title = title) +
    
    # Third layer: Add tract boundaries (if needed)
    geom_sf(data = tract_data, color = "white", size = 0.5) +  # White borders
    
    # Apply minimal theme again
    theme_minimal()
}



population_data <- function(census_data, population_data) {
  left_join(census_data, population_data)
}
 


library(mapview)

plot_pop_map <- function(data, popdata) { 
  # Create the map for coops
  coops_map <- mapview(data, 
                       legend = TRUE, 
                       layer.name = "Coops",
                       popup = popupTable(data, zcol = c("Name")))
  
  # Create the map for tract data
  tract_map <- mapview(popdata, 
                       legend = TRUE, 
                       zcol = "pop", 
                       layer.name = "Population",
                       popup = popupTable(popdata, zcol = c("pop", "TRTID10")),
                       zlim = c(0, 20000),  # Set limits for the color scale
                       legend.title = "Population")  # Optional: Set legend title
  
  # Combine both maps into one view
  combined_map <- coops_map + tract_map
  
  return(combined_map)  # Return the combined map
}


plot_blk_mapview <- function(data, racedata) { 
  # Create the map for coops
  coops_map <- mapview(data, 
                       legend = TRUE, 
                       layer.name = "Coops",
                       popup = popupTable(data, zcol = c("Name")))
  
  # Create the map for tract data
  tract_map <- mapview(racedata, 
                       legend = TRUE, 
                       zcol = "pop", 
                       layer.name = "Percent by Black",
                       popup = popupTable(racedata, zcol = c("pblk", "TRTID10")),
                       zlim = c(0, 100),  # Set limits for the color scale
                       legend.title = "Percent by Black")  # Optional: Set legend title
  
  # Combine both maps into one view
  combined_map <- coops_map + tract_map
  
  return(combined_map)  # Return the combined map
}
