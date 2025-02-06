

# ggplot2 maps ####

plot_map <- function(geodata, mapvar, addcoops = TRUE, 
                     title = "") {
  
  mapvar <- enquo(mapvar)
  
  p <- ggplot() +  
    
    # Second layer: Plot the population data
    geom_sf(data = geodata, aes(fill = !!mapvar)) +  
    
    # Use a viridis color scale for the population data, setting limits
    scale_fill_viridis_c(option = "C", limits = c(0, 10000),
                         direction = -1,
                         name = title) +
    
    # Minimalistic theme for the plot
    theme_minimal() +
    
    # Add labels for the title and legend
    labs(title = title) +
    
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



# Population map ####

plot_population_map <- function(population_data, coops, title = "Population by Region") {
  
  ggplot() +  
  
    # Second layer: Plot the population data
    geom_sf(data = population_data, aes(fill = pop)) +  
    
    # Use a viridis color scale for the population data, setting limits
    scale_fill_viridis_c(option = "C", limits = c(0, 10000), 
                         direction = -1,
                         name = "Population") +  
    
    # Minimalistic theme for the plot
    theme_minimal() +
    
    # Add labels for the title and legend
    labs(title = title) +
    geom_sf(data = coops, color = "black", size = 0.5, na.rm = TRUE) + 
    

    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

# population data function ####
population_data <- function(census_data, population_data) {
  left_join(census_data, population_data)
}

library(dplyr)
library(mapview)

# mapview pop ####
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
                       zlim = c(0, 10000),  # Set limits for the color scale
                       legend.title = "Population")  # Optional: Set legend title
  
  # Combine both maps into one view
  combined_map <- coops_map + tract_map
  
  return(combined_map)  # Return the combined map
}



#  race #### 

plot_race_map <- function(census_data, population_data, tract_data, race_var, title = "Race By Region") {
  
  # Ensure race_var is numeric (if it's not already)
  population_data[[race_var]] <- as.numeric(as.character(population_data[[race_var]]))
  
  # Check if there are any NA values in the race_var column
  if (any(is.na(population_data[[race_var]]))) {
    message("Warning: NA values present in race_var. These will be ignored in the plot.")
  }
  
  # Check if the population data is aligned with the census data
  if (!all(population_data$TRTID10 %in% census_data$TRTID10)) {
    stop("Mismatch in GEOID between population_data and census_data.")
  }
  
  ggplot() + 
    # Plot the census tract shapefile with borders only (no fill)
    geom_sf(data = census_data, fill = NA, color = "black", size = 0.3) +
    
    # Plot the population data using the specified race variable, ignoring NA values
    geom_sf(data = population_data, aes(fill = !!sym(race_var)), na.rm = TRUE) +  
    
    # Use a viridis color scale for the population data, setting limits based on data range
    scale_fill_viridis_c(option = "C", 
                         direction = -1,
                         name = paste(race_var),
                         na.value = "gray") +  # Set NA values to gray (or any color you prefer)
    
    # Minimalistic theme for the plot
    theme_minimal() +
    
    # Add labels for the title and legend
    labs(title = title) +
    
    # Add tract boundaries (if needed)
    geom_sf(data = tract_data, color = "white", size = 0.5, na.rm = TRUE) +  # White borders
    
    # Apply minimal theme again
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}


# unemployed ####
plot_unemp <- function(census_data, population_data, tract_data, unemp_var, title = "Unemployment Rates") {
  
  # Ensure race_var is numeric (if it's not already)
  population_data[[unemp_var]] <- as.numeric(as.character(population_data[[unemp_var]]))
  
  # Check if there are any NA values in the race_var column
  if (any(is.na(population_data[[unemp_var]]))) {
    message("Warning: NA values present in unemp_var. These will be ignored in the plot.")
  }
  
  # Check if the population data is aligned with the census data
  if (!all(population_data$TRTID10 %in% census_data$TRTID10)) {
    stop("Mismatch in GEOID between population_data and census_data.")
  }
  
  ggplot() + 
    # Plot the census tract shapefile with borders only (no fill)
    geom_sf(data = census_data, fill = NA, color = "black", size = 0.01) +
    
    # Plot the population data using the specified race variable, ignoring NA values
    geom_sf(data = population_data, aes(fill = !!sym(unemp_var)), na.rm = TRUE) +  
    
    # Use a viridis color scale for the population data, setting limits based on data range
    scale_fill_viridis_c(option = "C", limits = c(0, 0.50),
                         direction = -1,
                         name = paste("Unemployment"),
                         na.value = "gray") +  # Set NA values to gray (or any color you prefer)
    
    # Minimalistic theme for the plot
    theme_minimal() +
    
    # Add labels for the title and legend
    labs(title = title) +
    
    # Add tract boundaries (if needed)
    geom_sf(data = tract_data, color = "white", size = 0.5, na.rm = TRUE) +  # White borders
    
    
    # Apply minimal theme again
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

# College map ####
plot_col <- function(census_data, population_data, tract_data, col_var, title = "College Rates") {
  
  # Ensure race_var is numeric (if it's not already)
  population_data[[unemp_var]] <- as.numeric(as.character(population_data[[unemp_var]]))
  
  # Check if there are any NA values in the race_var column
  if (any(is.na(population_data[[unemp_var]]))) {
    message("Warning: NA values present in unemp_var. These will be ignored in the plot.")
  }
  
  # Check if the population data is aligned with the census data
  if (!all(population_data$TRTID10 %in% census_data$TRTID10)) {
    stop("Mismatch in GEOID between population_data and census_data.")
  }
  
  ggplot() + 
    # Plot the census tract shapefile with borders only (no fill)
    geom_sf(data = census_data, fill = NA, color = "black", size = 0.3) +
    
    # Plot the population data using the specified race variable, ignoring NA values
    geom_sf(data = population_data, aes(fill = !!sym(unemp_var)), na.rm = TRUE) +  
    
    # Use a viridis color scale for the population data, setting limits based on data range
    scale_fill_viridis_c(option = "C", limits = c(0, 1),
                         direction = -1,
                         name = paste("Unemployment"),
                         na.value = "gray") +  # Set NA values to gray (or any color you prefer)
    
    # Minimalistic theme for the plot
    theme_minimal() +
    
    # Add labels for the title and legend
    labs(title = title) +
    
    # Add tract boundaries (if needed)
    geom_sf(data = tract_data, color = "white", size = 0.5, na.rm = TRUE) +  # White borders
    
    # Apply minimal theme again
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}
