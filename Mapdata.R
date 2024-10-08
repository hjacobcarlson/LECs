library(mapview)
library(sf)
library(leafpop)
library(dplyr)
library(ggplot2)

#location of coops
coops_maps <- st_intersection(census_2020_shp, coops_shp)  
boroughs_outline <- st_read("data/Borough Boundaries/geo_export_aa61a510-02ac-4d56-920f-237be36487ea.shp")
boroughs_outline <- st_transform(boroughs_outline, crs = st_crs("+proj=longlat +datum=WGS84"))
coops_ggplot <- st_transform(coops_maps, crs = st_crs("+proj=longlat +datum=WGS84"))

mapview(coops_maps, 
        legend = TRUE, 
        layer.name = "coops",
        popup = popupTable(coops_maps, zcol = c("Name"))) +
  mapview(boroughs_outline, 
           legend = FALSE,
          color = "black", 
                lwd = 3) + 
  mapview(census_2020_shp, 
          col.regions = color_layer1,
          legend = FALSE,
          color = "white", 
          lwd = 1) 
color_layer1 <- "red"
color_layer2 <- "blue" 
  

#trying to create a source code with ggplot
coops_ggplot <- left_join(coops_ggplot, coops) 
coops_ggplot <- left_join(coops_ggplot, coop_tract_intersect) 
coops_ggplot <-coops_ggplot %>% st_as_sf(coops_ggplot, coords = c("Longitude", "Latitude"), crs = 4326) %>% select(geometry, Name, Longitude, Latitude, TRTID10)  # EPSG:4326 is for WGS84, a common lat/lon CRS
coops_ggplot <- st_transform(coops_ggplot, crs = st_crs(boroughs_outline)) 


p <- ggplot() + 
  # Plot the census_2020_shp layer with color and line settings
  geom_sf(data = census_2020_shp, 
          fill = color_layer1,  # fill color from variable
          color = "white",      # border color
          size = 1) +           # line width
  
  # Plot boroughs_outline with black border
  geom_sf(data = boroughs_outline, 
          color = "black", 
          size = 3, 
          fill = NA)    +      # No fill color for the outline
  
  # Plot coops_shp, assuming Name is an attribute in the dataset
  geom_sf(data = coops_ggplot, 
          aes(fill = "pop" )) +    # Color the points by "Name"
  
  # Add theme and customize appearance
  theme_minimal() +
  labs(title = "Coops Map with Boroughs and Census Data")
p



