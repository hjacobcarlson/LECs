
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(sf)
# creating new dataframes with the two categories of unmatched coops and matched coops ####
matched_coops <- coops_rent %>%
  filter(!is.na(Neighborhood)) 

unmatched_coops <- coops_rent %>% 
  filter(is.na(Neighborhood)) 

#cleaning coops data to match with new data  #### 
coops_2020 <- coops7020 %>% filter(year == 2020)


coops_2020$borough <- str_extract(coops_2020$ADDRESS, paste(boroughs, collapse = "|")) 

coops_2020$street_address <- str_replace(coops_2020$ADDRESS, paste(boroughs, collapse = "|"), "") 
coops_2020$street_address <- str_trim(coops_2020$street_address) # Remove any extra spaces 

coops_2020 <- coops_2020 %>% 
  mutate(
    zip_code = str_extract(street_address, "\\d{5}"),  # Correct escape for digits
    street_address = str_remove(street_address, "NY\\s*\\d{5}"),  # Remove NY and zip (escaped correctly)
    street_address = str_trim(str_remove(street_address, "\\d{5}$")),  # Just in case ZIP is still stuck at the end
    street_address = street_address %>%
      str_replace_all(",", "") %>%
      str_replace_all("The ", "") %>%
      str_replace_all("New York", "") %>%
      str_replace_all("NY", "")
  )

coops_2020$street_address <- trimws(coops_2020$street_address)
coops_2020$street_address <- toupper(coops_2020$street_address) 

coops_2020$street_address <- str_replace_all(coops_2020$street_address, "TIFFA", "TIFFANY") 
coops_2020$street_address <- str_replace_all(coops_2020$street_address, "WEST WEST", "WEST") 
coops_2020$street_address<- str_replace_all(coops_2020$street_address, "\\bE\\b", "EAST")
coops_2020$street_address<- str_replace_all(coops_2020$street_address, " ST ", " STREET ") 
coops_2020$street_address<- str_replace_all(coops_2020$street_address, " STREET NICHOLAS ", " ST NICHOLAS ") 
coops_2020$street_address<- str_replace_all(coops_2020$street_address, "4250 KATONAH AVENUE", "4260 KATONAH AVENUE") 

# joining matched coops and unmatched coops seperately to our coop data from 2020 #### 
matched_2020 <- left_join(matched_coops, coops_2020, by = "street_address" )
unmatched_2020 <- left_join(unmatched_coops,coops_2020, by = "street_address")

# looking for duplicates #### 
look <- unmatched_2020 %>%
    group_by(street_address) %>%
   summarize(n = n())

#matched map coding to clean for maps ####

maps_2020 <- coops_maps %>% select(NAME,geometry)
matched_maps <- left_join(matched_2020, maps_2020, by = "NAME")

matched_maps <- matched_maps %>%
  distinct(street_address, .keep_all = TRUE)

matched_maps <- st_as_sf(matched_maps)

points <- matched_maps %>% select(geometry)

census_shape_matched <- left_join(census_2010_shp,matched_2020)


# unmatched map coding to clean for maps ####
unmatched_maps <- left_join(unmatched_2020, maps_2020, by = "NAME")

unmatched_maps <- unmatched_maps %>%
  distinct(street_address, .keep_all = TRUE)

unmatched_maps <- st_as_sf(unmatched_maps)

points_unmatched <- unmatched_maps %>% select(geometry)

census_shape_unmatched <- left_join(census_2010_shp,unmatched_2020)

#general location ####

ggplot() +
  geom_sf(data = census_shape_matched, fill = "gray90", color = "white", size = 0.2) +  # base NYC map
  geom_sf(data = points, color = "red", size = 2, alpha = 0.7) +             # your points
  theme_minimal() +
  labs(title = "Map of Matched Coops with Points",
       caption = "Red dots = your data")

ggplot() +
  geom_sf(data = census_shape_unmatched, fill = "gray90", color = "white", size = 0.2) +  # base NYC map
  geom_sf(data = points_unmatched, color = "red", size = 2, alpha = 0.7) +             # your points
  theme_minimal() +
  labs(title = "Map of Unmatched Coops with Points",
       caption = "Red dots = your data")
# maps for pblk ####

ggplot() +
  # Base map: polygons filled by your external variable
  geom_sf(data = census_shape_matched, aes(fill = pblk), color = "white", size = 0.2) +
  
  # Points: plotted on top of the map
  geom_sf(data = points, size = 2, alpha = 0.025, color = "white") +
  
  # Styling
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  scale_color_manual(values = c("blue", "red")) +  # customize as needed
  theme_minimal() +
  labs(title = "Percent Black in Matched Coops",
       fill = "Percent Black",
       color = "Point Group")


ggplot() +
  # Base map: polygons filled by your external variable
  geom_sf(data = census_shape_unmatched, aes(fill = pblk), color = "white", size = 0.2) +
  
  # Points: plotted on top of the map
  geom_sf(data = points_unmatched, size = 2, alpha = 0.025, color = "white") +
  
  # Styling
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  scale_color_manual(values = c("blue", "red")) +  # customize as needed
  theme_minimal() +
  labs(title = "Percent Black in Unmatched Coops",
       fill = "Percent Black",
       color = "Point Group")


#wht maps ####
ggplot() +
  # Base map: polygons filled by your external variable
  geom_sf(data = census_shape_matched, aes(fill = pwht), color = "white", size = 0.2) +
  
  # Points: plotted on top of the map
  geom_sf(data = points, size = 2, alpha = 0.025, color = "white") +
  
  # Styling
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  scale_color_manual(values = c("blue", "red")) +  # customize as needed
  theme_minimal() +
  labs(title = "Percent White in Matched Coops",
       fill = "Percent White",
       color = "Point Group")

ggplot() +
  # Base map: polygons filled by your external variable
  geom_sf(data = census_shape_unmatched, aes(fill = pwht), color = "white", size = 0.2) +
  
  # Points: plotted on top of the map
  geom_sf(data = points_unmatched, size = 2, alpha = 0.025, color = "white") +
  
  # Styling
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  scale_color_manual(values = c("blue", "red")) +  # customize as needed
  theme_minimal() +
  labs(title = "Percent White in Unmatched Coops",
       fill = "Percent White",
       color = "Point Group")

#unemployed maps ####
ggplot() +
  # Base map: polygons filled by your external variable
  geom_sf(data = census_shape_matched, aes(fill = punemp), color = "white", size = 0.2) +
  
  # Points: plotted on top of the map
  geom_sf(data = points, size = 2, alpha = 0.025, color = "white") +
  
  # Styling
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  scale_color_manual(values = c("blue", "red")) +  # customize as needed
  theme_minimal() +
  labs(title = "Percent Unemployed in Matched Coops",
       fill = "Percent Unemployed",
       color = "Point Group")

ggplot() +
  # Base map: polygons filled by your external variable
  geom_sf(data = census_shape_unmatched, aes(fill = punemp), color = "white", size = 0.2) +
  
  # Points: plotted on top of the map
  geom_sf(data = points_unmatched, size = 2, alpha = 0.025, color = "white") +
  
  # Styling
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  scale_color_manual(values = c("blue", "red")) +  # customize as needed
  theme_minimal() +
  labs(title = "Percent Unemployed in Unmatched Coops",
       fill = "Percent Unemployed",
       color = "Point Group") 






# NEW TYPE of MAPS #### 
coops_map <- coops_maps %>% select(geometry,ID)
coops_map_dem <- left_join(coops,coops_map, by = "ID") # dataframe that contains demographics and geometry of the POINTS 
census <- left_join(census_2010_shp,coops, by = "TRTID10") # dataframe that contains demographics and geometry of the census tracts 

census <- st_as_sf(census)
coops_map_dem <- st_as_sf(coops_map_dem)

# general 

ggplot() +
  geom_sf(data = census, fill = "gray90", color = "white", size = 0.2) +  # base NYC map
  geom_sf(data = coops_map_dem, aes(color = matched), size = 2, alpha = 0.5) +             # your points
  theme_minimal() + 
  labs(title = "Map of Coops")


# unemployed 
ggplot() +
  # Base map: polygons filled by your external variable
  geom_sf(data = census, aes(fill = punemp), color = "white", size = 0.2) +
  
  # Points: color based on `matched` column
  geom_sf(data = coops_map_dem, aes(color = matched), size = 2, alpha = 0.1) +
  
  # Styling
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") +
scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +  # Customize colors
  theme_minimal() +
  labs(title = "Percent Unemployed in Coops",
       fill = "Percent Unemployed",
       color = "Matched Status")

# pblk
ggplot() +
  # Base map: polygons filled by your external variable
  geom_sf(data = census, aes(fill = pblk), color = "white", size = 0.2) +
  
  # Points: color based on `matched` column
  geom_sf(data = coops_map_dem, aes(color = matched), size = 2, alpha = 0.1) +
  
  # Styling
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +  # Customize colors
  theme_minimal() +
  labs(title = "Percent Black in Coops",
       fill = "Percent Black",
       color = "Matched Status")

# pwht with circles and triangles 
ggplot() +
  # Base map: NY shapefile with light-to-dark blue fill
  geom_sf(data = census, aes(fill = pwht), color = "white", size = 0.2) +
  
  # Points: shape based on matched
  geom_sf(data = coops_map_dem, aes(shape = matched), size = 2, color = "red", alpha = 0.1) +
  
  # Fill scale for background
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") +
  
  # Shape mapping: circle (16) for TRUE, triangle (17) for FALSE
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 17)) +
  
  theme_minimal() +
  labs(
    title = "Percent White in Coops",
    fill = "Percent White",
    shape = "Matched Status"
  )
