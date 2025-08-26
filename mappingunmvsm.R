
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(sf)


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

# creating column matched
coopsr <- coops_rent %>% select(Neighborhood,street_address)
coops_2020 <- left_join(coops_2020,coopsr) %>%   
  mutate(matched = !is.na(Neighborhood))


#  TYPE of MAPS #### 
coops_map <- coops_maps %>% select(geometry,ID)
coops_map_dem <- left_join(coops_2020,coops_map, by = "ID") # dataframe that contains demographics and geometry of the POINTS 
census <- left_join(census_2010_shp, coops_2020, by = "TRTID10") # dataframe that contains demographics and geometry of the census tracts 

census <- st_as_sf(census)
coops_map_dem <- st_as_sf(coops_map_dem)

# general location of coops matched and unmatched

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

# pwht with circles and triangles instead 
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

