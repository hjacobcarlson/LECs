library(mapview)
library(sf)
library(leafpop)
library(tidyverse)
library(patchwork)

# Functions ####
source("MappingSourceCode.R")


# Read in geodata ####

# 2010 Census tracts shapefile
census_2010_shp <- st_read("data/nyu_2451_34505/nyu_2451_34505.shp") %>%
  rename(TRTID10 = tractid) %>%
  mutate(TRTID10 = as.numeric(TRTID10)) %>%
  select(TRTID10)


# UHAB Coop Locations (web scrape)
coops <- read_csv("data/Clean UHAB Data - Sheet1 (1).csv")

# Convert to sf object
coops_shp <- st_as_sf(coops, coords = c("Longitude", "Latitude"), crs = 4326)  # EPSG:4326 is the WGS 84 coordinate system

coops_shp <- st_transform(coops_shp, st_crs(census_2010_shp))


# Tract location of coops #### 
coops_maps <- st_intersection(census_2010_shp, coops_shp)  


# Tract Data ####

source("DataCleaning.R")

# Combine tract data from LTDB with co-op tracts 
coops7020 <- left_join(coops_maps, tract, by = "TRTID10", relationship = "many-to-many") %>% 
  as_tibble() %>%
  select(-geometry)



#Making the dataframes for population with tracts and geometry using Source Code #### 
tract70data <- population_data(census_2010_shp, tract_70)
tract80data <- population_data(census_2010_shp, tract_80)
tract90data <- population_data(census_2010_shp, tract_90)
tract2000data <- population_data(census_2010_shp, tract_2000)
tract2010data <- population_data(census_2010_shp, tract_2010)
tract2020data <- population_data(census_2010_shp, tract_2020)



# Using source code plot_map to create pop map on mapview #### 
mapview1970pop <- plot_pop_map(coops_maps, tract70data)
mapview1980pop <- plot_pop_map(coops_maps, tract80data)
mapview1990pop <- plot_pop_map(coops_maps, tract90data)
mapview2020pop <- plot_pop_map(coops_maps, tract2020data)

# Using source code plot_map to create blk map on mapview #### 

mapview1970blk <- plot_blk_mapview(coops_maps, tract70data)
mapview1980blk <- plot_blk_mapview(coops_maps, tract80data)
mapview1990blk <- plot_blk_mapview(coops_maps, tract90data)
mapview2000blk <- plot_blk_mapview(coops_maps, tract2000data)
mapview2010blk <- plot_blk_mapview(coops_maps, tract2010data)
mapview2020blk <- plot_blk_mapview(coops_maps, tract2020data)
# Using source code plot_population_map to create tract pop maps on ggplot2 #### 

ggplot1970pop <- plot_population_map( tract70data, coops_maps)
ggplot1980pop <- plot_population_map(census_data = census_2010_shp, 
                                        population_data = tract80data,
                                        tract_data = coops_maps)  
ggplot1990pop <- plot_population_map(census_data = census_2010_shp, 
                                     population_data = tract90data,
                                     tract_data = coops_maps)  
ggplot2000pop <- plot_population_map(census_data = census_2010_shp, 
                                     population_data = tract2000data, 
                                     tract_data = coops_maps) 
ggplot2010pop <- plot_population_map(census_data = census_2010_shp, 
                                     population_data = tract2010data,
                                     tract_data = coops_maps)  
ggplot2020pop <- plot_population_map(census_data = census_2010_shp, 
                                     population_data = tract2020data,
                                     tract_data = coops_maps)  

#Combining the pop maps #### 
pop_map_combined <- ggplot1970pop + ggplot2020pop 

pop_map <- ggplot1970pop + ggplot2020pop



# Using source code for black percent on ggplot ####
ggplot1970blk <- plot_race_map(census_2010_shp, tract70data, coops_maps, "pblk")
ggplot1980blk <- plot_race_map(census_2010_shp, tract80data, coops_maps, "pblk")
ggplot1990blk <- plot_race_map(census_2010_shp, tract90data, coops_maps, "pblk")
ggplot2000blk <- plot_race_map(census_2010_shp, tract2000data, coops_maps, "pblk")
ggplot2010blk <- plot_race_map(census_2010_shp, tract2010data, coops_maps, "pblk")
ggplot2020blk <- plot_race_map(census_2010_shp, tract2020data, coops_maps, "pblk")


# Using source code for white percent on ggplot ####

ggplot1970wht <- plot_race_map(census_2010_shp, tract70data, coops_maps, "pwht")
ggplot1980wht <- plot_race_map(census_2010_shp, tract80data, coops_maps, "pwht")
ggplot1990wht <- plot_race_map(census_2010_shp, tract90data, coops_maps, "pwht")
ggplot2000wht <- plot_race_map(census_2010_shp, tract2000data, coops_maps, "pwht")
ggplot2010wht <- plot_race_map(census_2010_shp, tract2010data, coops_maps, "pwht")
ggplot2020wht <- plot_race_map(census_2010_shp, tract2020data, coops_maps, "pwht")


# Using source code for unemployment percent ####
ggplot1970unemp <- plot_unemp(census_2010_shp, tract70data, coops_maps, "punemp")
ggplot1980unemp <- plot_unemp(census_2010_shp, tract80data, coops_maps, "punemp")
ggplot1990unemp <- plot_unemp(census_2010_shp, tract90data, coops_maps, "punemp")
ggplot2000unemp <- plot_unemp(census_2010_shp, tract2000data, coops_maps, "punemp")
ggplot2010unemp <- plot_unemp(census_2010_shp, tract2010data, coops_maps, "punemp")
ggplot2020unemp <- plot_unemp(census_2010_shp, tract2020data, coops_maps, "punemp")


# Using source code for college graduates percent ####

# Using source code for 
library(gridExtra)
grid.arrange(ggplot1970blk, ggplot1980blk, ncol = 2)

