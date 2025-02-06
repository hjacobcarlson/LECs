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


# TEMPORARY! Only until we fix the geocoding, or get data directly from UHAB
# This makes the coops_shp only include those that were successfully intersected w/ the NYC tract shapefile
coops_shp <- coops_maps


# Tract Data ####

source("DataCleaning.R")

# Combine tract data from LTDB with co-op tracts 
coops7020 <- left_join(coops_maps, tract, by = "TRTID10", relationship = "many-to-many") %>% 
  as_tibble() %>%
  select(-geometry)



#Making the dataframes for population with tracts and geometry using Source Code #### 
tract70shp <- population_data(census_2010_shp, tract_70)
tract80shp <- population_data(census_2010_shp, tract_80)
tract90shp <- population_data(census_2010_shp, tract_90)
tract2000shp <- population_data(census_2010_shp, tract_2000)
tract2010shp <- population_data(census_2010_shp, tract_2010)
tract2020shp <- population_data(census_2010_shp, tract_2020)



# Using source code plot_map to create pop map on mapview #### 
mapview1970pop <- plot_pop_map(coops_maps, tract70shp)
mapview1980pop <- plot_pop_map(coops_maps, tract80shp)
mapview1990pop <- plot_pop_map(coops_maps, tract90shp)
mapview2020pop <- plot_pop_map(coops_maps, tract2020shp)

# Using source code plot_map to create blk map on mapview #### 

mapview1970blk <- plot_blk_mapview(coops_maps, tract70shp)
mapview1980blk <- plot_blk_mapview(coops_maps, tract80shp)
mapview1990blk <- plot_blk_mapview(coops_maps, tract90shp)
mapview2000blk <- plot_blk_mapview(coops_maps, tract2000shp)
mapview2010blk <- plot_blk_mapview(coops_maps, tract2010shp)
mapview2020blk <- plot_blk_mapview(coops_maps, tract2020shp)


# Using source code plot_population_map to create tract pop maps on ggplot2 #### 

ggplot1970pop <- plot_map( tract70shp, coops_maps)
ggplot1980pop <- plot_population_map(census_data = census_2010_shp, 
                                        population_data = tract80shp,
                                        tract_data = coops_maps)  
ggplot1990pop <- plot_population_map(census_data = census_2010_shp, 
                                     population_data = tract90shp,
                                     tract_data = coops_maps)  
ggplot2000pop <- plot_population_map(census_data = census_2010_shp, 
                                     population_data = tract2000shp, 
                                     tract_data = coops_maps) 
ggplot2010pop <- plot_population_map(census_data = census_2010_shp, 
                                     population_data = tract2010shp,
                                     tract_data = coops_maps)  
ggplot2020pop <- plot_population_map(census_data = census_2010_shp, 
                                     population_data = tract2020shp,
                                     tract_data = coops_maps)  

#Combining the pop maps #### 
pop_map_combined <- ggplot1970pop + ggplot2020pop 

pop_map <- ggplot1970pop + ggplot2020pop



# Using source code for black percent on ggplot ####
ggplot1970blk <- plot_race_map(census_2010_shp, tract70shp, coops_maps, "pblk")
ggplot1980blk <- plot_race_map(census_2010_shp, tract80shp, coops_maps, "pblk")
ggplot1990blk <- plot_race_map(census_2010_shp, tract90shp, coops_maps, "pblk")
ggplot2000blk <- plot_race_map(census_2010_shp, tract2000shp, coops_maps, "pblk")
ggplot2010blk <- plot_race_map(census_2010_shp, tract2010shp, coops_maps, "pblk")
ggplot2020blk <- plot_race_map(census_2010_shp, tract2020shp, coops_maps, "pblk")


# Using source code for white percent on ggplot ####

ggplot1970wht <- plot_race_map(census_2010_shp, tract70shp, coops_maps, "pwht")
ggplot1980wht <- plot_race_map(census_2010_shp, tract80shp, coops_maps, "pwht")
ggplot1990wht <- plot_race_map(census_2010_shp, tract90shp, coops_maps, "pwht")
ggplot2000wht <- plot_race_map(census_2010_shp, tract2000shp, coops_maps, "pwht")
ggplot2010wht <- plot_race_map(census_2010_shp, tract2010shp, coops_maps, "pwht")
ggplot2020wht <- plot_race_map(census_2010_shp, tract2020shp, coops_maps, "pwht")


# Using source code for unemployment percent ####
ggplot1970unemp <- plot_unemp(census_2010_shp, tract70shp, coops_maps, "punemp")
ggplot1980unemp <- plot_unemp(census_2010_shp, tract80shp, coops_maps, "punemp")
ggplot1990unemp <- plot_unemp(census_2010_shp, tract90shp, coops_maps, "punemp")
ggplot2000unemp <- plot_unemp(census_2010_shp, tract2000shp, coops_maps, "punemp")
ggplot2010unemp <- plot_unemp(census_2010_shp, tract2010shp, coops_maps, "punemp")
ggplot2020unemp <- plot_unemp(census_2010_shp, tract2020shp, coops_maps, "punemp")


# Using source code for college graduates percent ####

# Using source code for 
library(gridExtra)
grid.arrange(ggplot1970blk, ggplot1980blk, ncol = 2)

