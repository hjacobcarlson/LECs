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


# Create maps by year, variable ####

## Total Population ####

ggplot1970pop <- plot_map(tract70shp, pop, title = "Population")
ggplot1980pop <- plot_map(tract80shp, pop, title = "Population")
ggplot1990pop <- plot_map(tract90shp, pop, title = "Population")
ggplot2000pop <- plot_map(tract2000shp, pop, title = "Population")
ggplot2010pop <- plot_map(tract2010shp, pop, title = "Population")
ggplot2020pop <- plot_map(tract2020shp, pop, title = "Population")


## Percent Black ####

ggplot1970pblk <- plot_map(tract70shp, pblk, title = "Pct. Black")
ggplot1980pblk <- plot_map(tract80shp, pblk, title = "Pct. Black")
ggplot1990pblk <- plot_map(tract90shp, pblk, title = "Pct. Black")
ggplot2000pblk <- plot_map(tract2000shp, pblk, title = "Pct. Black")
ggplot2010pblk <- plot_map(tract2010shp, pblk, title = "Pct. Black")
ggplot2020pblk <- plot_map(tract2020shp, pblk, title = "Pct. Black")


## Percent white ####

ggplot1970pwht <- plot_map(tract70shp, pwht, title = "Pct. White")
ggplot1980pwht <- plot_map(tract80shp, pwht, title = "Pct. White")
ggplot1990pwht <- plot_map(tract90shp, pwht, title = "Pct. White")
ggplot2000pwht <- plot_map(tract2000shp, pwht, title = "Pct. White")
ggplot2010pwht <- plot_map(tract2010shp, pwht, title = "Pct. White")
ggplot2020pwht <- plot_map(tract2020shp, pwht, title = "Pct. White")


## Percent Unemployed

ggplot1970punemp <- plot_map(tract70shp, punemp, title = "Pct. Unemployed")
ggplot1980punemp <- plot_map(tract80shp, punemp, title = "Pct. Unemployed")
ggplot1990punemp <- plot_map(tract90shp, punemp, title = "Pct. Unemployed")
ggplot2000punemp <- plot_map(tract2000shp, punemp, title = "Pct. Unemployed")
ggplot2010punemp <- plot_map(tract2010shp, punemp, title = "Pct. Unemployed")
ggplot2020punemp <- plot_map(tract2020shp, punemp, title = "Pct. Unemployed")


## Percent college graduates ####




# NOTE: Below here, kept (temporarily?) from previous version
# Using source code for 
library(gridExtra)
grid.arrange(ggplot1970blk, ggplot1980blk, ncol = 2)



#Combining the pop maps #### 
pop_map_combined <- ggplot1970pop + ggplot2020pop 

pop_map <- ggplot1970pop + ggplot2020pop
