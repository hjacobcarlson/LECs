library(mapview)
library(sf)
library(leafpop)
library(tidyverse)
library(patchwork)

# Functions ####
source("MappingSourceCode.R")


# Read in Data ####
source("DataCleaning.R")


#Making the dataframes for population with tracts and geometry using Source Code #### 
tract70shp <- population_data(census_2010_shp, tract_70)
tract80shp <- population_data(census_2010_shp, tract_80)
tract90shp <- population_data(census_2010_shp, tract_90)
tract2000shp <- population_data(census_2010_shp, tract_2000)
tract2010shp <- population_data(census_2010_shp, tract_2010)
tract2020shp <- population_data(census_2010_shp, tract_2020)




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

