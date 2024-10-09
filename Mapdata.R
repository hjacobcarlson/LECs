library(mapview)
library(sf)
library(leafpop)
library(dplyr)
library(ggplot2)
library(patchwork)
#location of coops #### 
coops_maps <- st_intersection(census_2020_shp, coops_shp)  

#Making the dataframes for population with tracts and geometry using Source Code #### 
tract70data <- population_data(census_2020_shp, tract_70)
tract80data <- population_data(census_2020_shp, tract_80)
tract90data <- population_data(census_2020_shp, tract_90)
tract2000data <- population_data(census_2020_shp, tract_2000)
tract2010data <- population_data(census_2020_shp, tract_2010)
tract2020data <- population_data(census_2020_shp, tract_2020)


# Using source code plot_map to create pop map on mapview #### 
mapview1970pop <- plot_pop_map(coops_maps, tract70data)
mapview1980pop <- plot_pop_map(coops_maps, tract80data)
mapview1990pop <- plot_pop_map(coops_maps, tract90data)


# Using source code plot_map to create blk map on mapview #### 

mapview1970blk <- plot_blk_mapview(coops_maps, tract70data)
mapview1980blk <- plot_blk_map(coops_maps, tract80data)
mapview1990blk <- plot_blk_map(coops_maps, tract90data)

# Using source code plot_population_map to create tract pop maps on ggplot2 #### 

ggplot1970pop <- plot_population_map(census_data = census_2020_shp, 
                                      population_data = tract70data, 
                                      tract_data = coops_maps) 
ggplot1980pop <- plot_population_map(census_data = census_2020_shp, 
                                        population_data = tract80data,
                                        tract_data = coops_maps)  
ggplot1990pop <- plot_population_map(census_data = census_2020_shp, 
                                     population_data = tract90data,
                                     tract_data = coops_maps)  
ggplot2000pop <- plot_population_map(census_data = census_2020_shp, 
                                     population_data = tract2000data, 
                                     tract_data = coops_maps) 
ggplot2010pop <- plot_population_map(census_data = census_2020_shp, 
                                     population_data = tract2010data,
                                     tract_data = coops_maps)  
ggplot2020pop <- plot_population_map(census_data = census_2020_shp, 
                                     population_data = tract2020data,
                                     tract_data = coops_maps)  

 
#Combining the pop maps #### 
pop_map_combined <- ggplot1970pop + ggplot1980pop 

pop_map <- ggplot1970pop + ggplot2020pop


# Using source code for black percent on ggplot ####
ggplot1970blk <- plot_blk_map(census_2020_shp,tract70data, coops_maps)
ggplot1980blk <- plot_blk_map(census_2020_shp,tract80data,coops_maps)
ggplot1990blk <- plot_blk_map(census_2020_shp,tract90data,coops_maps)
ggplot2000blk <- plot_blk_map(census_2020_shp,tract2000data,coops_maps)
ggplot2010blk <- plot_blk_map(census_2020_shp,tract2010data,coops_maps)
ggplot2020blk <- plot_blk_map(census_2020_shp,tract2020data,coops_maps)



blk_map_combined <- ggplot1970blk + ggplot1990blk
