library(mapview)
library(sf)
library(leafpop)
library(dplyr)
library(ggplot2)
library(patchwork)
library(htmltools)
#location of coops #### 
coops_maps <- st_intersection(census_2020_shp, coops_shp)  

#Making the dataframes for population with tracts and geometry using Source Code #### 
tract70data <- population_data(census_2020_shp, tract_70)
tract80data <- population_data(census_2020_shp, tract_80)
tract90data <- population_data(census_2020_shp, tract_90)


# Using source code plot_map to create tract80 pop map on mapview #### 
mapview1970pop <- plot_map(coops_maps, tract70data)
mapview1980pop <- plot_map(coops_maps, tract80data)
mapview1990pop <- plot_map(coops_maps, tract90data)

# Using source code plot_population_map to create tract pop maps on ggplot2 #### 

ggplot1970pop <- plot_population_map(census_data = census_2020_shp, 
                                      population_data = tract70bdata, 
                                      tract_data = coops_maps) 
ggplot1980pop <- plot_population_map(census_data = census_2020_shp, 
                                        population_data = tract80data,
                                        tract_data = coops_maps)  
ggplot1990pop <- plot_population_map(census_data = census_2020_shp, 
                                     population_data = tract90data,
                                     tract_data = coops_maps)  


#Combining the pop maps #### 
pop_map_combined <- ggplot1970pop + ggplot1980pop + ggplot1990pop

