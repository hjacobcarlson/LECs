library(mapview)
library(sf)
library(leafpop)
coops_maps <- cross_join(coops, census_2020_shp)
coops_maps <- st_transform(coops_maps, crs = st_crs("+proj=longlat +datum=WGS84"))
map_70_black <- st_as_sf(map_70_black, coords = c("Longitude", "Latitude"), crs = 4326)
boroughs_outline <- st_read("data/Borough Boundaries/geo_export_aa61a510-02ac-4d56-920f-237be36487ea.shp")
boroughs_outline <- st_transform(boroughs_outline, crs = st_crs("+proj=longlat +datum=WGS84"))

mapview(coops_maps, 
        legend = TRUE, 
        layer.name = "coops",
        popup = popupTable(coops_shp, zcol = c("Name"))) +
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
  
coops_maps1970_blk <- left_join(coops_maps, tract_70 %>% select(, age), by = "id")


coops_maps <- st_intersection(census_2020_shp, coops_shp)  

map_70_black <- left_join(map_70_black, coops, by = "Name")
 