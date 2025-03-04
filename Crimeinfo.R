library(tidyverse)
library(sf)

crime_data <- read.csv("data/crime_original.csv") %>% # read in data
  rename(Precinct = PCT)

colnames(crime_data) <- gsub("^X", "", colnames(crime_data)) #remove "X" in front of years


crime_data <- crime_data %>% 
  pivot_longer(
    cols = matches("^20|^1990"), # Select year columns
    names_to = "Year", # New column for year
    values_to = "Count") %>% arrange(Year, CRIME)  


# Precincts shapefile ####
nypd_shp <- st_read("data/shp/nypp_25a/nypp.shp") %>%
  mutate(area = st_area(geometry))


# 2010 Census tracts shapefile ####
census_2010_shp <- st_read("data/nyu_2451_34505/nyu_2451_34505.shp") %>%
  rename(TRTID10 = tractid) %>%
  mutate(TRTID10 = as.numeric(TRTID10)) %>%
  select(TRTID10)

# Tract location of precincts #### 
precinct_tracts <- st_intersection(census_2010_shp, nypd_shp) %>%
  mutate(portion_area = st_area(geometry)) %>%
  as_tibble() %>%
  select(TRTID10, Precinct, portion_area)


nypd_weights <- nypd_shp %>%
  left_join(precinct_tracts, by = "Precinct") %>%
  mutate(pctPrecinct = as.numeric(portion_area/area)) %>%
  as_tibble() %>%
  select(Precinct, TRTID10, pctPrecinct)
  
crime_data_weighted <- crime_data %>%
  left_join(nypd_weights, by = "Precinct") %>%
  mutate(w_count = Count * pctPrecinct)

crime_data_tract <- crime_data_weighted %>%
  group_by(TRTID10, Year, CRIME) %>%
  summarize(w_count = sum(w_count, na.rm = TRUE)) %>%
  mutate(CRIME = str_trim(CRIME)) %>%
  pivot_wider(names_from = CRIME, values_from = w_count) %>%
  rename(burglary = BURGLARY,
         assault = `FELONY ASSAULT`,
         larceny = `GRAND LARCENY`,
         vehicle = `GRAND LARCENY OF MOTOR VEHICLE`,
         murder = `MURDER & NON NEGL. MANSLAUGHTER`,
         rape = RAPE,
         robbery = ROBBERY,
         totfel = `TOTAL SEVEN MAJOR FELONY OFFENSES`) %>%
  filter(Year %in% c("1990", "2000", "2010", "2020"))

write_csv(crime_data_tract, "data/crime_clean.csv")

