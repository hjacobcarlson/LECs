library(tidyverse)
library(sf)
library(quantmod)

# Functions ####
# To adjust HH Incomes for Inflation

# Adjust median incomes to 2015 dollars
getSymbols("CPIAUCSL", src='FRED') #Consumer Price Index for All Urban Consumers: All Items
#[1] "CPIAUCSL"

avg.cpi <- apply.yearly(CPIAUCSL, mean)

cf <- as.numeric(avg.cpi['2020'])/avg.cpi #using 2020 as the base year
cdf <- as.data.frame(cf)
cdf$LastDayOfYear <- rownames(cdf)
cdf$year <- sapply(cdf$LastDayOfYear, FUN=function(x) as.numeric(substr(x, 1, 4)))
cdf <- cdf %>%
  mutate(year = as.character(year))





# Read in geodata ####

# 2010 Census tracts shapefile
census_2010_shp <- st_read("data/nyu_2451_34505/nyu_2451_34505.shp") %>%
  rename(TRTID10 = tractid) %>%
  mutate(TRTID10 = as.numeric(TRTID10)) %>%
  select(TRTID10)


# UHAB Coop Locations (full UHAB data, v1)
coops <- read_csv("data/NY Co-op Data (1).csv") %>%
  filter(!is.na(ID)) %>%
  filter(!is.na(LONG)) %>%
  filter(!Type %in% c("Affordable, Former HUD 213", "Senior", "Student co-op")) # omitting some types of "coops", may change later

# Convert to sf object
coops_shp <- st_as_sf(coops, coords = c("LONG", "LAT"), crs = 4326)  # EPSG:4326 is the WGS 84 coordinate system

coops_shp <- st_transform(coops_shp, st_crs(census_2010_shp))


# Tract location of coops #### 
coops_maps <- st_intersection(census_2010_shp, coops_shp)  


# TEMPORARY! Only until we fix the geocoding, or get data directly from UHAB
# This makes the coops_shp only include those that were successfully intersected w/ the NYC tract shapefile
#coops_shp <- coops_maps


# Count number of coops in each tract
n_coops <- coops_maps %>%
  as_tibble() %>%
  group_by(TRTID10) %>%
  summarize(ncoops = n(),
            ncoopunits = sum(UNITS, na.rm = TRUE))



# Tract Data ####
## 1970 ####
# Import data from LTDB, only NYC

tract_70 <- read_csv("data/ltdb_std_all_fullcount/LTDB_Std_1970_fullcount.csv") %>%
  filter(state == "NY") %>%
  filter(county == "Bronx County" | county == "Queens County" | county == "Kings County" 
         | county == "Richmond County" | county == "New York County") %>%
  mutate(TRTID10 = as.numeric(TRTID10))

LTDB_1970_sample <- read_csv("data/ltdb_std_all_sample/ltdb_std_1970_sample.csv") %>%
  mutate(TRTID10 = as.numeric(TRTID10)) %>%
  select(-county)


# Join sample and population, recode

tract_70 <- left_join(tract_70, LTDB_1970_sample, by = 'TRTID10') %>%
  rename(hinc = HINC70,
    owner = OWN70,
    rent = RENT70,
    ohu = OHU70,
    multi = MULTI70,
    wht = WHITE70,
    blk = BLACK70,
    pop = POP70,
    hs = HS70,
    col = COL70, 
    unemp = UNEMP70, 
    mrent = MRENT70, 
    clf = CLF70,
    str30old = H30OLD70,
    hh10old = H10YRS70,
    fhh = FHH70,
    pov = NPOV70,
    dpov = DPOV70,
    pfb = FB70,
    pfb10 = N10IMM70,
    p18und = A18UND70,
    p60up = A60UP70) %>%
  mutate(powner = owner / ohu,
    prent = rent / ohu,
    pwht = wht / pop,
    pblk = blk / pop, 
    phs = hs / pop,
    pcol = col / pop, 
    punemp = unemp / clf,
    ppov = pov / dpov) %>% 
  mutate(year = "1970") %>% 
  filter(pop > 100) %>%
  select(year, county, TRTID10, hinc, prent, powner, pwht, pblk, pop, punemp, ohu, multi,
         mrent, pcol, phs, str30old, hh10old, fhh, pov, ppov, pfb, pfb10, p18und, p60up) %>%
  mutate(across(-c(year, county), as.numeric)) %>%
  left_join(n_coops, by = "TRTID10") %>%
  mutate(ncoops = if_else(is.na(ncoops), 0, ncoops),
         ncoopunits = if_else(is.na(ncoopunits), 0, ncoopunits))
  


## 1980 ####
# Import data from LTDB, only NYC

tract_80 <- read_csv("data/ltdb_std_all_fullcount/LTDB_Std_1980_fullcount.csv") %>%
  filter(state == "NY") %>%
  filter(county == "Bronx County" | county == "Queens County" | county == "Kings County" 
         | county == "Richmond County" | county == "New York County") %>%
  mutate(TRTID10 = as.numeric(TRTID10))

LTDB_1980_sample <- read_csv("data/ltdb_std_all_sample/ltdb_std_1980_sample.csv") %>% 
  rename(TRTID10 = trtid10) %>%
  mutate(TRTID10 = as.numeric(TRTID10)) %>%
  select(-county)


# Join sample and population, recode

tract_80 <- left_join(tract_80, LTDB_1980_sample, by = 'TRTID10') %>%
  rename(hinc = hinc80,
         owner = OWN80,
         rent = RENT80,
         ohu = OHU80,
         multi = multi80,
         wht = NHWHT80,
         blk = NHBLK80,
         hisp = HISP80,
         pop = POP80, 
         hs = hs80,
         col = col80,
         unemp = unemp80, 
         mrent = MRENT80,
         clf = clf80,
         str30old = h30old80,
         hh10old = h10yrs80,
         fhh = fhh80,
         pov = npov80,
         dpov = dpov80,
         pfb = fb80,
         pfb10 = n10imm80,
         p18und = A18UND80,
         p60up = A60UP80) %>%
  mutate(powner = owner / ohu,
         prent = rent / ohu, 
         pwht = wht / pop,
         pblk = blk / pop, 
         phisp = hisp / pop,
         phs = hs / pop,
         pcol = col / pop, 
         punemp = unemp / clf,
         ppov = pov / dpov) %>% 
  mutate(year = "1980") %>% 
  filter(pop > 100) %>%
  select(year, county, TRTID10, hinc, powner, prent, pwht, pblk, phisp, pop, punemp, ohu, multi,
         mrent, pcol, phs, str30old, hh10old, fhh, pov, ppov, pfb, pfb10, p18und, p60up) %>%
  mutate(across(-c(year, county), as.numeric)) %>%
  left_join(n_coops, by = "TRTID10") %>%
  mutate(ncoops = if_else(is.na(ncoops), 0, ncoops),
         ncoopunits = if_else(is.na(ncoopunits), 0, ncoopunits))


## 1990 ####
# Import data from LTDB, only NYC

tract_90 <- read_csv("data/ltdb_std_all_fullcount/LTDB_Std_1990_fullcount.csv") %>%
  filter(state == "NY") %>%
  filter(county == "Bronx County" | county == "Queens County" | county == "Kings County" 
         | county == "Richmond County" | county == "New York County") %>%
  mutate(TRTID10 = as.numeric(TRTID10))

LTDB_1990_sample <- read_csv("data/ltdb_std_all_sample/ltdb_std_1990_sample.csv") %>%
  mutate(TRTID10 = as.numeric(TRTID10)) %>%
  select(-county)


# Join sample and population, recode

tract_90 <- left_join(tract_90, LTDB_1990_sample, by = 'TRTID10') %>%
  rename(hinc = HINC90,
         owner = OWN90,
         rent = RENT90,
         ohu = OHU90,
         multi = MULTI90,
         wht = NHWHT90,
         blk = NHBLK90,
         hisp = HISP90,
         pop = POP90, 
         hs = HS90,
         col = COL90,
         unemp = UNEMP90,
         mrent = MRENT90,
         clf = CLF90,
         str30old = H30OLD90,
         hh10old = H10YRS90,
         fhh = FHH90,
         pov = NPOV90,
         dpov = DPOV90,
         pfb = FB90,
         pfb10 = N10IMM90,
         p18und = A18UND90,
         p60up = A60UP90) %>%
  mutate(powner = owner / ohu,
         prent = rent / ohu,
         pwht = wht / pop,
         pblk = blk / pop, 
         phisp = hisp / pop,
         phs = hs / pop,
         pcol = col / pop, 
         punemp = unemp / clf,
         ppov = pov / dpov) %>% 
  mutate(year = "1990") %>% 
  filter(pop > 100) %>%
  select(year, county, TRTID10, hinc, powner, prent, pwht, pblk, phisp, pop, punemp, ohu, multi,
         mrent, pcol, phs, str30old, hh10old, fhh, pov, ppov, pfb, pfb10, p18und, p60up) %>%
  mutate(across(-c(year, county), as.numeric)) %>%
  left_join(n_coops, by = "TRTID10") %>%
  mutate(ncoops = if_else(is.na(ncoops), 0, ncoops),
         ncoopunits = if_else(is.na(ncoopunits), 0, ncoopunits))


## 2000 ####
# Import data from LTDB, only NYC

tract_2000 <- read_csv("data/ltdb_std_all_fullcount/LTDB_Std_2000_fullcount.csv") %>%
  filter(state == "NY") %>%
  filter(county == "Bronx County" | county == "Queens County" | county == "Kings County" 
         | county == "Richmond County" | county == "New York County") %>%
  mutate(TRTID10 = as.numeric(TRTID10))

LTDB_2000_sample <- read_csv("data/ltdb_std_all_sample/ltdb_std_2000_sample.csv") %>%
  mutate(TRTID10 = as.numeric(TRTID10)) %>%
  select(-county)


# Join sample and population, recode

tract_2000 <- left_join(tract_2000, LTDB_2000_sample, by = 'TRTID10') %>%
  rename(hinc = HINC00,
         owner = OWN00,
         rent = RENT00, 
         ohu = HU00,
         multi = MULTI00,
         wht = NHWHT00,
         blk = NHBLK00,
         hisp = HISP00,
         pop = POP00,
         hs = HS00,
         col = COL00, 
         unemp = UNEMP00, 
         mrent = MRENT00,
         clf = CLF00,
         str30old = H30OLD00,
         hh10old = H10YRS00,
         fhh = FHH00,
         pov = NPOV00,
         dpov = DPOV00,
         pfb = FB00,
         pfb10 = N10IMM00,
         p18und = A18UND00,
         p60up = A60UP00) %>%
  mutate(powner = owner / ohu,
         prent = rent / ohu,
         pwht = wht / pop,
         pblk = blk / pop, 
         phisp = hisp / pop,
         phs = hs / pop,
         pcol = col / pop, 
         punemp = unemp / clf,
         ppov = pov / dpov) %>% 
  mutate(year = "2000") %>% 
  filter(pop > 100) %>%
  select(year, county, TRTID10, hinc, powner, prent, pwht, pblk, phisp, pop, punemp, ohu, multi,
         mrent, pcol, phs, str30old, hh10old, fhh, pov, ppov, pfb, pfb10, p18und, p60up) %>%
  mutate(across(-c(year, county), as.numeric)) %>%
  left_join(n_coops, by = "TRTID10") %>%
  mutate(ncoops = if_else(is.na(ncoops), 0, ncoops),
         ncoopunits = if_else(is.na(ncoopunits), 0, ncoopunits))


## 2010 ####
# Import data from LTDB, only NYC

tract_2010 <- read_csv("data/ltdb_std_all_fullcount/LTDB_Std_2010_fullcount.csv") %>%
  rename(TRTID10 = tractid) %>%
  filter(state == "NY") %>%
  filter(county == "Bronx County" | county == "Queens County" | county == "Kings County" 
         | county == "Richmond County" | county == "New York County") %>%
  mutate(TRTID10 = as.numeric(TRTID10))

LTDB_2008_2012_sample <- read_csv("data/ltdb_std_all_sample/LTDB_std_200812_Sample.csv") %>%
  rename(TRTID10 = tractid) %>%
  mutate(TRTID10 = as.numeric(TRTID10))


# Join sample and population, recode

tract_2010 <- left_join(tract_2010, LTDB_2008_2012_sample, by = 'TRTID10') %>%
  rename(hinc = hinc12,
         owner = own10,
         rent = rent10,
         ohu = hu10,
         multi = multi12,
         wht = nhwht10,
         blk = nhblk10,
         hisp = hisp10,
         pop = pop10,
         hs = hs12,
         col = col12, 
         unemp = unemp12, 
         mrent = mrent12, 
         clf = clf12,
         str30old = h30old12,
         hh10old = h10yrs12,
         fhh = fhh10,
         pov = npov12,
         dpov = dpov12,
         pfb = pfb12,
         pfb10 = n10imm12,
         p18und = a18und10,
         p60up = a60up10) %>%
  mutate(powner = owner / ohu,
         prent = rent / ohu,
         pwht = wht / pop,
         pblk = blk / pop, 
         phisp = hisp / pop,
         phs = hs / pop,
         pcol = col / pop, 
         punemp = unemp / clf,
         ppov = pov / dpov) %>% 
  mutate(year = "2010") %>% 
  filter(pop > 100) %>%
  select(year, county, TRTID10, hinc, prent, powner, pwht, pblk, phisp, pop, punemp, ohu, multi,
         mrent, pcol, phs, str30old, hh10old, fhh, pov, ppov, pfb, pfb10, p18und, p60up) %>%
  mutate(across(-c(year, county), as.numeric)) %>%
  left_join(n_coops, by = "TRTID10") %>%
  mutate(ncoops = if_else(is.na(ncoops), 0, ncoops),
         ncoopunits = if_else(is.na(ncoopunits), 0, ncoopunits))


## 2020 ####
# Import data from LTDB, only NYC

tract_2020 <- read_csv("data/ltdb_std_all_fullcount/ltdb_std_2020_fullcount.csv") %>%
  rename(TRTID10 = TRTID2010) %>%
  mutate(TRTID10 = as.numeric(TRTID10))

LTDB_2015_2019_sample <- read_csv("data/ltdb_std_all_sample/LTDB_std_201519_Sample.csv") %>% 
  rename(TRTID10 = tractid,
         state = statea,
         county = countya) %>%
  mutate(TRTID10 = as.numeric(TRTID10)) %>%
  filter(state == "New York") %>%
  filter(county == "Bronx County" | county == "Queens County" | county == "Kings County" 
         | county == "Richmond County" | county == "New York County")


# Join sample and population, recode

tract_2020 <- right_join(tract_2020, LTDB_2015_2019_sample, by = "TRTID10") %>%
  rename(hinc = hinc19,
         owner = own19,
         rent = rent19,
         ohu = hu19,
         multi = multi19,
         wht = nhwt20,
         blk = nhblk20,
         hisp = hisp20,
         pop = pop20, 
         hs = hs19,
         col = col19, 
         unemp = unemp19, 
         mrent = mrent19, 
         clf = clf19,
         str30old = h30old19,
         hh10old = h10yrs19,
         fhh = fhh19,
         pov = npov19,
         dpov = dpov19,
         pfb = pfb19,
         pfb10 = n10imm19,
         p18und = a18und19,
         p60up = a60up19) %>%
  mutate(powner = owner / ohu,
         prent = rent / ohu,
         pwht = wht / pop,
         pblk = blk / pop, 
         phisp = hisp / pop,
         phs = hs / pop,
         pcol = col / pop, 
         punemp = unemp / clf,
         ppov = pov / dpov) %>% 
  mutate(year = "2020") %>%
  filter(pop > 100) %>%
  select(year, county, TRTID10, hinc, powner, prent, pwht, pblk, phisp, pop, punemp, ohu, multi,
         mrent, pcol, phs, str30old, hh10old, fhh, pov, ppov, pfb, pfb10, p18und, p60up) %>%
  mutate(across(-c(year, county), as.numeric)) %>%
  left_join(n_coops, by = "TRTID10") %>%
  mutate(ncoops = if_else(is.na(ncoops), 0, ncoops),
         ncoopunits = if_else(is.na(ncoopunits), 0, ncoopunits))

## Panel data ####
# combining all years
tract <- bind_rows(tract_70, tract_80, tract_90, tract_2000, tract_2010, tract_2020) 

# Create gentrification variable

tract <- tract %>%
  group_by(year) %>%
  # if tract is at or below 40th pctile, hinc
  mutate(hinc_40ile = quantile(hinc, 0.40, na.rm = TRUE), 
         gent_1 = hinc <= hinc_40ile) %>%
  # calculate tract-level change in % w/ college degree
  group_by(TRTID10) %>%
  arrange(year) %>%
  mutate(pcol_chg = pcol - lag(pcol)) %>%
  # if increase in % w/ college degree is above 50th pctile *of increases* in a given year
  group_by(year) %>%
  mutate(pcol_chg_50ile = quantile(pcol_chg[pcol_chg > 0], 0.5, na.rm = TRUE),
         gent_2 = pcol_chg >= pcol_chg_50ile) %>%
  group_by(TRTID10) %>%
  arrange(year) %>%
  # make gentrification variable
  mutate(gent = case_when(lag(gent_1) == FALSE ~ "Not gentrifiable", 
                          lag(gent_1) == TRUE & gent_2 == FALSE ~ "Non-gentrifying",
                          lag(gent_1) == TRUE & gent_2 == TRUE ~ "Gentrifying",
                          TRUE ~ NA))


rm(list=ls(pattern="^LTDB_"))



# Add crime data 

crime <- read_csv("data/crime_clean.csv") %>%
  rename(year = Year) %>%
  mutate(year = as.character(year))

tract <- left_join(tract, crime, by = c("year", "TRTID10")) %>%
  left_join(cdf, by = "year") %>%
  mutate(hinc20 = hinc * CPIAUCSL)


# Joins ####

# Combine tract data from LTDB with co-op tracts 
coops7020 <- left_join(coops_maps, tract, by = "TRTID10", relationship = "many-to-many") %>% 
  as_tibble() %>%
  select(-geometry)



# Write out data ####

write_csv(coops7020, "data/coops7020.csv")
write_csv(tract_70, "data/tract70.csv")
write_csv(tract_80, "data/tract80.csv")
write_csv(tract_90, "data/tract90.csv")
write_csv(tract_2000, "data/tract2000.csv")
write_csv(tract_2010, "data/tract2010.csv")
write_csv(tract_2020, "data/tract2020.csv")
write_csv(tract, "data/tract.csv")



