library(readxl)
library(dplyr)
#1970 Coding 
#Import data from LTDB
LTDB_1970 <- read.csv("data/ltdb_std_all_fullcount/LTDB_Std_1970_fullcount.csv")
LTDB_1970_sample <- read.csv("data/ltdb_std_all_sample/ltdb_std_1970_sample.csv")

#Filter for NY only 
extra_data <- filter(LTDB_1970, 
                     county == "Bronx County" | county == "Queens County" | county == "Kings County" 
                     | county == "Richmond County" | county == "New York County" & state == "NY")

LTDB_1970 <- filter(extra_data, state == "NY")
# Join sample and population 
LTDB_1970 <- left_join(LTDB_1970, LTDB_1970_sample, by = 'TRTID10')
colnames(LTDB_1970) 
#create tract file 
tract_70 <- LTDB_1970 %>%
  rename(
    hinc = HINC70,
    owner = OWN70,
    ohu = OHU70,
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
    pfb = FB70,
    pfb10 = N10IMM70,
    p18und = A18UND70,
    p60up = A60UP70
  ) %>%
  mutate(
    powner = owner / ohu,
    pwht = wht / pop,
    pblk = blk / pop, 
    phs = hs / pop,
    pcol = col / pop, 
    punemp = unemp / clf
  ) %>% mutate(year = "1970") %>%
  select(year,
         TRTID10, hinc, powner, pwht, pblk, pop, punemp, mrent, pcol, phs, str30old, hh10old, fhh, pov, pfb, pfb10, p18und, p60up
  ) %>%
  mutate(
    across(-year, 
           ~ ifelse(is.na(.), NA, sprintf("%.2f", as.numeric(.))))
  )




#1980
#Import data
LTDB_1980 <- read.csv("data/ltdb_std_all_fullcount/LTDB_Std_1980_fullcount.csv")
LTDB_1980_sample <- read.csv("data/ltdb_std_all_sample/ltdb_std_1980_sample.csv")

#filter for NY only 
extra_data <- filter(LTDB_1980, 
                     county == "Bronx County" | county == "Queens County" | county == "Kings County" 
                     | county == "Richmond County" | county == "New York County" & state == "NY")

LTDB_1980 <- filter(extra_data, state == "NY")

#Match a column for sample to population
LTDB_1980_sample <- LTDB_1980_sample %>% 
  rename(TRTID10 = trtid10)

#Join sample and population 
LTDB_1980 <- left_join(LTDB_1980, LTDB_1980_sample, by = 'TRTID10')

colnames(LTDB_1980)

#create tract file 

tract_80 <- LTDB_1980 %>%  rename(hinc = hinc80,
                                  owner = OWN80,
                                  ohu = OHU80,
                                  wht = NHWHT80,
                                  blk = NHBLK80,
                                  pop = POP80, hs = hs80,
                                  col = col80, 
                                  unemp = unemp80, 
                                  mrent = MRENT80,
                                  clf = clf80,
                                  str30old = h30old80,
                                  hh10old = h10yrs80,
                                  fhh = fhh80,
                                  pov = npov80,
                                  pfb = fb80,
                                  pfb10 = n10imm80,
                                  p18und = A18UND80,
                                  p60up = A60UP80
) %>%
  mutate(
    powner = owner / ohu,
    pwht = wht / pop,
    pblk = blk / pop, 
    phs = hs / pop,
    pcol = col / pop, 
    punemp = unemp / clf
  ) %>% mutate(year = "1980") %>%
  select(year,
         TRTID10, hinc, powner, pwht, pblk, pop, punemp, mrent, pcol, phs, str30old, hh10old, fhh, pov, pfb, pfb10, p18und, p60up
  ) %>%
  mutate(
    across(-year, 
           ~ ifelse(is.na(.), NA, sprintf("%.2f", as.numeric(.))))
  )

#1990
#import data 
LTDB_1990 <- read.csv("data/ltdb_std_all_fullcount/LTDB_Std_1990_fullcount.csv")
LTDB_1990_sample <- read.csv("data/ltdb_std_all_sample/ltdb_std_1990_sample.csv")

#filter for NY only 
extra_data <- filter(LTDB_1990, 
                     county == "Bronx County" | county == "Queens County" | county == "Kings County" 
                     | county == "Richmond County" | county == "New York County" & state == "NY")

LTDB_1990 <- filter(extra_data, state == "NY")

# Join sample and population 
LTDB_1990 <- left_join(LTDB_1990, LTDB_1990_sample, by = 'TRTID10')
colnames(LTDB_1990)

#create tract file 
tract_90 <- LTDB_1990 %>%  rename(hinc = HINC90,
                                  owner = OWN90,
                                  ohu = OHU90,
                                  wht = NHWHT90,
                                  blk = NHBLK90,
                                  pop = POP90, hs = HS90,
                                  col = COL90, 
                                  unemp = UNEMP90, 
                                  mrent = MRENT90, 
                                  clf = CLF90,
                                  str30old = H30OLD90,
                                  hh10old = H10YRS90,
                                  fhh = FHH90,
                                  pov = NPOV90,
                                  pfb = FB90,
                                  pfb10 = N10IMM90,
                                  p18und = A18UND90,
                                  p60up = A60UP90
) %>%
  mutate(
    powner = owner / ohu,
    pwht = wht / pop,
    pblk = blk / pop, 
    phs = hs / pop,
    pcol = col / pop, 
    punemp = unemp / clf
  ) %>% mutate(year = "1990") %>%
  select(year,
         TRTID10, hinc, powner, pwht, pblk, pop, punemp, mrent, pcol, phs, str30old, hh10old, fhh, pov, pfb, pfb10, p18und, p60up
  ) %>%
  mutate(
    across(-year, 
           ~ ifelse(is.na(.), NA, sprintf("%.2f", as.numeric(.))))
  )

#2000
#import data from LTDB 
LTDB_2000 <- read.csv("data/ltdb_std_all_fullcount/LTDB_Std_2000_fullcount.csv")
LTDB_2000_sample <- read.csv("data/ltdb_std_all_sample/ltdb_std_2000_sample.csv")


#Filter data for NY only 
extra_data <- filter(LTDB_2000, 
                     county == "Bronx County" | county == "Queens County" | county == "Kings County" 
                     | county == "Richmond County" | county == "New York County" & state == "NY")

LTDB_2000 <- filter(extra_data, state == "NY")


#join sample and population 
LTDB_2000 <- left_join(LTDB_2000, LTDB_2000_sample, by = 'TRTID10')
colnames(LTDB_2000)

#create tract file 
tract_2000 <- LTDB_2000 %>%  rename(hinc = HINC00,
                                    owner = OWN00,
                                    ohu = HU00,
                                    wht = NHWHT00,
                                    blk = NHBLK00,
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
                                    pfb = FB00,
                                    pfb10 = N10IMM00,
                                    p18und = A18UND00,
                                    p60up = A60UP00
) %>%
  mutate(
    powner = owner / ohu,
    pwht = wht / pop,
    pblk = blk / pop, 
    phs = hs / pop,
    pcol = col / pop, 
    punemp = unemp / clf
  ) %>% mutate(year = "2000") %>%
  select(year,
         TRTID10, hinc, powner, pwht, pblk, pop, punemp, mrent, pcol, phs, str30old, hh10old, fhh, pov, pfb, pfb10, p18und, p60up
  ) %>%
  mutate(
    across(-year, 
           ~ ifelse(is.na(.), NA, sprintf("%.2f", as.numeric(.))))
  )

#2010
#import data 
LTDB_2010 <- read.csv("data/ltdb_std_all_fullcount/LTDB_Std_2010_fullcount.csv")
LTDB_2008_2012_sample <- read.csv("data/ltdb_std_all_sample/LTDB_std_200812_Sample.csv")

#filter for NY only 
extra_data <- filter(LTDB_2010, 
                     county == "Bronx County" | county == "Queens County" | county == "Kings County" 
                     | county == "Richmond County" | county == "New York County" & state == "NY")

LTDB_2010 <- filter(extra_data, state == "NY")

# match for population and sample column 
LTDB_2010 <- LTDB_2010 %>% rename(TRTID10 = tractid)
LTDB_2008_2012_sample <- LTDB_2008_2012_sample %>% rename(TRTID10 = tractid)

#join sample and population
LTDB_2010 <- left_join(LTDB_2010, LTDB_2008_2012_sample, by = 'TRTID10')
colnames(LTDB_2010)
#create tract file 
tract_2010 <- LTDB_2010 %>%  rename(hinc = hinc12,
                                    owner = own10,
                                    ohu = hu10,
                                    wht = nhwht10,
                                    blk = nhblk10,
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
                                    pfb = pfb12,
                                    pfb10 = n10imm12,
                                    p18und = a18und10,
                                    p60up = a60up10
) %>%
  mutate(
    powner = owner / ohu,
    pwht = wht / pop,
    pblk = blk / pop, 
    phs = hs / pop,
    pcol = col / pop, 
    punemp = unemp / clf
  ) %>% mutate(year = "2010") %>%
  select(year,
         TRTID10, hinc, powner, pwht, pblk, pop, punemp, mrent, pcol, phs, str30old, hh10old, fhh, pov, pfb, pfb10, p18und, p60up
  ) %>%
  mutate(
    across(-year, 
           ~ ifelse(is.na(.), NA, sprintf("%.2f", as.numeric(.))))
  )


#2020
#import data from LTDB
LTDB_2020 <- read.csv("data/ltdb_std_all_fullcount/ltdb_std_2020_fullcount.csv")
LTDB_2015_2019_sample <- read.csv("data/ltdb_std_all_sample/LTDB_std_201519_Sample.csv")

# match population and sample column 
LTDB_2020 <- LTDB_2020 %>% rename(TRTID10 = TRTID2010)
LTDB_2015_2019_sample <- LTDB_2015_2019_sample %>% rename(TRTID10 = tractid)

# install sf and read in census 2020 shapefile 
install.packages("sf")
library(sf)
str(LTDB_2020)
census_2020_shp <- st_read("data/nyu_2451_34505/nyu_2451_34505.shp") 
census_2020_shp <- census_2020_shp %>% rename(TRTID10 = tractid)
census_2020_shp$TRTID10 <- as.numeric(census_2020_shp$TRTID10)

# match census with 2020 to get only NY tracts 
LTDB_2020_with_shp <- left_join(census_2020_shp, LTDB_2020, by = "TRTID10")  
LTDB_2020 <- LTDB_2020_with_shp %>% select(-CTLabel,-BoroCode,-BoroName, -BoroCT2020,-CDEligibil,-NTAName, -NTA2020, -CDTA2020,-CDTANAME, -PUMA, -Shape_Leng, -Shape_Area, -geometry, -CT2020 )
LTDB_2020 <- LTDB_2020 %>% select(-geometry)
colnames(LTDB_2015_2019_sample)

#join sample and population 
LTDB_2020 <- left_join(LTDB_2020, LTDB_2015_2019_sample)
colnames(LTDB_2020)

#create tract file 
tract_2020 <- LTDB_2020 %>%  rename(hinc = hinc19,
                                    owner = own19,
                                    ohu = hu19,
                                    wht = nhwt20,
                                    blk = nhblk20,
                                    pop = pop20, hs = hs19,
                                    col = col19, 
                                    unemp = unemp19, 
                                    mrent = mrent19, 
                                    clf = clf19,
                                    str30old = h30old19,
                                    hh10old = h10yrs19,
                                    fhh = fhh19,
                                    pov = npov19,
                                    pfb = pfb19,
                                    pfb10 = n10imm19,
                                    p18und = a18und19,
                                    p60up = a60up19
) %>%
  mutate(
    powner = owner / ohu,
    pwht = wht / pop,
    pblk = blk / pop, 
    phs = hs / pop,
    pcol = col / pop, 
    punemp = unemp / clf
  ) %>% mutate(year = "2020") %>%
  st_drop_geometry() %>%
  select(year,
         TRTID10, hinc, powner, pwht, pblk, pop, punemp, mrent, pcol, phs, str30old, hh10old, fhh, pov, pfb, p18und, p60up
  ) %>%
  mutate(
    across(-year, 
           ~ ifelse(is.na(.), NA, sprintf("%.2f", as.numeric(.))))
  )

# combining all years
tract <- bind_rows(tract_70, tract_80, tract_90, tract_2000, tract_2010, tract_2020) %>% group_by(year)%>% select(year, everything()) %>% group_by(year) %>%
  distinct() 

#insert UHAB coop scrape 
coops <- read.csv("data/Clean UHAB Data - Sheet1 (1).csv")

# Convert to sf object
coops_shp <- st_as_sf(coops, coords = c("Longitude", "Latitude"), crs = 4326)  # EPSG:4326 is the WGS 84 coordinate system

coops_shp <- st_transform(coops_shp, st_crs(census_2020_shp))

# combine co-op addresses with their tracts 
library(sf)
coop_tract_intersect <- st_intersection(census_2020_shp, coops_shp) %>%
  as_tibble() %>%
  select(-CDEligibil)  

#combine tract data from LTDB with co-op tracts 
coops7020 <- left_join(coop_tract_intersect, tract, by = "TRTID10") %>% select(-geometry, -CTLabel, -CT2020, -BoroCT2020, -NTA2020, -CDTA2020, -PUMA, -Shape_Leng, -Shape_Area)


library(writexl)



# Export to Excel

write_xlsx(coops7020, "coops7020.xlsx")


