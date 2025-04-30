
library(tidyverse)
library(dplyr)
library(stringr)
# import borough data 
brooklyn_rent <- read_csv("data/b3_brooklyn_coop_comp_032224.csv")
brooklyn_address <- select(brooklyn_rent, `Address...2`, "Neighborhood...3", "Year Built...6")

bronx_rent <- read_csv("data/b2_bronx_coop_comp_d030122 (1).csv") 
bronx_address <- select(bronx_rent, `Address...2`, "Neighborhood...3")
manhattan_rent <- read_csv("data/b1_manhattan_coop_comp_d030122 (1) 2.csv")
manhattan_address <- select(manhattan_rent, `Address...2`, "Neighborhood...3")

ny_address <- bind_rows(bronx_address, manhattan_address, brooklyn_address)
ny_address <- ny_address %>% mutate(Address = str_squish(Address))


coops_clean_test <- coops_cleaned %>% select("Address", "Type")
coops_clean_test <- coops_clean_test %>%
  mutate(Address = str_trim(Address, side = "right")) 

coops_rent <- left_join(coops_clean_test, ny_address)

