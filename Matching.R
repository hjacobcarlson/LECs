
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyverse)

boroughs <- c("Manhattan", "New York", "Brooklyn", "Queens", "Bronx")

coops_test <- read_csv("data/NY Co-op Data(Sheet1).csv") %>% 
  filter(!is.na(ID)) %>% 
  filter(!is.na(LONG)) %>% 
  filter(!Type %in% c("Affordable, Former HUD 213", "Senior", "Student co-op")) # omitting some types of "coops", may change later 

coops_test$borough <- str_extract(coops_test$ADDRESS, paste(boroughs, collapse = "|")) 

coops_test$street_address <- str_replace(coops_test$ADDRESS, paste(boroughs, collapse = "|"), "") 
coops_test$street_address <- str_trim(coops_test$street_address) # Remove any extra spaces 

coops_cleaned <- coops_test %>% 
  mutate(
    zip_code = str_extract(street_address, "\\d{5}"),  # Correct escape for digits
    street_address = str_remove(street_address, "NY\\s*\\d{5}"),  # Remove NY and zip (escaped correctly)
    street_address = str_trim(str_remove(street_address, "\\d{5}$")),  # Just in case ZIP is still stuck at the end
    street_address = street_address %>%
      str_replace_all(",", "") %>%
      str_replace_all("The ", "") %>%
      str_replace_all("New York", "") %>%
      str_replace_all("NY", "")
  )

coops_cleaned$street_address <- toupper(coops_cleaned$street_address) 

# import borough data 
brooklyn_rent <- read_csv("data/b3_brooklyn_coop_comp_032224.csv")
brooklyn_address <- select(brooklyn_rent, `Address...2`, "Neighborhood...3", "Year Built...6")

bronx_rent <- read_csv("data/b2_bronx_coop_comp_d030122 (1).csv") 
bronx_address <- select(bronx_rent, `Address...2`, "Neighborhood...3")
manhattan_rent <- read_csv("data/b1_manhattan_coop_comp_d030122 (1) 2.csv")
manhattan_address <- select(manhattan_rent, `Address...2`, "Neighborhood...3")

ny_address <- bind_rows(bronx_address, manhattan_address, brooklyn_address)
ny_address <- ny_address %>% mutate(Address = str_squish(Address))


coops_clean_test <- coops_cleaned %>%
  rename(Address = street_address) %>%
  select(Address, Type) 

#### fixing addresses

coops_clean_test$Address <- str_replace_all(coops_clean_test$Address, "TIFFA", "TIFFANY") 
coops_clean_test$Address <- str_replace_all(coops_clean_test$Address, "WEST WEST", "WEST") 
coops_clean_test$Address <- str_replace_all(coops_clean_test$Address, "\\bE\\b", "EAST")
coops_clean_test$Address <- str_replace_all(coops_clean_test$Address, " ST ", " STREET ") 
coops_clean_test$Address <- str_replace_all(coops_clean_test$Address, " STREET NICHOLAS ", " ST NICHOLAS ") 



coops_clean_test <- coops_clean_test %>%
  mutate(Address = str_trim(Address, side = "right")) 


coops_rent <- left_join(coops_clean_test, ny_address) 

write.csv(coops_rent, "coopsrent.csv", row.names = FALSE) 



