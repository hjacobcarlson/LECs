
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
brooklyn_rent <- brooklyn_rent %>% 
                  rename(Address = Address...2)

testtest <- left_join(coops_cleaned, brooklyn_rent)

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
coops_clean_test$Address <- str_replace_all(coops_clean_test$Address, "4250 KATONAH AVENUE", "4260 KATONAH AVENUE") 


coops_clean_test <- coops_clean_test %>%
  mutate(Address = str_trim(Address, side = "right")) 


coops_rent <- left_join(coops_clean_test, ny_address) 

write.csv(coops_rent, "coopsrent.csv", row.names = FALSE) 


mh_test_23 <- read_csv("~/Downloads/data/b1_manhattan_coop_comp_d032223.csv")
mh_test_22 <- read_csv("~/Downloads/data/b1_manhattan_coop_comp_d030122 (2).csv")
mh_test_13 <- read_csv("~/Downloads/Sheet1-Table 1.csv")

# Assume df is your data frame and row 1 contains column names
names(mh_test_23) <- as.character(mh_test_23[1, ])  # Set first row as column names
mh_test_23 <- mh_test_23[-1, ]  # Remove the first row

mh_test_22 <- mh_test_22 %>%
  rename(Address = Address...2) %>% select(Address) 

mh_test13 <- mh_test_13 %>%
  rename(Address = Address...2) %>% select(Address) 

names(mh_test_23) <- make.names(names(mh_test_23), unique = TRUE)

mh_test23 <- mh_test_23 %>% select(Address)

df_unique <- df %>% distinct(Address, .keep_all = TRUE)
mh_test23 <- mh_test23 %>% distinct(Address, .keep_all = TRUE)
mh_test22 <- mh_test_22 %>% distinct(Address, .keep_all = TRUE)
mh_test13 <- mh_test13 %>% distinct(Address, .keep_all = TRUE)

mh_test13 <- mh_test13 %>% mutate(Address = str_squish(Address))
mh_test_23b <- mh_test23 %>% mutate(Address = str_squish(Address))

joined_mh <- full_join(mh_test23, mh_test22, by = "Address")

inner_joined_mh <- inner_join(mh_test23,mh_test22, by = "Address")

joined_mh_2313 <- full_join(mh_test23, mh_test13, by = "Address")
inner_joined_mh2313 <- inner_join(mh_test23,mh_test13, by = "Address")
k