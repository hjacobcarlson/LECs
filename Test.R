library(tidyverse)

test_df <- data.frame(full_address = c("1 Barker St Mt. Kisco, NY 10549",
                                  "1 COOPER SQUARE New York, NY 10003",
                                  "1 MORNINGSIDE AVENUE New York, NY 10026",
                                  "10 MORNINGSIDE AVENUE New York, NY 10026",
                                  "10 ST NICHOLAS TERRACE New York, NY 10027"),
                 stringsAsFactors = FALSE)

boroughs <- c("Manhattan", "New York", "Brooklyn", "Queens", "Bronx")

test_df$borough <- str_extract(test_df$full_address, paste(boroughs, collapse = "|"))

test_df$street_address <- str_replace(test_df$full_address, paste(boroughs, collapse = "|"), "")
test_df$street_address <- str_trim(test_df$street_address)  # Remove any extra spaces

coops_test <- read_csv("data/NY Co-op Data(Sheet1).csv") %>%
  filter(!is.na(ID)) %>%
  filter(!is.na(LONG)) %>%
  filter(!Type %in% c("Affordable, Former HUD 213", "Senior", "Student co-op")) # omitting some types of "coops", may change later

coops_test$borough <- str_extract(coops_test$ADDRESS, paste(boroughs, collapse = "|"))

coops_test$street_address <- str_replace(coops_test$ADDRESS, paste(boroughs, collapse = "|"), "")
coops_test$street_address <- str_trim(coops_test$street_address)  # Remove any extra spaces

