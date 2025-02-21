library(tidyr)
library(dplyr)

crime_data <- read.csv("data/crime_original.csv") # read in data

colnames(crime_data) <- gsub("^X", "", colnames(crime_data)) #remove "X" in front of years


crime_data <- crime_data %>% 
  pivot_longer(
    cols = matches("^20|^1990"), # Select year columns
    names_to = "Year", # New column for year
    values_to = "Count") %>% arrange(Year, CRIME)  



