crime_data <- read.csv("~/Downloads/Book 15(Sheet1) (2).csv")

library(tidyr)

colnames(crime_data) <- gsub("^X", "", colnames(crime_data))

crime_long <- crime_data %>%
  pivot_longer(
    cols = starts_with("20"), # Select year columns
    names_to = "Year", # New column for year
    values_to = "Count")
summary(crime_long)


library(dplyr)
crime_long <- crime_long %>% arrange(Year, CRIME)


