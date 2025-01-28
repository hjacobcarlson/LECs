
threshold70 <- quantile(tract_70$hinc, 0.40, na.rm = TRUE)
threshold70

threshold80 <- quantile(tract_80$hinc, 0.40, na.rm = TRUE)
threshold80


threshold90 <- quantile(tract_90$hinc, 0.40, na.rm = TRUE)
threshold90


threshold00 <- quantile(tract_2000$hinc, 0.40, na.rm = TRUE)
threshold00


threshold10 <- quantile(tract_2010$hinc, 0.40, na.rm = TRUE)
threshold10


threshold20 <- quantile(tract_2020$hinc, 0.40, na.rm = TRUE)
threshold20

library(dplyr)
tract <- tract %>%
mutate(
  gent = case_when(
    year >= 1970 & year < 1980 ~ hinc <= 7906.25,  # 1970s threshold
    year >= 1980 & year < 1990 ~ hinc <= 12672,    # 1980s threshold
    year >= 1990 & year < 2000 ~ hinc <= 27075.6,    # 1990s threshold
    year >= 2000 & year < 2010 ~ hinc <= 34212.94, 
    year >= 2010 & year < 2020 ~ hinc <= 46853.4,
    year >= 2020 & year < 2030 ~ hinc <= 59203.2,
    TRUE ~ FALSE  # Default case, for years outside defined decades
  ) 
)


tract <- tract %>% arrange(TRTID10, year)



tract <- tract %>% group_by(TRTID10) %>% mutate(col_per = pcol - lag(pcol))

tract <- tract %>% arrange(year)


tract <- tract %>%
  mutate(
    colr = case_when(
      year >= 1980 & year < 1990 ~ col_per >= 0.02,    # 1980s threshold
      year >= 1990 & year < 2000 ~ col_per >= 0.03 ,    # 1990s threshold
      year >= 2000 & year < 2010 ~ col_per >= 0.02, 
      year >= 2010 & year < 2020 ~ col_per >= 0.04,
      year >= 2020 & year < 2030 ~ col_per >= 0.02,
      TRUE ~ FALSE  # Default case, for years outside defined decades
    ) 
  )







colper80fr <- tract %>% filter(year == 1980)
quantile(colper80fr$col_per, 0.50, na.rm = TRUE)


colper90fr <- tract %>% filter(year == 1990)
quantile(colper90fr$col_per, 0.50, na.rm = TRUE)


colper00fr <- tract %>% filter(year == 2000)
quantile(colper00fr$col_per, 0.50, na.rm = TRUE)


colper10fr <- tract %>% filter(year == 2010)
quantile(colper10fr$col_per, 0.50, na.rm = TRUE)


colper20fr <- tract %>% filter(year == 2020)
quantile(colper20fr$col_per, 0.50, na.rm = TRUE)


tract <- tract %>%
  mutate(gentstat = ifelse(
gent == TRUE & !is.na(colr) & colr == TRUE,  
    "gentrified",                                                 
    "not-gentrified"                                              
  ))

