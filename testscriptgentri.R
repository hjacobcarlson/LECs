
n <- 30 
random_sample_1970 <- tract_70[sample(nrow(tract_70), n), ]

median_value_1970 <- median(random_sample_1970$hinc, na.rm = TRUE)

threshold <- quantile(random_sample_1970$hinc, 0.40, na.rm = TRUE)

fourt <- random_sample_1970[random_sample_1970$hinc <= threshold, ]
fourt
max(fourt$hinc)
random_sample_1970$gent <- ifelse(random_sample_1970$hinc <= 8507.94, "Gentrifiable", "Non-Gentrifiable")


library(dplyr)
sample_1970_1980 <- left_join(random_sample_1970, tract_80, by = "TRTID10")

sample_1970_1980$college_rate <- sample_1970_1980$pcol.y - sample_1970_1980$pcol.x

tres <- quantile(sample_1970_1980$college_rate, 0.50, na.rm = TRUE) 
col <- sample_1970_1980[sample_1970_1980$college_rate >= tres, ]
col
sample_1970_1980$gent2 <- ifelse(sample_1970_1980$college_rate <= 0.02, "Lower", "Higher") 

need <- sample_1970_1980 %>% select(gent2, gent, college_rate)

need <- need %>% mutate(gentri = ifelse(gent2 == "Higher" & gent == "Gentrifiable", "Gentrified", "Not Gentrified"))








# Full sample ####

thresholda <- quantile(tract_70$hinc, 0.40, na.rm = TRUE) 
tract_70$gent <- ifelse(tract_70$hinc <= 7906.25, "Gentrifiable", "Non-Gentrifiable")

tract$gent <- ifelse(tract$year == 1970 & tract$hinc <= 7906.25, "Gentrifiable", "Non-Gentrifiable")
tract_70$gent70 <- ifelse(tract_70$hinc <= 7906.25, "Gentrifiable", "Non-Gentrifiable")

college_rates <- left_join(tract_70,tract_80, by = "TRTID10") %>% rename(col1970 = pcol.x, col1980 = pcol.y, hinc1970 = hinc.x, hinc1980 = hinc.y) 
college_rates$gent70 <- ifelse(college_rates$hinc1970 <= 7906.25, "Gentrifiable", "Non-Gentrifiable")
college_rates$change7080 <- college_rates$col1980 - college_rates$col1970
college_rates <- college_rates %>% select(col1970,hinc1970, gent70, col1980, hinc1980, gent80) 
tresb <- quantile(college_rates$change7080, 0.50, na.rm = TRUE) 
college_rates$gent1980 <- ifelse(college_rates$change >= 0.02 & college_rates$gent == "Gentrifiable", "Gentrified", "Not Gentrified")




thresholdb <- quantile(college_rates$hinc1980, 0.40, na.rm = TRUE)

college_rates$gent80 <- ifelse(college_rates$hinc1980 <= 12662.4, "Gentrifiable", "Non-Gentrifiable") 


