library(tidyverse)
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

# Read in Data ####
# Coops
coops <- read_csv("data/coops7020.csv") %>%
  left_join(cdf, by = "year") %>%
  mutate(hinc20 = hinc * CPIAUCSL)

# Tracts
tracts <- read_csv("data/tract.csv") %>%
  left_join(cdf, by = "year") %>%
  mutate(hinc20 = hinc * CPIAUCSL)


coops.d <- coops %>%
  filter(pop > 500) %>%
  group_by(year) %>%
  summarize(hinc = mean(hinc20, na.rm = TRUE),
            hinc2 = mean(hinc20, na.rm = TRUE),
            powner.m = mean(powner, na.rm = TRUE),
            pwht.m = mean(pwht, na.rm = TRUE),
            pblk.m = mean(pblk, na.rm = TRUE),
            pop.m = mean(pop, na.rm = TRUE), 
            unemp.m = mean(punemp, na.rm = TRUE),
            mrent = mean(mrent, na.rm = TRUE),
            pcol.m = mean(pcol, na.rm = TRUE),
            phs.m = mean(phs, na.rm = TRUE),
            str30.m = mean(str30old, na.rm = TRUE),
            hh10old.m = mean(hh10old, na.rm = TRUE),
            fhh.m = mean(fhh, na.rm = TRUE),
            pov.m = mean(pov, na.rm = TRUE),
            fb.m = mean(pfb, na.rm = TRUE),
            fb10.m = mean(pfb10, na.rm = TRUE),
            n18und.m = mean(p18und, na.rm = TRUE),
            n60up.m = mean(p60up, na.rm = TRUE)) %>%
  mutate(geog = "Coops")

tracts.d <- tracts %>%
  filter(pop > 500) %>%
  group_by(year) %>%
  summarize(hinc2 = mean(hinc, na.rm = TRUE),
            hinc = mean(hinc20, na.rm = TRUE),
            powner.m = mean(powner, na.rm = TRUE),
            pwht.m = mean(pwht, na.rm = TRUE),
            pblk.m = mean(pblk, na.rm = TRUE),
            pop.m = mean(pop, na.rm = TRUE),
            unemp.m = mean(punemp, na.rm = TRUE),
            mrent = mean(mrent, na.rm = TRUE),
            pcol.m = mean(pcol, na.rm = TRUE),
            phs.m = mean(phs, na.rm = TRUE),
            str30.m = mean(str30old, na.rm = TRUE),
            hh10old.m = mean(hh10old, na.rm = TRUE),
            fhh.m = mean(fhh, na.rm = TRUE),
            pov.m = mean(pov, na.rm = TRUE),
            fb.m = mean(pfb, na.rm = TRUE),
            fb10.m = mean(pfb10, na.rm = TRUE),
            n18und.m = mean(p18und, na.rm = TRUE),
            n60up.m = mean(p60up, na.rm = TRUE)) %>%
  mutate(geog = "City")


combined <- bind_rows(tracts.d, coops.d) %>% 
  pivot_longer(c(contains(".m"), hinc, hinc2, mrent), names_to = "var", values_to = "percent")


# GRAPHS ####

## Time series graphs, initial vars ####

p <- combined %>%
  #filter(var != "pop.m") %>%
  ggplot(aes(x = year, y = percent, group = geog)) +
  geom_line(aes(color = geog)) +
  facet_wrap(~var, scales = "free") +
  theme_minimal()

p


boro <- coops %>%
  group_by(BoroName) %>%
  summarize(n = n())


mean(tract_2010$pfb)
