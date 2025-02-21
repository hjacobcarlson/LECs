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


# Weighted mean to fully drop NAs
weighted_mean <- function(x, w, ..., na.rm = FALSE){
  
  if(na.rm){
    
    df_omit <- na.omit(data.frame(x, w))
    
    return(weighted.mean(df_omit$x, df_omit$w, ...))
    
  } 
  
  weighted.mean(x, w, ...)
}


# Read in Data ####

# Coops
coops <-  read_csv("data/coops7020.csv") %>%
  left_join(cdf, by = "year") %>%
  mutate(hinc20 = hinc * CPIAUCSL)

# Tracts
tracts <- read_csv("data/tract.csv") %>%
  left_join(cdf, by = "year") %>%
  mutate(hinc20 = hinc * CPIAUCSL)

# Calculate means by coops, multifamily units ####

# Coops
coops.w <- tracts %>%
  filter(pop > 500) %>%
  group_by(year) %>%
  summarize(hinc20 = weighted_mean(hinc20, ncoops, na.rm = TRUE),
            powner.m = weighted_mean(powner, ncoops, na.rm = TRUE),
            pwht.m = weighted_mean(pwht, ncoops, na.rm = TRUE),
            pblk.m = weighted_mean(pblk, ncoops, na.rm = TRUE),
            phisp.m = weighted_mean(phisp, ncoops, na.rm = TRUE),
            pop.m = weighted_mean(pop, ncoops, na.rm = TRUE),
            unemp.m = weighted_mean(punemp, ncoops, na.rm = TRUE),
            mrent = weighted_mean(mrent, ncoops, na.rm = TRUE),
            pcol.m = weighted_mean(pcol, ncoops, na.rm = TRUE),
            phs.m = weighted_mean(phs, ncoops, na.rm = TRUE),
            str30.m = weighted_mean(str30old, ncoops, na.rm = TRUE),
            hh10old.m = weighted_mean(hh10old, ncoops, na.rm = TRUE),
            fhh.m = weighted_mean(fhh, ncoops, na.rm = TRUE),
            pov.m = weighted_mean(pov, ncoops, na.rm = TRUE),
            ppov.m = weighted_mean(ppov, ncoops, na.rm = TRUE),
            fb.m = weighted_mean(pfb, ncoops, na.rm = TRUE),
            fb10.m = weighted_mean(pfb10, ncoops, na.rm = TRUE),
            n18und.m = weighted_mean(p18und, ncoops, na.rm = TRUE),
            n60up.m = weighted_mean(p60up, ncoops, na.rm = TRUE)) %>%
  mutate(geog = "Coops")

# Multifamily
multi.w <- tracts %>%
  filter(pop > 500) %>%
  group_by(year) %>%
  summarize(hinc20 = weighted_mean(hinc20, multi, na.rm = TRUE),
            powner.m = weighted_mean(powner, multi, na.rm = TRUE),
            pwht.m = weighted_mean(pwht, multi, na.rm = TRUE),
            pblk.m = weighted_mean(pblk, multi, na.rm = TRUE),
            phisp.m = weighted_mean(phisp, multi, na.rm = TRUE),
            pop.m = weighted_mean(pop, multi, na.rm = TRUE),
            unemp.m = weighted_mean(punemp, multi, na.rm = TRUE),
            mrent = weighted_mean(mrent, multi, na.rm = TRUE),
            pcol.m = weighted_mean(pcol, multi, na.rm = TRUE),
            phs.m = weighted_mean(phs, multi, na.rm = TRUE),
            str30.m = weighted_mean(str30old, multi, na.rm = TRUE),
            hh10old.m = weighted_mean(hh10old, multi, na.rm = TRUE),
            fhh.m = weighted_mean(fhh, multi, na.rm = TRUE),
            pov.m = weighted_mean(pov, multi, na.rm = TRUE),
            ppov.m = weighted_mean(ppov, multi, na.rm = TRUE),
            fb.m = weighted_mean(pfb, multi, na.rm = TRUE),
            fb10.m = weighted_mean(pfb10, multi, na.rm = TRUE),
            n18und.m = weighted_mean(p18und, multi, na.rm = TRUE),
            n60up.m = weighted_mean(p60up, multi, na.rm = TRUE)) %>%
  mutate(geog = "Multifamily")




combined <- bind_rows(coops.w, multi.w) %>% 
  pivot_longer(c(contains(".m"), hinc20, mrent), names_to = "var", values_to = "percent")



# GRAPHS ####

## Time series graphs, initial vars ####

p <- combined %>%
  filter(var %in% c("hinc20", "pblk.m", "pwht.m", "phisp.m", "pcol.m", "ppov.m", "powner.m")) %>%
  mutate(var = case_when(var == "hinc20" ~ "HH Income (in $1,000s)",
                         var == "pblk.m" ~ "% Black",
                         var == "pwht.m" ~ "% White",
                         var == "phisp.m" ~ "% Latinx",
                         var == "pcol.m" ~ "% w/ College Deg.",
                         var == "ppov.m" ~ "% in Poverty",
                         var == "powner.m" ~ "% Owners"),
         percent = if_else(percent < 1, percent * 100, percent / 1000)) %>%
  ggplot(aes(x = year, y = percent, group = geog)) +
  geom_line(aes(color = geog)) +
  labs(color = "",
       x = "",
       y = "") +
  facet_wrap(~var, scales = "free") +
  theme_minimal()

p

ggsave(p, file = "out/desc_city.pdf",
       height = 5.75,
       width = 7.75)


# Gentrification

gent <- tracts %>%
  filter(!is.na(gent), year != 1970) %>%
  group_by(year, gent) %>%
  summarize(coops = sum(ncoops, na.rm = TRUE),
            multi = sum(multi, na.rm = TRUE)) %>%
  group_by(year) %>%
  mutate(coops_sum = sum(coops, na.rm = TRUE),
         multi_sum = sum(multi, na.rm = TRUE),
         pcoops = coops/coops_sum,
         pmulti = multi/multi_sum) %>%
  select(year, gent, pcoops, pmulti) %>%
  pivot_longer(c(pcoops, pmulti), names_to = "Geog", values_to = "Percent") %>%
  mutate(Geog = case_when(Geog == "pcoops" ~ "Coops",
                          Geog == "pmulti" ~ "Multifamily"),
         Percent = Percent * 100)


p <- ggplot(gent, aes(x = year, y = Percent, group = Geog)) +
  geom_col(aes(fill = Geog), position = "dodge") +
  facet_wrap(~gent) +
  labs(fill = "",
       x = "") +
  theme_minimal() +
  theme(legend.position = "bottom")
p

ggsave(p, file = "out/gent.pdf",
       height = 3.75,
       width = 5.75)


# By counties ####


# Coops
coops.w_co <- tracts %>%
  filter(pop > 500) %>%
  filter(!county %in% c("Queens County", "Richmond County")) %>%
  group_by(year, county) %>%
  summarize(hinc20 = weighted_mean(hinc20, ncoops, na.rm = TRUE),
            powner.m = weighted_mean(powner, ncoops, na.rm = TRUE),
            pwht.m = weighted_mean(pwht, ncoops, na.rm = TRUE),
            pblk.m = weighted_mean(pblk, ncoops, na.rm = TRUE),
            phisp.m = weighted_mean(phisp, ncoops, na.rm = TRUE),
            pop.m = weighted_mean(pop, ncoops, na.rm = TRUE),
            unemp.m = weighted_mean(punemp, ncoops, na.rm = TRUE),
            mrent = weighted_mean(mrent, ncoops, na.rm = TRUE),
            pcol.m = weighted_mean(pcol, ncoops, na.rm = TRUE),
            phs.m = weighted_mean(phs, ncoops, na.rm = TRUE),
            str30.m = weighted_mean(str30old, ncoops, na.rm = TRUE),
            hh10old.m = weighted_mean(hh10old, ncoops, na.rm = TRUE),
            fhh.m = weighted_mean(fhh, ncoops, na.rm = TRUE),
            pov.m = weighted_mean(pov, ncoops, na.rm = TRUE),
            ppov.m = weighted_mean(ppov, ncoops, na.rm = TRUE),
            fb.m = weighted_mean(pfb, ncoops, na.rm = TRUE),
            fb10.m = weighted_mean(pfb10, ncoops, na.rm = TRUE),
            n18und.m = weighted_mean(p18und, ncoops, na.rm = TRUE),
            n60up.m = weighted_mean(p60up, ncoops, na.rm = TRUE)) %>%
  mutate(geog = "Coops")

# Multifamily
multi.w_co <- tracts %>%
  filter(pop > 500) %>%
  filter(!county %in% c("Queens County", "Richmond County")) %>%
  group_by(year, county) %>%
  summarize(hinc20 = weighted_mean(hinc20, multi, na.rm = TRUE),
            powner.m = weighted_mean(powner, multi, na.rm = TRUE),
            pwht.m = weighted_mean(pwht, multi, na.rm = TRUE),
            pblk.m = weighted_mean(pblk, multi, na.rm = TRUE),
            phisp.m = weighted_mean(phisp, multi, na.rm = TRUE),
            pop.m = weighted_mean(pop, multi, na.rm = TRUE),
            unemp.m = weighted_mean(punemp, multi, na.rm = TRUE),
            mrent = weighted_mean(mrent, multi, na.rm = TRUE),
            pcol.m = weighted_mean(pcol, multi, na.rm = TRUE),
            phs.m = weighted_mean(phs, multi, na.rm = TRUE),
            str30.m = weighted_mean(str30old, multi, na.rm = TRUE),
            hh10old.m = weighted_mean(hh10old, multi, na.rm = TRUE),
            fhh.m = weighted_mean(fhh, multi, na.rm = TRUE),
            pov.m = weighted_mean(pov, multi, na.rm = TRUE),
            ppov.m = weighted_mean(ppov, multi, na.rm = TRUE),
            fb.m = weighted_mean(pfb, multi, na.rm = TRUE),
            fb10.m = weighted_mean(pfb10, multi, na.rm = TRUE),
            n18und.m = weighted_mean(p18und, multi, na.rm = TRUE),
            n60up.m = weighted_mean(p60up, multi, na.rm = TRUE)) %>%
  mutate(geog = "Multifamily")




combined_co <- bind_rows(coops.w_co, multi.w_co) %>% 
  pivot_longer(c(contains(".m"), hinc20, mrent), names_to = "var", values_to = "percent")

# GRAPHS ####
p <- combined_co %>%
  filter(var %in% c("hinc20", "pblk.m", "pwht.m", "phisp.m", "pcol.m", "ppov.m", "powner.m")) %>%
  mutate(var = case_when(var == "hinc20" ~ "HH Income (2020 $s)",
                         var == "pblk.m" ~ "% Black",
                         var == "pwht.m" ~ "% White",
                         var == "phisp.m" ~ "% Latinx",
                         var == "pcol.m" ~ "% w/ College Deg.",
                         var == "ppov.m" ~ "% in Poverty",
                         var == "powner.m" ~ "% Owners"),
         percent = if_else(percent < 1, percent * 100, percent / 1000)) %>%
  ggplot(aes(x = year, y = percent)) +
  geom_line(aes(color = county, linetype = geog)) +
  labs(color = "",
       linetype = "",
       x = "",
       y = "") +
  facet_wrap(~var, scales = "free") +
  theme_minimal() 
p

ggsave(p, file = "out/desc_counties.pdf",
       height = 5.75,
       width = 7.75)
