library(tidyverse)
library(MASS)
library(ggeffects)


df <- read_csv("data/tract.csv") %>%
  filter(year != 1970) %>%
  mutate(year = relevel(factor(year), "1980"),
         gent = relevel(factor(gent), "Non-gentrifying"),
         ptotfel = (totfel/pop) * 1000)




# Reg


m1 <- glm.nb(ncoops ~ gent * year, data = df)
summary(m1)
pm1 <- predict_response(m1, terms = c("gent", "year"))

p <- ggplot(pm1, aes(x = group, y = predicted, group =x,  color = x)) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15) +
  labs(color = "",
       y = "Predicted # Coops",
       x = "Year") +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(p, file = "out/mod_nocontrols.png",
       height = 4,
       width = 6.2)

m2 <- glm.nb(ncoops ~ gent*year +
               multi + powner + pwht + pblk + phisp + mrent + pcol + ppov, data = df)
summary(m2)
pm2 <- predict_response(m2, terms = c("gent", "year"))

p <- ggplot(pm2, aes(x = group, y = predicted, group =x,  color = x)) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15) +
  labs(color = "",
       y = "Predicted # Coops",
       x = "Year") +
  theme_classic() +
  theme(legend.position = "bottom")
p
ggsave(p, file = "out/mod_controls.png",
       height = 4,
       width = 6.2)
