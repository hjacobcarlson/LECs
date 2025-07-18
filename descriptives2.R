# creating a dataset with both matched and unmatched coops with coop demographics #### 
coopsrnt_dem <- bind_rows(matched_2020,unmatched_2020)



coops <-coopsrnt_dem %>%
  mutate(matched = !is.na(Neighborhood))



p <- coops %>%
  ggplot(aes(x = pblk)) +
  geom_histogram() +
  facet_wrap(~matched)
p

desc <- coops %>%
  group_by(matched) %>%
  summarize(mhinc = mean(hinc, na.rm = TRUE),
            sdhinc = sd(hinc, na.rm = TRUE),
            mpblk = mean(pblk, na.rm = TRUE),
            sdpblk = sd(pblk, na.rm = TRUE))

desc
