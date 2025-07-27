# remove geometry to make descriptions look cleaner 
coops_desc <- coops_map_dem %>%
  st_drop_geometry()

# graphs for pblk 
his_blk <- coops_desc %>%
  ggplot(aes(x = pblk)) +
  geom_histogram() +
  facet_wrap(~matched)

box_blk <-  coops_desc %>%
  ggplot(aes(x = pblk)) +
  geom_boxplot() +
  facet_wrap(~matched)

density_blk <- coops_desc %>%
  ggplot(aes(x = pblk)) +
  geom_density() +
  facet_wrap(~matched)
# graphs for pwht 
his_wht <- coops_desc %>%
  ggplot(aes(x = pwht)) +
  geom_histogram() +
  facet_wrap(~matched)

box_wht <-  coops_desc %>%
  ggplot(aes(x = pwht)) +
  geom_boxplot() +
  facet_wrap(~matched)

density_wht <- coops_desc %>%
  ggplot(aes(x = pwht)) +
  geom_density() +
  facet_wrap(~matched)

# descriptives 
desc <- coops_desc %>%
  group_by(matched) %>%
  summarize(mhinc = mean(hinc, na.rm = TRUE),
            sdhinc = sd(hinc, na.rm = TRUE),
            mpblk = mean(pblk, na.rm = TRUE),
            sdpblk = sd(pblk, na.rm = TRUE))

desc 
