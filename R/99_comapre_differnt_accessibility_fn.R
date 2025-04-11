
# Compare measures considering local match in local araes


# Load packages
library(sf)
library(tidyverse)

# Grid names
grids <- c("grid_1000", "grid_2000", "grid_4000", "grid_500",  "postcodes")

# OLD Accessibility estimate for public transport
access_pt_old <- 
  list.files("data/accessibility_r5r/", full.names = TRUE, pattern = "pt") %>%
  map(data.table::fread) %>%
  bind_rows()

# NEW Accessibility estimate for public transport
access_pt_new <- 
  list.files("data/accessibility_r5r2/", full.names = TRUE, pattern = "pt") %>%
  map(data.table::fread) %>%
  bind_rows()

# Merge data
access_pt <- list(access_pt_old, access_pt_new) %>% 
  set_names(c('old', 'new')) %>% 
  bind_rows(.id = 'version_access')

# Keep matching measure only
access_pt <- access_pt %>% 
  filter(opportunity == 'k')

# Descriptive comparison --------------------------------------------------

# Difference of units with accessibility data
access_pt %>% 
  count(version_access, grid) %>% 
  pivot_wider(names_from = 'version_access', values_from = n) %>% 
  mutate(difference = old - new)

# Distribution after-before
access_pt %>% 
  group_by(grid, version_access) %>% 
  summarise(
    observations = n(),
    min = min(accessibility),
    max = max(accessibility),
    mean = mean(accessibility),
    median = median(accessibility)
  )
# 
# access_pt %>% 
#   pivot_wider(names_from = version_access, values_from = accessibility) %>% 
#   mutate(
#     access_diff = old - new,
#     missing = is.na(new)
#   ) %>% 
#   view()
#    

# Plot distribution
access_pt %>% 
  mutate(accessibility = accessibility^(1 / 3)) %>% 
  ggplot(aes(accessibility, col = version_access)) +
  geom_density() +
  facet_grid(~grid)

