

# Date: 24/03/2022
# Author: Rafael Verduzco

# This script corresponds to the analysis. Ch. 6, titled:
# In essence, it is an empirical review of PT accessibility in GMC.

# This code:
# 3. Develops the analysis with temporal focus of accessibility on the MPTN stages.



# Set environment ---------------------------------------------------------

# Load packages
library(sf)
library(ggspatial)
library(ggstar)
library(ggrepel)
library(tidyverse)
library(spdep)

# Read data and preliminary transform -------------------------------------

# Grid names
grids <- c("grid_1000", "grid_2000", "grid_4000", "grid_500",  "postcodes")

# Accessibility estimate for public transport
access_pt <- 
  list.files("data/accessibility_r5r2/", full.names = TRUE, pattern = "pt") %>%
  map(data.table::fread) %>%
  bind_rows()

# Accessibility estimate for car
access_car <- 
  list.files("data/accessibility_r5r2/", full.names = TRUE, pattern = "car") %>%
  map(data.table::fread) %>%
  setNames(grids) %>% 
  bind_rows(.id = 'grid')

# Read grid geometries
geom_paths <- c(
  list.files("data/Spatial grids", full.names = T),
  list.files("data/Post codes", full.names = T, pattern = ".gpkg")
)
grid_geoms <- geom_paths %>% 
  map(st_read) %>%
  map(mutate, id = as.numeric(id)) %>%
  setNames(grids)

# Read OD points
od_points_dirs <- list.files("data/od_points/", full.names = TRUE)
od_points <- lapply(od_points_dirs, read_csv)
names(od_points) <- grids
# Bind in a single DF
od_points_all <- od_points %>% 
  bind_rows(.id = 'grid')

# Read MPTN geometries
mptn <- readRDS("data/transit_network/transit_network.rds")

# State boundaries
states <- 
  st_read("data/marco_geoestadistico_nacional_2014_version_6.2/CONTINUO_NACIONAL/ESTADOS.shp")

# Read population census data (origin)
census <- list.files("data/scince_2012/", full.names = T) %>% 
  lapply(data.table::fread) %>%
  set_names(grids) %>% 
  bind_rows(.id = 'grid')

# Edu labs
edu_labs <- c('1 - Least \neducated', 2:4, '5 - Most \neducated')
# Compute education quintile
census <- census %>% 
  group_by(grid) %>% 
  mutate(
    edu_quintile = ntile(edu49_r, 5),
    edu_quintile = factor(edu_quintile, labels = edu_labs),
    edu_quintile_w = gtools::quantcut(edu49_r, 5, weights = pob1, labels = FALSE),
    edu_quintile_w = factor(edu_quintile_w, labels = edu_labs),
  ) %>% 
  ungroup()


# Labels and plot features ------------------------------------------------

# Grid labels
grid_labs <- 
  c(
    "Grid 0.5 km" = "grid_500", 
    "Grid 1 km" = "grid_1000", 
    "Grid 2 km" = "grid_2000", 
    "Grid 4 km" = "grid_4000", 
    "Post code" = "postcodes"
  )
# Stages
stage_labs <- c(
  "Before 2010" = "before_2010", 
  "2010 to 2012" = "2010_2012", 
  "2012 to 2013" = "2012_2013", 
  "2013 to 2015" = "2013_2015",
  "2015 to 2016" = "2015_2016", 
  "2016 to 2018" = "2016_2018", 
  "2018 to 2019" = "2018_2019"
)

# Stages labs 2
stage_labs2 <- paste0('Stage ', 1:7, '\n', '(', names(stage_labs), ')')

# Zocalo
zocalo <- data.frame(y = 19.432413, x = -99.132938, name = "CBD")


# Theme faceted map
theme_fmap <-
  theme_void() +
  theme(
    plot.title = element_text(size = 11.5),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8.5, margin = margin(t = 15, b = 5)),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.height = unit(.225, "cm"),
    legend.key.width = unit(0.9, "cm"),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.spacing.x = unit(0, "cm"),
    legend.margin = margin(0.1, 0.5, .1, 0.5, unit = "cm"),
    strip.text.y.left = element_text(angle = 90, margin = margin(l = 0, r = 1)),
    strip.text.x = element_text(margin = margin(t = 10, b = 5))
  )



# Format accessibility ----------------------------------------------------


# Limit PT accessibility  to exponential and bind in a single DF
access_pt <- access_pt %>%
  filter(dec_fn == "exp" & beta != 0.031) 

# Keep records with both all and matching access
access_pt <- access_pt %>%
  group_by(from_id, grid, router) %>%
  filter(n_distinct(opportunity) > 1) %>% 
  ungroup()

# Bind geometries in a single DF
grid_geoms <- grid_geoms %>% 
  bind_rows(.id = "grid")



## ----------------------------------------------------------------
##                 Exploratory - Temporal focus                 --
## ----------------------------------------------------------------

# Plot longitudinal access ------------------------------------------------

# Select 5 km grid and join census data
access_pt_500 <- access_pt %>% 
  filter(grid == 'grid_500') %>% 
  group_by(from_id) %>%
  filter(n_distinct(opportunity) > 1) %>% 
  ungroup() %>% 
  left_join(census, by = c('grid', 'from_id' = 'id'))

library(data.table)
setDT(access_pt_500)

# Calculate mean accessibility and weighted mean accessibility
longitudinal_relative <- access_pt_500[, .(
    accessibility = mean(accessibility, na.rm = TRUE),
    accessibility_w = weighted.mean(accessibility, w = pob1, na.rm = TRUE)
  ), by = .(opportunity, router)]

# Estimate relative change over time using Stage 1 as reference
longitudinal_relative <- longitudinal_relative %>% 
  mutate(
    router = factor(router, stage_labs, names(stage_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching")),
    router = factor(router, labels = stage_labs2)
  ) %>%
  arrange(opportunity, router) %>%
  group_by(opportunity) %>%
  mutate(
    rel_change = (accessibility_w / first(accessibility_w)) - 1,
    rel_change = replace_na(rel_change, 0) * 100
  ) %>% 
  ungroup()


# Relative change 0.5 km grid only
plot_longitudinal_relative_500 <- longitudinal_relative %>%
  ggplot(aes(router, rel_change, group = opportunity, col = opportunity)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point() +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(
    x = "MPTN temporal stage",
    y = "Mean change (%, Ref. 'Before 2010')",
    col = "Accessibility measure"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(hjust = 0, size = 9)
  )
plot_longitudinal_relative_500

# Save plot
ggsave(
  filename =  "output/plots_maps/exploratory/plot_longitudinal_relative_500.png", 
  plot = plot_longitudinal_relative_500,
  height = 4, 
  width = 6, 
  dpi = 400, 
  bg = "white"
)


# MPTN layers ----------------------------------------------------------

# Evolution of MPTN
mptn_evo <-
  lapply(2:7, function(x) {
    mptn[[1]][[x]] %>%
      filter(!route_id %in% mptn[[1]][[x - 1]]$route_id)
  }) %>%
  setNames(names(stage_labs)[-1]) %>%
  bind_rows(.id = "router") %>%
  bind_rows(., .) %>%
  mutate(
    opportunity = c(rep("All", nrow(.) / 2), rep("Matching", nrow(.) / 2))
  ) %>%
  st_as_sf()

# Cumulative evo of network
mptn_cum <-
  lapply(2:7, function(x) {
    mptn[[1]][[x]] %>%
      filter(!route_id %in% mptn[[1]][[1]]$route_id)
  }) %>%
  setNames(names(stage_labs)[-1]) %>%
  bind_rows(.id = "router") %>%
  bind_rows(., .) %>%
  mutate(
    opportunity = c(rep("All", nrow(.) / 2), rep("Matching", nrow(.) / 2))
  ) %>%
  st_as_sf()

# Read annotations
temp_annotation <- read_csv("data/labels_stage.csv")
temp_annotation <- temp_annotation[-6:-8, ]
temp_annotation <- bind_rows(list(All = temp_annotation, Matching = temp_annotation), .id = "opportunity")
temp_annotation_north <- filter(temp_annotation, lat > zocalo$y)
temp_annotation_south <- filter(temp_annotation, lat < zocalo$y)


# Map change of access measures over temporal stages -----------------

# Compute change
access_change <- access_pt %>% 
  filter(grid == 'grid_500') %>% 
  # pivot_wider(names_from = opportunity, values_from = accessibility) %>% 
  mutate(
    router = factor(router, stage_labs, names(stage_labs))
  ) %>%
  arrange(from_id, opportunity, router) %>% 
  group_by(from_id, opportunity) %>%
  mutate(
    change_access = accessibility / lag(accessibility),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
  ) %>%
  ungroup() 

# Map relative change
map_change_access <- access_change %>%
  filter(router != 'Before 2010') %>% 
  group_by(router) %>% 
  mutate(
    change_access = if_else(
      change_access > quantile(change_access, 0.995, na.rm = TRUE), 
      quantile(change_access, 0.995, na.rm = TRUE),
      change_access
    )
  ) %>% 
  # filter(change_access > 1) %>% 
  left_join(grid_geoms, by = c('grid', "from_id" = "id")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = change_access), col = NA, alpha = 1) +
  geom_sf(data = states, fill = NA, lwd = 0.3, col = "White", alpha = 0.75) +
  # geom_sf(data = mptn_cum, aes(col = "MPTN ext."), lwd = 0.18, alpha = 0.6) +
  geom_sf(
    data = mptn_evo,
    aes(col = "Previous \nMPTN ext."),
    lwd = 0.18, alpha = 0.7
  ) +
  geom_star(
    data = zocalo,
    aes(x, y, starshape = "CBD (Zócalo)"),
    fill = "white", col = "black", size = 1.5, alpha = 0.9
  ) +
  scale_starshape(name = NULL) +
  coord_sf(xlim = c(-98.91, -99.26), ylim = c(19.24, 19.7)) +
  facet_grid(opportunity ~ router, switch = "y") +
  scale_fill_viridis_c(
    n.breaks = 10, option = "turbo", begin = 0.05, end = 0.95
  ) +
  scale_color_manual(values = c('black'), name = NULL) +
  labs(
    fill = 'Relative change\n(Prev. stage over current)'
  ) +
  theme_fmap

# Annotations
map_change_access_annotated <-map_change_access +
  geom_label_repel(
    aes(lon, lat, label = name),
    data = temp_annotation,
    nudge_x = 0.06, nudge_y = 0.03, label.padding = 0.1,
    segment.size = 0.35, segment.color = "gray40",
    size = 1.6, alpha = 0.75
  )


# Save map
ggsave(
  filename = "output/plots_maps/exploratory/map_change_access.png",
  plot = map_change_access_annotated,
  height = 6, 
  width = 11, 
  dpi = 700, 
  bg = "white"
)


# Equity analysis ---------------------------------------------------------

# Plot distribution denisty car access
access_car %>%
  filter(grid == 'grid_500') %>%
  filter(n_distinct(opportunity) > 1) %>% 
  ggplot(aes(accessibility, col = opportunity)) +
  geom_density(adjust = 2) 

# Calculate accessibility lower threshold
car_thresholds <- access_car %>%
  filter(grid == 'grid_500') %>% 
  group_by(from_id) %>%
  filter(n_distinct(opportunity) > 1) %>% 
  ungroup() %>% 
  left_join(census, by = c('grid', 'from_id' = 'id')) %>% 
  mutate(
    pob1 = replace_na(pob1, 1),
    pob1 = ifelse(pob1 == 0, 1, pob1),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
    ) %>% 
  filter(!is.na(accessibility)) %>% 
  group_by(opportunity) %>% 
  summarise(
    access_car10 = DescTools::Quantile(
      x = accessibility, 
      weights = pob1,
      probs = 0.10
    ), 
    access_car15 = DescTools::Quantile(
      x = accessibility, 
      weights = pob1,
      probs = 0.15
    ), 
    access_car20 = DescTools::Quantile(
      x = accessibility, 
      weights = pob1,
      probs = 0.2
    ),
    access_car30 = DescTools::Quantile(
      x = accessibility, 
      weights = pob1,
      probs = 0.3
    )
  )
car_thresholds

# Join census data
access_change_census <- access_change %>% 
  left_join(census, by = c('grid', 'from_id' = 'id')) %>% 
  left_join(car_thresholds, by = c('opportunity'))

tot_pop <- 19573149

threshold_labs <- paste0('Under P', c(10, 15, 20, 30))
stage_labs3 <- paste0('S', 1:7, ': ', names(stage_labs))

# Quick changes summary
equity_overtime <- access_change_census %>% 
  group_by(router, opportunity) %>% 
  summarise(
    # count = sum(change_access > 1, na.rm = TRUE),
    # mean_change = mean(change_access, na.rm = TRUE),
    pop_under10 = sum(if_else(accessibility < access_car10, pob1, 0), na.rm = TRUE),
    pop_under15 = sum(if_else(accessibility < access_car15, pob1, 0), na.rm = TRUE),
    pop_under20 = sum(if_else(accessibility < access_car20, pob1, 0), na.rm = TRUE),
    pop_under30 = sum(if_else(accessibility < access_car30, pob1, 0), na.rm = TRUE),
  ) %>% 
  ungroup() %>% 
  mutate(across(starts_with('pop'), \(x) x / tot_pop)) %>% 
  pivot_longer(starts_with('pop_un')) %>% 
  mutate(
    name = factor(name, labels = threshold_labs),
    router = factor(router, labels = stage_labs3)
  ) %>% 
  ggplot(aes(router, value, group = opportunity, col = opportunity)) +
  geom_line(lwd = 1.1) +
  facet_wrap(~name) +
  theme_minimal() +
  scale_color_viridis_d(option = 'turbo', begin = 0.1, end = 0.9) +
  labs(
    x = 'MPTN stage',
    y = 'Head count ratio',
    col = 'Accessibility\nmeasure'
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
 
# Save plot
ggsave(
  filename =  "output/plots_maps/exploratory/equity_overtime.png", 
  plot = equity_overtime,
  height = 4, 
  width = 6, 
  dpi = 400, 
  bg = "white"
)


# Equity by education ntile -----------------------------------------------


# Equity summary by education quintile
equity_summary <- access_change_census %>% 
  group_by(router, opportunity, edu_quintile) %>% 
  summarise(
    pop_under10 = sum(if_else(accessibility < access_car10, pob1, 0), na.rm = TRUE),
    pop_under15 = sum(if_else(accessibility < access_car15, pob1, 0), na.rm = TRUE),
    pop_under20 = sum(if_else(accessibility < access_car20, pob1, 0), na.rm = TRUE),
    pop_under30 = sum(if_else(accessibility < access_car30, pob1, 0), na.rm = TRUE),
  ) %>% 
  group_by(opportunity, edu_quintile) %>% 
  mutate(
    pop_benefited10 = lag(pop_under10) - pop_under10,
    pop_benefited15 = lag(pop_under15) - pop_under15,
    pop_benefited20 = lag(pop_under20) - pop_under20,
    pop_benefited30 = lag(pop_under30) - pop_under30
  ) %>% 
  ungroup()


# Plot population taken out of lower threshold
equity_barplot <- equity_summary %>% 
  filter(router != 'Before 2010' & !is.na(edu_quintile)) %>% 
  select(router, opportunity, edu_quintile, starts_with('pop_benefited')) %>% 
  pivot_longer(starts_with('pop_ben')) %>% 
  mutate(
    edu_quintile = factor(edu_quintile, labels = 1:5),
    name = factor(name, labels = paste0('P', c(10, 15, 20, 30))),
    router = factor(router, labels = stage_labs2[-1]),
    value = value / 1e3
  ) %>%
  ggplot(aes(edu_quintile, value, fill = opportunity)) +
  geom_col(position = 'dodge') +
  geom_smooth(
    aes(x = as.numeric(factor(edu_quintile)), group = opportunity),
    method = lm, formula = y ~ splines::bs(x, 3), se = FALSE,
    lwd = 0.85, show.legend = FALSE, col = 'white'
  ) +
  geom_smooth(
    aes(x = as.numeric(factor(edu_quintile)), col = opportunity),
    method = lm, formula = y ~ splines::bs(x, 3), se = FALSE,
    lwd = 0.6, show.legend = FALSE
  ) +
  facet_grid(name ~ router) +
  theme_minimal() +
  scale_fill_viridis_d(option = 'turbo', begin = 0.1, end = 0.9, alpha = 0.75) +
  scale_color_viridis_d(option = 'turbo', begin = 0.1, end = 0.9, alpha = 1) +
  labs(
    y= 'Population (in thousands)',
    x = "Education quintile (1 - Least educated, 5 - Most educated)",
    fill = 'Accessibility measure',
    col = 'Fitted GAM'
  ) +
  theme(legend.position = 'bottom')

# Save plot
ggsave(
  filename =  "output/plots_maps/exploratory/equity_barplot.png", 
  plot = equity_barplot,
  height = 5, 
  width = 8, 
  dpi = 400, 
  bg = "white"
)


# Gini coef ---------------------------------------------------------------


gini_lineplot <- access_change_census %>% 
  group_by(opportunity, router) %>% 
  summarise(
    gini = ineq::Gini(accessibility)
  ) %>% 
  ggplot(aes(router, gini, group = opportunity, col = opportunity)) +
  scale_y_continuous(limits = c(0.75, 0.9)) +
  geom_line(lwd = 1.1) +
  geom_point() +
  scale_color_viridis_d(option = 'turbo', begin = 0.1, end = 0.9) +
  labs(
    y= 'Gini coefficient',
    x = "MPTN Stage",
    col = 'Accessibility \nmeasure'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1))
# Save plot
ggsave(
  filename =  "output/plots_maps/exploratory/gini_lineplot.png", 
  plot = gini_lineplot,
  height = 4, 
  width = 7, 
  dpi = 400, 
  bg = "white"
)
  

