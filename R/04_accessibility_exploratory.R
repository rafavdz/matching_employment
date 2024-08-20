###########################################################################
###########################################################################
###                                                                     ###
###                                 RQ1                                 ###
###                      ACCESSIBILITY EXPLORATORY                      ###
###                                                                     ###
###########################################################################
###########################################################################

# Date: 24/03/2022
# Author: Rafael Verduzco

# This script corresponds to the analysis. Ch. 6, titled:
# In essence, it is an empirical review of PT accessibility in GMC.

# This code:
# 1. Generates accessibility descriptive statistics
# 2. A visual exploratory analysis with spatial focus of access, using verious SAS
# 3. A visual explorator analysis with temporal focus of access.


# Set environment ---------------------------------------------------------

# Load packages
library(sf)
library(ggspatial)
library(ggstar)
library(ggrepel)
library(data.table)
setDTthreads(0) # Max threads
library(tidyverse)
library(patchwork)


# Read data and preliminary transform -------------------------------------


# Grid names
grids <- c("grid_1000", "grid_2000", "grid_4000", "grid_500",  "postcodes")
# Accessibility estimate for public transport
access_pt <- 
  list.files("data/accessibility/", full.names = TRUE, pattern = "pt") %>%
  map(fread) %>%
  setNames(grids)
# Accessibility estimate for car
access_car <- 
  list.files("data/accessibility/", full.names = TRUE, pattern = "car") %>%
  map(fread) %>%
  setNames(grids)

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

# Define routers (temporal stages) for public transport
routers <- unique(access_pt$postcodes$router)
router_labels <- str_to_title(gsub("_", " ", routers))

# Access parameters
access_pars <- read_csv("data/accessibility/access_pars.csv")

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
access_cube <- expression(sqrt(Accessibility, 3))

# Zocalo
zocalo <- data.frame(y = 19.432413, x = -99.132938, name = "CBD")
cbd_star <- 
  geom_star(
    data = zocalo, 
    aes(x, y, starshape = "CBD (Zócalo)"), 
    fill = "white", col = "black", 
    size = 1.3, alpha = 0.9
  )

# Metro area bounding box
metro_bbox <- st_bbox(grid_geoms[[5]])

# State boundaries
states <- 
  st_read("data/marco_geoestadistico_nacional_2014_version_6.2/CONTINUO_NACIONAL/ESTADOS.shp")

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

# Fn format big number
format2 <- 
  function(x, digits = 1) {
    formatC(x, format = "f", big.mark = " ", digits = digits)
  }
    
# Function to extract the cube root
cube_root <- 
  function(x){
    trans_new("cuberoot", transform = function(x) x^(1 / 3), inverse = function(x) x^3)
  }
   

# Create dir to save plots and maps
dir.create("output/exploratory")
# Directory for other results
dir.create("output//exploratory_rq1")


## ---------------------------------------------------------------
##                      Descriptive stats                      --
## ---------------------------------------------------------------

# Descriptive statistics --------------------------------------------------

# Table, horizontal version

# Car
desc_stats_car2 <- access_car %>%
  rbindlist(idcol = "grid") %>%
  group_by(grid) %>%
  summarise(across(accessibility, list(
    q1 = ~ quantile(.x, .25),
    mean = mean,
    sd = sd,
    q3 = ~ quantile(.x, .75)
  ))) %>%
  mutate(
    grid = factor(grid, grid_labs, names(grid_labs)),
    across(starts_with("access"), \(x) format2(x / 1e3, 1)),
    access_sum = 
      paste0(
        accessibility_mean, 
        " (", accessibility_sd, ")\n", 
        "[", accessibility_q1, ", ", accessibility_q3, "]"
    )
  ) %>%
  select(grid, access_sum) %>%
  arrange(grid) %>%
  pivot_wider(names_from = grid, values_from = access_sum) %>%
  mutate(opportunity = "Car")
# PT
desc_stats2 <- access_pt %>%
  rbindlist(idcol = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  group_by(opportunity, grid, router) %>%
  summarise(
    across(accessibility, list(
      q1 = ~ quantile(.x, .25),
      mean = mean,
      sd = sd,
      q3 = ~ quantile(.x, .75)
    ))
  ) %>%
  mutate(
    across(starts_with("access"), ~ format2(.x / 1e3, 1)),
    access_sum = paste0(
      accessibility_mean, 
      " (", accessibility_sd, ")\n", 
      "[", accessibility_q1, ", ", accessibility_q3, "]"
    )
  ) %>%
  select(opportunity:router, access_sum) %>%
  mutate(
    router = factor(router, stage_labs, names(stage_labs)),
    grid = factor(grid, grid_labs, names(grid_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
  ) %>%
  arrange(opportunity, grid, router) %>%
  pivot_wider(names_from = grid, values_from = access_sum) %>%
  bind_rows(desc_stats_car2)

# !! INCLUDE NUMBER OF MISSING IN MATCHING

# N. observations below SAS name
observations <- format2(sapply(access_pt, function(x) n_distinct(x$origin)), 0)
col_names <- paste0(names(grid_labs), " (N=", observations[c(4, 1:3, 5)], ")")
names(desc_stats2) <- c("Type", "Stage", col_names)
desc_stats2

# Save Table: desc. stats
write_csv(desc_stats2, "output//exploratory_rq1/desc_stats2.csv")


# Boxplot accessibility distribution --------------------------------------



# Distribution in boxplot
dodge <- position_dodge(width = 0.65)
access_dist_boxplot <- access_pt %>%
  rbindlist(idcol = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  mutate(
    router = factor(router, stage_labs, names(stage_labs)),
    grid = factor(grid, grid_labs, names(grid_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching")),
    accessibility = accessibility^(1 / 3)
  ) %>%
  ggplot(aes(accessibility, router, fill = opportunity)) +
  geom_violin(position = dodge, alpha = 0.5, col = NA) + # col = "gray30", size = 0.4
  geom_boxplot(
    position = dodge, alpha = 0.5, size = 0.3,
    outlier.shape = NA, width = 0.2
  ) +
  stat_summary(fun = "mean", position = dodge, size = 0.1, alpha = 0.9) +
  facet_wrap(~grid) +
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
  labs(
    x = access_cube,
    y = "Temporal stage",
    fill = "Type of measure"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.825, .2),
    legend.title = element_text(size = 9),
    legend.key.height = unit(.3, "cm"),
    legend.key.width = unit(1.5, "cm"),
  )
# Save plot
ggsave(
  filename = "output/plots_maps//exploratory/access_dist_boxplot.png", 
  access_dist_boxplot,
  height = 5, 
  width = 8, 
  dpi = 600, 
  bg = "white"
)


## ---------------------------------------------------------------
##                 Exploratory - Spatial focus                 --
## ---------------------------------------------------------------

# Map accessibility over various grids ------------------------------------


## Map PT accessibility over various SAS
# Summarise accessibility by median
# Transform data for Map
access_pt2 <- access_pt %>%
  map(filter, dec_fn == "exp" & beta != 0.031) %>%
  map(group_by, origin, opportunity) %>%
  map(summarise,
    accessibility_med = median(accessibility, na.rm = TRUE),
    accessibility_mean = mean(accessibility, na.rm = TRUE),
    accessibility_sd = sd(accessibility, na.rm = TRUE),
    accessibility_sum = sum(accessibility, na.rm = TRUE),
  ) %>%
  map(group_by, opportunity) %>%
  map(mutate, access_ntile = factor(ntile(accessibility_med, 10))) %>%
  map2(., grid_geoms, left_join, by = c("origin" = "id")) %>%
  rbindlist(idcol = "grid") %>%
  mutate(
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching")),
    grid = factor(grid, grid_labs, names(grid_labs))
  ) %>%
  st_as_sf()

# Legend label
map1_legend_labs <- 
  c("Low (1)", rep("", 8), "High (10)")
map1_title <- 
  "Accessibility to employment by public transport. Greater Mexico City, 2010-2019."
map1_subtitle <- 
  "Transport modes include those comprising the main public transport network and walking. Accessibility is computed as the median of the period of study."
map1_fill <- 
  "Accessibility \ndecile"
map1_caption <- 
  "Source: the author based on own calculations, Censos Economicos 2014 (Inegi, n.d.), Censo de Población y Vivienda 2010 (Inegi, n.d)."
# Annotations
map1_annotation <- read_csv("data/map_annotation_ch5.csv")
map1_annotation_w <- filter(map1_annotation, x > zocalo$x)
map1_annotation_e <- filter(map1_annotation, x < zocalo$x)

# Plot median accessibility by SAS
access_map1 <- access_pt2 %>%
  ggplot() +
  geom_sf(aes(fill = access_ntile), col = NA) +
  geom_sf(data = states, fill = NA, size = 0.25, col = "White") +
  geom_star(
    data = zocalo, aes(x, y, starshape = "CBD (Zócalo)"), 
    fill = "white", col = "black", size = 1.5, alpha = 0.9
  ) +
  coord_sf(
    xlim = c(metro_bbox[1], metro_bbox[3]), 
    ylim = c(metro_bbox[2], metro_bbox[4])
  ) +
  facet_grid(opportunity ~ grid, switch = "y") +
  scale_starshape(name = NULL) +
  scale_fill_viridis_d(option = "turbo", begin = 0.05, labels = map1_legend_labs) +
  labs(
    title = map1_title,
    subtitle = map1_subtitle,
    fill = map1_fill,
    # caption = map1_caption
  ) +
  guides(
    fill = guide_legend(label.position = "bottom", title.vjust = 1, nrow = 1)
  ) +
  theme_fmap +
  theme(
    panel.spacing.x = unit(0, "cm")
  ) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.3, "cm"), width = unit(0.5, "cm"),
    style = north_arrow_orienteering(text_size = 4),
    data = tibble(opportunity = c("All"), grid = "Post code")
  ) +
  annotation_scale(
    location = "bl", height = unit(0.1, "cm"), line_width = 0.75,
    style = "ticks", text_cex = 0.5,
    data = tibble(opportunity = c("Matching"), grid = "Grid 0.5 km")
  )

# Add annotations
access_map1_l <- access_map1 +
  geom_point(
    data = map1_annotation, aes(x, y),
    size = 0.2, col = "gray15"
  ) +
  geom_label_repel(
    data = map1_annotation_w,
    aes(x, y, label = name),
    min.segment.length = 0,
    label.padding = 0.1,
    nudge_x = 0.1, force = 5, xlim = -98.85,
    segment.size = 0.35, segment.color = "gray30",
    size = 1.4, alpha = 0.65
  ) +
  geom_label_repel(
    data = map1_annotation_e,
    aes(x, y, label = name),
    min.segment.length = 0,
    label.padding = 0.1,
    force = 5,
    xlim = c(-99.55, -99.45),
    segment.size = 0.35, segment.color = "gray30",
    size = 1.4, alpha = 0.65
  )


# Save map
ggsave(
  filename = "output/plots_maps//exploratory/access_map_grid.png", 
  access_map1_l,
  height = 6, 
  width = 11, 
  dpi = 600, 
  bg = "white"
)

# library(cowplot)
# # Zoom limits
# m1_xlim <- c(-99.23, -98.97)
# m1_ylim <- c(19.45, 19.7)
#
# # Include inset showing a zoom of SUB-L1 and METRO-LB for the grid .5 km
# map1_inset <- access_pt2 %>%
#   filter(grid == "Grid 0.5 km" & opportunity == "All") %>%
#   ggplot() +
#   geom_sf(aes(fill = access_ntile), col = NA) +
#   scale_fill_viridis_d(option = "turbo", begin = 0.05, labels = map1_legend_labs) +
#   geom_star(data = zocalo, aes(x, y, starshape = 'CBD (Zócalo)'), fill = 'white', col = 'black', size = 2, alpha = 0.9) +
#   coord_sf(xlim = m1_xlim, m1_ylim) +
#   theme_void() +
#   theme(
#     legend.position = "none"
#   )
# # Merge maps
# gg_inset_map1 = ggdraw() +
#   draw_plot(access_map1) +
#   draw_plot(map1_inset, x = -.01, y = 0.45, width = 0.12, height = 0.12)
# # Save map
# ggsave('05_Plots and maps/exploratory/access_map_grid.png', gg_inset_map1,
#        height = 6, width = 10, dpi = 600, bg = 'white')

# Spatial correlation coefficient -----------------------------------------

library(spdep)

# Maps using matching measures display more heterogeneous patters than maps using all
# We can confirm using a
# Spatial correlation. Compare heterogeneity of different access measures

# Read OD points (representing SAS's centroids)
od_points <- 
  lapply(list.files("data/od_points/", full.names = TRUE), read_csv)
# K-nearest neigh.
knn <- 6

# Run Moran's I test
moran_res <-
  lapply(seq_along(access_pt), function(i) {
    # Filter zones included in the dataset
    zone_id <- unique(access_pt[[i]]$origin)
    od_points_s <- od_points[[i]][od_points[[i]]$GEOID %in% zone_id, ]
    od_points_s <- od_points_s[order(od_points_s$GEOID), ]
    pc_coords <- as.matrix(od_points_s[, c("X", "Y")])
    # Compute k nearest nb at post code level
    knearnb_pc <- 
      knn2nb(
        knearneigh(pc_coords, knn, longlat = TRUE), 
        row.names = od_points_s$GEOID
      )
    knearnb_pc <- make.sym.nb(knearnb_pc)
    # Store results in list
    W_list <- nb2listw(knearnb_pc, style = "B")
    # Moran's I
    lapply(c("all", "k"), function(o) {
      morans_i <- access_pt[[i]] %>%
        group_by(opportunity, origin) %>%
        summarise(accessibility = median(accessibility, na.rm = TRUE)) %>%
        pivot_wider(names_from = opportunity, values_from = accessibility) %>%
        arrange(origin) %>%
        pull({{ o }}) %>%
        moran.mc(., W_list, 3000, na.action = na.omit)
    })
  })

# There is a warning because some points are identical.
# This happens because in some cases the centroid is snapped to the nearest vertex in the network.
# If the road network is not dense, the nearest vertex is the same for more than one point.

# Check number of duplicated OD points
sapply(seq_along(access_pt), function(i) {
  # Filter zones in included in the dataset
  zone_id <- unique(access_pt[[i]]$origin)
  od_points_s <- od_points[[i]][od_points[[i]]$GEOID %in% zone_id, ]
  od_points_s <- od_points_s[order(od_points_s$GEOID), ]
  nrow(od_points_s[, -1]) - n_distinct(od_points_s[, -1])
})

# Compute summary table
moran_summary <- t(
  sapply(
    flatten(moran_res), function(x) c(x$statistic, p_value = x$p.value)
  )
)
moran_summary <-
  data.frame(
    grid = c(rbind(grids, grids)),
    type = rep(c("All", "Matching"), 5),
    moran_summary
  )
# Format table
moran_summary <- moran_summary %>%
  mutate(grid = factor(grid, grid_labs, names(grid_labs))) %>%
  arrange(grid)
# Print summary
moran_summary

# Save results
write_csv(moran_summary, "output/exploratory_rq1/moran_summary.csv")

# Correlation surface area vs accessibility -------------------------------

# Post code aggregates zones in the periphery which tend to be larger.
# These also have lower accessibility. For this reason, the average is higher for post code SAS

# Correlation surface area vs access by grid (averages by stage)
corr_area_data <-
  map2(access_pt, grid_geoms, left_join, by = c("origin" = "id")) %>%
  rbindlist(idcol = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  st_as_sf() %>%
  mutate(
    area_sqkm = as.numeric(st_area(.) / 1e6),
    area_sqkm = log(area_sqkm),
    accessibility = accessibility^(1 / 3)
  ) %>%
  st_set_geometry(NULL)

# Scatter plot
corr_area_data %>%
  filter(grid == "postcodes") %>%
  mutate(accessibility = accessibility^3) %>%
  ggplot(aes(area_sqkm, accessibility)) +
  geom_point(shape = 1, alpha = 0.5) +
  facet_grid(~opportunity) +
  geom_smooth(method = "glm")

# Compute correlation coefficient
corr_area_access <- corr_area_data %>% 
  group_by(opportunity, grid) %>%
  summarise(
    cor_estimate = cor.test(accessibility, area_sqkm)$estimate,
    cor_tvalue = cor.test(accessibility, area_sqkm)$statistic,
    cor_pvalue = cor.test(accessibility, area_sqkm)$p.value,
  )
# Format table
corr_area_access <- corr_area_access %>%
  mutate(grid = factor(grid, grid_labs, names(grid_labs))) %>%
  arrange(opportunity, grid)
# Save table
write_csv(corr_area_access, "output//exploratory_rq1/corr_area_access.csv")


# Plot distribution of accessibility --------------------------------------


# Plot ecdf distribution
ecdf_plot <- access_pt %>%
  rbindlist(idcol = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  group_by(origin, opportunity, grid) %>%
  summarise(accessibility = median(accessibility)) %>%
  mutate(
    accessibility = accessibility^(1 / 3),
    grid = factor(grid, grid_labs, names(grid_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("a) All", "b) Matching"))
  ) %>%
  ggplot(aes(accessibility, col = grid)) +
  stat_ecdf(linewidth = 0.5, alpha = 0.8) +
  facet_wrap(~opportunity) +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  scale_y_continuous(labels = function(x) x * 100) +
  labs(
    x = access_cube,
    y = "Percent",
    col = "SAS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(hjust = 0)
  )
# Save plot
ggsave(
  filename = "output/plots_maps//exploratory/ecdf_plot.png", 
  plot = ecdf_plot,
  height = 4, 
  width = 8, 
  dpi = 400, 
  bg = "white"
)


# Plot distribution density
grid_density_plot <- access_pt %>%
  rbindlist(idcol = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  group_by(origin, opportunity, grid) %>%
  summarise(accessibility = median(accessibility)) %>%
  mutate(
    accessibility = accessibility^(1 / 3),
    grid = factor(grid, grid_labs, names(grid_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("a) All", "b) Matching"))
  ) %>%
  ggplot(aes(accessibility, col = grid)) +
  geom_density(adjust = 2, linewidth = 0.65, alpha = 0.8) +
  facet_wrap(~opportunity) +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  labs(
    x = access_cube,
    y = "Density",
    col = "SAS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.6, "cm"),
    strip.text = element_text(hjust = 0, size = 9)
  )
# Save plot
ggsave(
  "output/plots_maps//exploratory//grid_density_plot.png", 
  grid_density_plot,
  height = 4, 
  width = 8, 
  dpi = 400, 
  bg = "white"
)



# Show how observations shift according accessibility measure
access_pt$postcodes %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  group_by(origin, opportunity) %>%
  summarise(accessibility = median(accessibility)) %>%
  group_by(opportunity) %>%
  mutate(
    accessibility_ntile = ntile(accessibility, 5),
    accessibility = accessibility^(1 / 3),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
  ) %>%
  select(-accessibility) %>%
  pivot_wider(names_from = opportunity, values_from = accessibility_ntile) %>%
  group_by(All, Matching) %>%
  summarise(n = n()) %>%
  group_by(All) %>%
  mutate(prop = n / sum(n, na.rm = TRUE) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = Matching, values_from = prop)

# # alluvial plot
# library(ggalluvial)
# access_pt$postcodes %>%
#   filter(dec_fn == "exp" & beta != 0.031) %>%
#   group_by(origin, opportunity) %>%
#   summarise(accessibility = median(accessibility)) %>%
#   group_by(opportunity) %>%
#   mutate(
#     accessibility_ntile = ntile(accessibility, 5),
#     accessibility = accessibility^(1 / 3),
#     opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
#   ) %>%
#   select(-accessibility) %>%
#   pivot_wider(names_from = opportunity, values_from = accessibility_ntile) %>%
#   group_by(All, Matching) %>%
#   summarise(n = n()) %>%
#   drop_na() %>%
#   filter(All < 6) %>%
#   mutate(
#     All = factor(All, levels = 5:1),
#     Matching = factor(Matching, levels = 5:1)
#   ) %>%
#   ggplot(aes(y = n, axis1 = All, axis2 = Matching)) +
#   geom_alluvium(aes(fill = All), width = 1 / 12) +
#   geom_stratum(width = 1 / 12) +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
#   theme_void() +
#   theme(
#     legend.position = "bottom"
#   )





## ----------------------------------------------------------------
##                 Exploratory - Temporal focus                 --
## ----------------------------------------------------------------

# Plot longitudinal access ------------------------------------------------


# Relative accessibility change over time
plot_longitudinal_relative <- 
  access_pt %>%
  rbindlist(idcol = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  mutate(
    accessibility = accessibility^(1 / 3),
    router = factor(router, stage_labs, names(stage_labs)),
    grid = factor(grid, grid_labs, names(grid_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("a) All", "b) Matching"))
  ) %>%
  group_by(opportunity, grid, router) %>%
  summarise(accessibility = mean(accessibility, na.rm = TRUE)) %>%
  arrange(opportunity, grid, router) %>%
  group_by(opportunity, grid) %>%
  mutate(
    rel_change = (accessibility / first(accessibility)) - 1,
    rel_change = replace_na(rel_change, 0) * 100
  ) %>%
  ggplot(aes(router, rel_change, group = grid, col = grid)) +
  geom_line(size = 0.6, alpha = 0.8) +
  facet_wrap(~opportunity) +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  scale_y_continuous(breaks = seq(0, 8, 1.5)) +
  labs(
    x = "Temporal stage",
    y = "Relative change (%)",
    col = "SAS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(hjust = 0, size = 9)
  )
# Save plot
ggsave(
  filename =  "output/plots_maps/exploratory/plot_longitudinal_relative.png", 
  plot = plot_longitudinal_relative,
  height = 4, 
  width = 8, 
  dpi = 400, 
  bg = "white"
)



# Map variability of access measures over temporal stages -----------------

library(ggrepel)

# Read MPTN
mptn <- readRDS("data/transit_network/transit_network.rds")

# Evolution of MPTN
mptn_evo <-
  lapply(2:7, function(x) {
    mptn[[1]][[x]] %>%
      filter(!route_id %in% mptn[[1]][[x - 1]]$route_id)
  }) %>%
  setNames(names(stage_labs)[-1]) %>%
  rbindlist(idcol = "router") %>%
  bind_rows(., .) %>%
  mutate(
    opportunity = c(rep("All", nrow(.) / 2), rep("Matching", nrow(.) / 2))
  ) %>%
  st_as_sf()

mptn_cum <-
  lapply(2:7, function(x) {
    mptn[[1]][[x]] %>%
      filter(!route_id %in% mptn[[1]][[1]]$route_id)
  }) %>%
  setNames(names(stage_labs)[-1]) %>%
  rbindlist(idcol = "router") %>%
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


# Cumulative variance fn
cumvar <- function(x, sd = TRUE) {
  x <- x - x[sample.int(length(x), 1)] ## see Remark 2 below
  n <- seq_along(x)
  v <- (cumsum(x^2) - cumsum(x)^2 / n) / (n - 1)
  if (sd) v <- sqrt(v)
  v
}

# Estimate changes in accessibility
access_change <- access_pt %>%
  rbindlist(idcol = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  mutate(
    router = factor(router, stage_labs, names(stage_labs)),
    grid = factor(grid, grid_labs, names(grid_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
  ) %>%
  arrange(grid, origin, opportunity, router) %>%
  group_by(grid, origin, opportunity) %>%
  mutate(
    change_net = accessibility - lag(accessibility),
    change_rel = accessibility / lag(accessibility),
    change_sd = sd(accessibility, na.rm = TRUE),
    change_cumsum = cumsum(accessibility),
    change_cumsd = cumvar(accessibility),
    change_relcumsd = change_cumsd / mean(accessibility, na.rm = TRUE),
    change_first = accessibility - first(accessibility),
    change_first_r = accessibility / first(accessibility)
  ) %>%
  ungroup()


# Label
map2_legend_labs <- 
  c("Low (0)", rep("", 8), "High (27)")
map2_title <- 
  "Variability of accessibility to employment introduced by various extensions of the MPTN between 2010 and 2019"
map2_subtitle <- 
  "The degree of variability is measured as the standard deviation for each origin according to the estimates of each temporal stage. \nThe maps show the cumulative standard deviation over time as standarized by type of accessibility measure."
map2_fill <- 
  "Variability \n(standardized SD)  "
map2_caption <- 
  "Source: the author based on own calculations, Censos Economicos 2014 (Inegi, n.d.), Censo de Población y Vivienda 2010 (Inegi, n.d)."
# Col scale for routes
col_routes <- 
  c("gray60", "black")

# Map
map_change_access <- 
  access_change %>%
  filter(grid == "Grid 1 km") %>%
  filter(!is.na(change_cumsd)) %>%
  group_by(opportunity) %>%
  mutate(change_scaled = scale(change_cumsd)[, 1]) %>%
  left_join(grid_geoms$grid_1000, by = c("origin" = "id")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = change_scaled), col = NA, alpha = 1) +
  geom_sf(data = states, fill = NA, lwd = 0.3, col = "White", alpha = 0.75) +
  geom_sf(data = mptn_cum, aes(col = "MPTN ext."), lwd = 0.18, alpha = 0.6) +
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
  scale_fill_viridis_b(
    n.breaks = 10, option = "inferno", direction = -1, begin = 0.05, end = 0.95, 
    labels = map2_legend_labs
  ) +
  scale_color_manual(values = col_routes, name = NULL) +
  labs(
    title = map2_title,
    subtitle = map2_subtitle,
    fill = map2_fill,
    # caption = map2_caption
  ) +
  guides(
    fill = guide_legend(label.position = "bottom", title.vjust = 1, nrow = 1, order = 1),
    color = guide_legend(override.aes = list(size = 1.3, col = rev(col_routes)))
  ) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.3, "cm"), width = unit(0.5, "cm"),
    style = north_arrow_orienteering(text_size = 4),
    data = tibble(opportunity = c("All"), router = "2018 to 2019")
  ) +
  annotation_scale(
    location = "bl", height = unit(0.1, "cm"), line_width = 0.75,
    style = "ticks", text_cex = 0.5,
    data = tibble(opportunity = c("Matching"), router = "2010 to 2012")
  ) +
  theme_fmap

# Annotations
map_change_access_annotated <-
  map_change_access +
  geom_label_repel(
    aes(lon, lat, label = name),
    data = temp_annotation,
    nudge_x = 0.06, nudge_y = 0.03, label.padding = 0.1,
    segment.size = 0.35, segment.color = "gray40",
    size = 1.6, alpha = 0.6
  )

# Save map
ggsave(
  filename = "output/plots_maps/exploratory/map_change_access.png",
  plot = map_change_access_annotated,
  height = 6, 
  width = 11, 
  dpi = 600, 
  bg = "white"
)


# Gini index --------------------------------------------------------------

# Gini index
gini_temporal <- access_pt %>%
  rbindlist(idcol = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  group_by(opportunity, grid, router) %>%
  summarise(gini = ineq::Gini(accessibility)) %>%
  mutate(
    router = factor(router, stage_labs, names(stage_labs)),
    grid = factor(grid, grid_labs, names(grid_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
  ) %>%
  arrange(opportunity, grid, router) %>%
  pivot_wider(names_from = grid, values_from = gini) %>%
  rowwise() %>%
  mutate("Overall (mean)" = mean(`Grid 1 km`:`Post code`))
# Save Table: summary Gini
write_csv(gini_temporal, "output/exploratory_rq1/gini_temporal.csv")


# Plot Gini index
access_pt %>%
  rbindlist(idcol = 'grid') %>%
  filter( dec_fn == 'exp' & beta !=  0.031) %>%
  group_by(opportunity, grid, router) %>%
  summarise(gini = ineq::Gini(accessibility)) %>%
  mutate(
    router =  factor(router, stage_labs, names(stage_labs)),
    grid = factor(grid, grid_labs, names(grid_labs))
  ) %>%
  group_by(opportunity, router) %>%
  mutate(mean = mean(gini)) %>%
  ggplot(aes(x=router)) +
  geom_line(aes(y=gini, group = grid, col = grid), alpha = 0.8) +
  geom_line(aes(y=mean, group = 1), col = 'gray20', size = 1) +
  #geom_smooth(aes(y=gini, group = opportunity)) +
  facet_wrap(~opportunity) +
  theme_minimal()+
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 35, hjust = 1)
  ) +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) 



# Plot Gini index averaged
plot_gini <- access_pt %>%
  rbindlist(idcol = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) %>%
  group_by(opportunity, grid, router) %>%
  summarise(gini = ineq::Gini(accessibility)) %>%
  group_by(opportunity, router) %>%
  summarise(gini = mean(gini)) %>%
  mutate(
    router = factor(router, stage_labs, names(stage_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
  ) %>%
  ggplot(aes(router, gini, group = opportunity)) +
  geom_smooth(
    aes(col = opportunity, linetype = "Smoothed curve\nby LOESS", fill = "SE"),
    method = 'loess',
    alpha = 0.25,
    size = 1.2
  ) +
  geom_line(
    aes(linetype = "Mean"),
    col = "gray30", alpha = 0.8
  ) +
  labs(
    col = "Type of\nopportunity",
    x = "Temporal stage",
    y = "Gini index",
    # caption = "Source: The author based on own calculations. LOESS = Local Polynomial Regression Fitting. SE = Standard error."
  ) +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  scale_linetype_manual(values = c("dashed", "solid"), name = NULL) +
  scale_fill_manual(values = "gray50", name = NULL) +
  guides(
    color = guide_legend(order = 1, override.aes = list(fill = NA)),
    fill = guide_legend(order = 2, override.aes = list(col = NA)),
    linetype = guide_legend(override.aes = list(size = 1, fill = NA))
  ) +
  theme_minimal() +
  theme(legend.key.width = unit(1, "cm"))

# Save plot
ggsave(
  filename = "output/plots_maps/exploratory/plot_gini.png", 
  plot = plot_gini,
  height = 4, 
  width = 8, 
  dpi = 400, 
  bg = "white"
)

# Clean env.
rm(list = ls())
gc()
