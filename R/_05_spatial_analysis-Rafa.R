
# Date: 24/03/2022
# Author: Rafael Verduzco

# This script corresponds to the analysis. Ch. 6, titled:
# In essence, it is an empirical review of PT accessibility in GMC.

# This code:
# 2. A visual exploratory analysis with spatial focus of access, using verious SAS



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
  list.files("data/accessibility_r5r/", full.names = TRUE, pattern = "pt") %>%
  map(data.table::fread) %>%
  bind_rows()

# Accessibility estimate for car
access_car <- 
  list.files("data/accessibility/", full.names = TRUE, pattern = "car") %>%
  map(data.table::fread) %>%
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
# Bind in a single DF
od_points_all <- od_points %>% 
  bind_rows(.id = 'grid')

# Define routers (temporal stages) for public transport
routers <- unique(access_pt$postcodes$router)
router_labels <- str_to_title(gsub("_", " ", routers))

# Accessibility parameters
access_pars <- read_csv("data/accessibility/access_pars.csv")

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

# Local functions ---------------------------------------------------------


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


# Limit PT accessibility  to exponential and bind in a single DF
access_pt <- access_pt %>%
  bind_rows(.id = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) 

# Bind Car accessibility
access_car <- access_car %>%
  bind_rows(.id = "grid") 

# Bind geometries
grid_geoms <- grid_geoms %>% 
  bind_rows(.id = "grid")



## ---------------------------------------------------------------
##                 Exploratory - Spatial focus                 --
## ---------------------------------------------------------------


# Spatial weight matrix ---------------------------------------------------

# Compute spatial matrix

# K-nearest neigh.
knn <- 6

i <- grids[1]


# Compute spatial matrix
spatial_matrix <-
  lapply(grids, function(i) {
    
    # Split access by grid
    access_pt_split <- access_pt %>% filter(grid == i)
    
    # Filter zones included in the dataset
    zone_id <- unique(access_pt_split$from_id)
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
    
    # Return W matrix
    return(W_list)
  })
names(spatial_matrix) <- grids

# There is a warning because some points are identical.
# This happens because in some cases the centroid is snapped to the nearest vertex in the network.
# If the road network is not dense, the nearest vertex is the same for more than one point.

# Check number of duplicated OD points
sapply(grids, function(i) {
  access_pt_grid <- access_pt %>% filter(grid == {{i}})
  # Filter zones in included in the dataset
  zone_id <- unique(access_pt_grid$origin)
  od_points_s <- od_points[[i]][od_points[[i]]$GEOID %in% zone_id, ]
  od_points_s <- od_points_s[order(od_points_s$GEOID), ]
  nrow(od_points_s[, -1]) - n_distinct(od_points_s[, -1])
})



# Aggregated accessibility over various temporal stages to grids o --------


# Summarise accessibility by median
# Transform data to ntiles for map
access_pt_aggregated <- access_pt %>%
  group_by(grid, origin, opportunity) %>%
  summarise(
    accessibility_med = median(accessibility, na.rm = TRUE),
    accessibility_mean = mean(accessibility, na.rm = TRUE),
    accessibility_sd = sd(accessibility, na.rm = TRUE),
    accessibility_sum = sum(accessibility, na.rm = TRUE),
  ) %>%
  group_by(grid, opportunity) %>%
  mutate(
    # Ntile by median accessibility over time for each cell
    access_ntile = factor(ntile(accessibility_med, 10))
  ) %>%
  ungroup() %>% 
  left_join(grid_geoms, by = c('grid', 'origin' = 'id')) %>% 
  st_as_sf() %>% 
  mutate(
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching")),
    grid_labelled = factor(grid, grid_labs, names(grid_labs))
  )

# Map accessibility over various grids ------------------------------------


# Map aggregated accessibility

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
map1_annotation <- rename(map1_annotation, grid_labelled = grid)
map1_annotation_w <- filter(map1_annotation, x > zocalo$x)
map1_annotation_e <- filter(map1_annotation, x < zocalo$x)

# Plot median accessibility by SAS
access_map1 <- access_pt_aggregated %>%
  ggplot() +
  # Fill by median ntile
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
  facet_grid(opportunity ~ grid_labelled, switch = "y") +
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
  filename = "output/plots_maps/exploratory/access_map_grid.png", 
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

# Maps using matching measures display more heterogeneous patters than maps using all
# We can confirm using a spatial correlation measure
# and compare heterogeneity of different access measures

set.seed(4)
# Moran's I
moran_res <-
  lapply(grids, function(i) {
    # Moran's I
    lapply(c("all", "k"), function(o) {
      morans_i <- access_pt %>%
        filter(grid == {{i}}) %>% 
        group_by(opportunity, origin) %>%
        summarise(accessibility = median(accessibility, na.rm = TRUE)) %>%
        pivot_wider(names_from = opportunity, values_from = accessibility) %>%
        arrange(origin) %>%
        pull({{ o }}) %>%
        moran.mc(., spatial_matrix[[i]], 3000, na.action = na.omit)
    })
  })

# # There are omitted cases in 'Matching':
# sapply(1:5, function(i) length(str_split(moran_res[[i]][[2]]$data.name, ', ')[[1]]))
# 
# # Check if this is because matching Access is NA too
# access_pt %>% 
#   group_by(grid, opportunity, origin) %>%
#   summarise(accessibility = median(accessibility, na.rm = TRUE)) %>%
#   pivot_wider(names_from = opportunity, values_from = accessibility) %>% 
#   group_by(grid) %>% 
#   summarise(
#     na_count = sum(is.na(k))
#   )
# # Yes, they correspond

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

# # Correlation surface area vs accessibility -------------------------------
# 
# # Post code aggregates zones in the periphery which tend to be larger.
# # These also have lower accessibility. 
# # For this reason, the average is higher for post code SAS
# 
# # Compute surface area
# access_pt_aggregated <- access_pt_aggregated %>% 
#   mutate(
#     area_sqkm = as.numeric(st_area(.) / 1e6),
#     area_log = log(area_sqkm)
#   )
# 
# 
# # Scatter plot
# access_pt_aggregated %>%
#   filter(grid == "Post code") %>%
#   ggplot(aes(area_sqkm, accessibility_med)) +
#   geom_point(shape = 1, alpha = 0.5) +
#   scale_x_log10() +
#   scale_y_log10() +
#   facet_wrap(~opportunity) +
#   geom_smooth(method = "glm")
# 
# # Compute correlation coefficient
# corr_area_access <- access_pt_aggregated %>% 
#   st_drop_geometry() %>% 
#   group_by(opportunity, grid) %>%
#   summarise(
#     cor_estimate = cor.test(accessibility_med, area_sqkm)$estimate,
#     cor_tvalue = cor.test(accessibility_med, area_sqkm)$statistic,
#     cor_pvalue = cor.test(accessibility_med, area_sqkm)$p.value,
#   )
# 
# # Save table
# write_csv(corr_area_access, "output//exploratory_rq1/corr_area_access.csv")


# Plot distribution of accessibility --------------------------------------

# Plot distribution density
grid_density_plot <- access_pt_aggregated %>%
  mutate(
    accessibility_cube = accessibility_med ^ (1 / 3),
    opportunity = factor(opportunity, labels = c("a) All", "b) Matching"))
  ) %>%
  ggplot(aes(accessibility_cube, col = grid_labelled)) +
  geom_density(adjust = 2, linewidth = 0.65, alpha = 0.8) +
  facet_wrap(~opportunity) +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  # scale_x_log10() +
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
  "output/plots_maps//exploratory/grid_density_plot.png", 
  grid_density_plot,
  height = 4, 
  width = 8, 
  dpi = 400, 
  bg = "white"
)

# Check mean and dispersion, adn coefficient of variance (CV)
access_pt_aggregated %>% 
  st_drop_geometry() %>% 
  group_by(opportunity, grid) %>% 
  summarise(
    mean = mean(accessibility_med, na.rm = TRUE),
    sd = sd(accessibility_med, na.rm = TRUE),
    CV = sd / mean
  )

# CV is larger for matching, indicating between cell heterogeneity is larger
# Between cell heterogeneity is smaller for post code in both measures


# Simple correlation between All and Matching -----------------------------

# Pivot wider to focus on differences 
access_pt_wide <- access_pt_aggregated %>% 
  pivot_wider(names_from = opportunity, values_from = starts_with('access')) %>% 
  mutate(
    access_diff = accessibility_med_Matching / accessibility_med_All
  )

# Compute mean difference
mean_diff <- access_pt_wide %>% 
  st_drop_geometry() %>% 
  group_by(grid) %>% 
  summarise(mean_diff = mean(access_diff, na.rm = TRUE))
mean_diff

# Pearson correlation between all and matching by grid
correlation_all_matching <- access_pt_wide %>% 
  st_drop_geometry() %>% 
  group_by(grid_labelled) %>% 
  summarise(
    cor_estimate = cor.test(accessibility_med_All, accessibility_med_Matching)$estimate,
    cor_statistic = cor.test(accessibility_med_All, accessibility_med_Matching)$statistic,
    cor_pvalue = cor.test(accessibility_med_All, accessibility_med_Matching)$p.value
  )

correlation_all_matching

# Save results
write_csv(
  correlation_all_matching, 
  "output/exploratory_rq1/correlation_all_matching.csv"
)

# Scatter plot
access_pt_wide %>% 
  st_drop_geometry() %>% 
  ggplot(aes(accessibility_med_All, accessibility_med_Matching)) +
  geom_point(shape = 4, alpha = 0.7) +
  facet_grid(~grid_labelled) +
  theme_classic()

# # Is this affecting high-access
# access_pt_wide %>% 
#   ggplot(aes(access_diff, accessibility_med_All)) +
#   geom_point(shape = 1) +
#   facet_wrap(~ grid_labelled) 

# Plot ratio matching over generic access
distribution_difference <- access_pt_wide %>% 
  st_drop_geometry() %>% 
  ggplot(aes(access_diff, col = grid_labelled)) +
  geom_density(adjust = 3, lwd = 1.3) +
  facet_wrap(~grid_labelled) +
  theme_minimal() +
  labs(
    y = 'Density',
    x = 'Similarity rate (matching over generic)',
    col = ''
  ) +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  theme(
    legend.position = c(0.9, 0.2)
  )

# Save plot
ggsave(
  "output/plots_maps/exploratory/distribution_difference.png", 
  distribution_difference,
  height = 6, 
  width = 8, 
  dpi = 400, 
  bg = "white"
)

# Differences in LISA analysis --------------------------------------------


library(sfdep)

# Map raw differences
access_pt_wide %>% 
  ggplot() +
  geom_sf(aes(fill = access_diff), col = NA) +
  facet_wrap(~ grid) +
  scale_fill_viridis_b() +
  theme_void()


# Compute LISA clusters
access_spat_stats <- access_pt_wide %>% 
  split(.$grid) %>% 
  lapply(function(df){
    
    # Compute spat stats using centroids
    access_spat_stats <- df %>% 
      st_drop_geometry() %>%
      left_join(od_points_all, by = c('grid', 'origin' = 'GEOID')) %>% 
      st_as_sf(coords = c('X', 'Y'), crs = 4326) %>%
      filter(!is.na(access_diff)) %>% 
      mutate(
        nb = st_knn(geometry, k = 6, symmetric = TRUE),
        wts = st_weights(nb),
        lag_access_diff = st_lag(access_diff, nb, wts),
        moran_local_diff = local_moran(access_diff, nb, wts)
      ) 
    
    # Expand spat stats and drop geometry and nb
    access_spat_stats <- access_spat_stats %>% 
      unnest(moran_local_diff) %>% 
      select(-nb, -wts) %>% 
      st_drop_geometry()
    
    return(access_spat_stats)
  })

# Bind results and join geoms
access_spat_stats <-  bind_rows(access_spat_stats, .id = 'grid')
access_spat_stats <- access_spat_stats %>% 
  left_join(grid_geoms, by = c('grid', 'origin' = 'id')) %>% 
  st_as_sf()


# LISA visualisations -----------------------------------------------------


# Map LISA results
lisa_diff_map <- access_spat_stats %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), NA)) %>%
  ggplot() +
  geom_sf(aes(fill = pysal), col = NA) +
  geom_sf(data = states, fill = NA, size = 0.25, col = "White") +
  geom_star(
    data = zocalo, aes(x, y, starshape = "CBD (Zócalo)"), 
    fill = "white", col = "black", size = 1.5, alpha = 0.9
  ) +
  coord_sf(
    xlim = c(metro_bbox[1], metro_bbox[3]), 
    ylim = c(metro_bbox[2], metro_bbox[4])
  ) +
  facet_wrap(~ grid_labelled) +
  scale_starshape(name = NULL) +
  scale_fill_viridis_d(option = 'turbo', begin = 0.1, end = 0.9, na.value = "grey50") +
  theme_void() +
  labs(
    fill = 'LISA\ncluster'
  ) +
  theme(legend.position = c(0.85, 0.25))


# Save map
ggsave(
  filename = "output/plots_maps/exploratory/lisa_diff_map.png", 
  lisa_diff_map,
  height = 6, 
  width = 8, 
  dpi = 600, 
  bg = "white"
)

# Plot LISA results
access_spat_stats %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.1, as.character(pysal), NA)) %>%
  ggplot(aes(access_diff, lag_access_diff, col = pysal)) +
  geom_point() +
  facet_wrap(~grid)

# Differences by social groups ---------------------------------------------


# Boxplot
access_spat_stats %>% 
  st_drop_geometry() %>% 
  left_join(census, by = c('grid', 'origin' = 'id')) %>%
  mutate(edu_quintile = factor(edu_quintile, labels = edu_labs)) %>% 
  ggplot(aes(access_diff, edu_quintile)) +
  facet_wrap(~grid_labelled) +
  geom_boxplot(outliers = FALSE) +
  labs(
    x = 'Similarity rate (Matching over generic accessibility)',
    y = 'Education level'
  ) +
  theme_minimal()

# Composition of clusters by education
access_spat_stats %>% 
  st_drop_geometry() %>% 
  mutate(pysal = ifelse(p_folded_sim <= 0.5, as.character(pysal), NA)) %>%
  filter(!is.na(pysal)) %>%
  left_join(census, by = c('grid', 'origin' = 'id')) %>%
  ggplot(aes(edu_quintile, pob1, fill = pysal)) +
  geom_col(position = 'fill') +
  coord_flip() +
  scale_fill_viridis_d(option = 'turbo', begin = 0.1, end = 0.9) +
  facet_wrap(~grid_labelled)  +
  labs(
    y = 'Similarity rate (Matching over generic accessibility)',
    x = 'Education level',
    fill = 'LISA\ncluster'
  ) +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.25))




# models_logit <- access_spat_stats %>% 
#   st_drop_geometry() %>% 
#   mutate(pysal = ifelse(p_folded_sim <= 0.05, as.character(pysal), NA)) %>%
#   left_join(census, by = c('grid', 'origin' = 'id')) %>%
#   mutate(
#     edu_quintile = factor(edu_quintile),
#     edu_quintile = fct_relevel(edu_quintile, '5'),
#     edu_deprived = if_else(edu_quintile == '1', 1, 0),
#     overestimated = if_else(pysal == 'Low-Low', 1, 0)
#   ) %>% 
#   split(.$grid) %>% 
#   lapply(function(data){
#     glm(
#       overestimated ~ edu_deprived + log(pop_den),  
#       data = data, 
#       family = "binomial"
#       )
#   })
# lapply(models_logit, summary)
