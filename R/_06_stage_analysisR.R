

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
  list.files("data/accessibility_r5r/", full.names = TRUE, pattern = "pt") %>%
  map(data.table::fread) %>%
  # setNames(grids) %>% 
  bind_rows()

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


# Limit PT accessibility  to exponential and bind in a single DF
access_pt <- access_pt %>%
  # bind_rows(.id = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) 


# Bind geometries in a single DF
grid_geoms <- grid_geoms %>% 
  bind_rows(.id = "grid")



## ----------------------------------------------------------------
##                 Exploratory - Temporal focus                 --
## ----------------------------------------------------------------

# Plot longitudinal access ------------------------------------------------


# Estimate relative change over time using Stage 1 as reference
longitudinal_relative <- access_pt %>%
  group_by(opportunity, grid, router) %>%
  summarise(accessibility = mean(accessibility, na.rm = TRUE)) %>%
  mutate(
    router = factor(router, stage_labs, names(stage_labs)),
    grid_labelled = factor(grid, grid_labs, names(grid_labs)),
    opportunity = factor(opportunity, c("all", "k"), c("a) All", "b) Matching"))
  ) %>%
  arrange(opportunity, grid_labelled, router) %>%
  group_by(opportunity, grid) %>%
  mutate(
    rel_change = (accessibility / first(accessibility)) - 1,
    rel_change = replace_na(rel_change, 0) * 100
  ) %>% 
  ungroup()


longitudinal_relative %>%
  ggplot(aes(router, rel_change, group = opportunity, col = opportunity)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  facet_wrap(~grid_labelled) +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(
    x = "MPTN temporal stage",
    y = "Relative change (%)",
    col = "SAS"
  ) +
  theme_minimal()

# Relative accessibility change over time all grids
plot_longitudinal_relative <- longitudinal_relative %>%
  ggplot(aes(router, rel_change, group = grid, col = grid_labelled)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  facet_wrap(~opportunity) +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(
    x = "MPTN temporal stage",
    y = "Relative change (%)",
    col = "SAS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
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


# Relative change 0.5 km grid only
plot_longitudinal_relative_500 <- longitudinal_relative %>%
  filter(grid == 'grid_500') %>% 
  ggplot(aes(router, rel_change, group = opportunity, col = opportunity)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point() +
  scale_color_viridis_d(option = "turbo", begin = .1, end = .9) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(
    x = "MPTN temporal stage",
    y = "Relative change Grid 0.5 km (%)",
    col = "Accessibility measure"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(hjust = 0, size = 9)
  )
# Save plot
ggsave(
  filename =  "output/plots_maps/exploratory/plot_longitudinal_relative_500.png", 
  plot = plot_longitudinal_relative_500,
  height = 4, 
  width = 6, 
  dpi = 400, 
  bg = "white"
)



# Identify geometries influenced by the MPT network -----------------------

# Prepare stops
stops <- mptn[[2]] %>%
  set_names(stage_labs) %>%
  map(st_transform, 32614)

# Prepare origins
access_pt_cent <- access_pt %>%
  filter(grid == "grid_500") %>%
  left_join(od_points_all, by = c('grid', 'from_id' = 'GEOID')) %>%
  split(.$router)

# Spatial join
access_pt_cent <- map2(access_pt_cent, stops, \(x, y) {
  x %>%
    st_as_sf(coords = c('X', 'Y'), crs = 4326) %>%
    st_transform(32614) %>%
    mutate(
      mptn_2km = if_else(
        lengths(st_intersects(geometry, st_buffer(y, 2e3))) > 0, 1, 0
      ),
      mptn_3km = if_else(
        lengths(st_intersects(geometry, st_buffer(y, 3e3))) > 0, 1, 0
      )
    )
})

access_pt_cent <- access_pt_cent %>%
  bind_rows(.id = 'router') %>%
  st_drop_geometry()



# Map variability of access measures over temporal stages -----------------



access_change <- access_pt_cent %>% 
  # pivot_wider(names_from = opportunity, values_from = accessibility) %>% 
  mutate(
    router = factor(router, stage_labs, names(stage_labs)),
    grid_labeled = factor(grid, grid_labs, names(grid_labs))
  ) %>% 
  arrange(from_id, opportunity, router) %>% 
  group_by(from_id, opportunity) %>%
  mutate(
    change_access = accessibility / lag(accessibility),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
  ) %>%
  ungroup() 

# # Join census data
# access_change %>% 
#   filter(mptn_2km == 1) %>% 
#   left_join(census, by = c('grid', 'from_id' = 'id')) %>% 
#   ggplot(aes(change_access, edu_quintile, fill = opportunity)) +
#   geom_boxplot() +
#   coord_cartesian(xlim = c(0, 4)) +
#   facet_wrap(~router)


# Map change
map_change_access_annotated2 <- access_change %>%
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
  # geom_sf(data = states, fill = NA, lwd = 0.3, col = "White", alpha = 0.75) +
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
  theme_fmap

# Save map
ggsave(
  filename = "output/plots_maps/exploratory/map_change_access2.png",
  plot = map_change_access_annotated2,
  height = 6, 
  width = 11, 
  dpi = 600, 
  bg = "white"
)

# 
# 
# # Map variability of access measures over temporal stages -----------------
# 
# 
# # Evolution of MPTN
# mptn_evo <-
#   lapply(2:7, function(x) {
#     mptn[[1]][[x]] %>%
#       filter(!route_id %in% mptn[[1]][[x - 1]]$route_id)
#   }) %>%
#   setNames(names(stage_labs)[-1]) %>%
#   bind_rows(.id = "router") %>%
#   bind_rows(., .) %>%
#   mutate(
#     opportunity = c(rep("All", nrow(.) / 2), rep("Matching", nrow(.) / 2))
#   ) %>%
#   st_as_sf()
# # Cumulative evo of network
# mptn_cum <-
#   lapply(2:7, function(x) {
#     mptn[[1]][[x]] %>%
#       filter(!route_id %in% mptn[[1]][[1]]$route_id)
#   }) %>%
#   setNames(names(stage_labs)[-1]) %>%
#   bind_rows(.id = "router") %>%
#   bind_rows(., .) %>%
#   mutate(
#     opportunity = c(rep("All", nrow(.) / 2), rep("Matching", nrow(.) / 2))
#   ) %>%
#   st_as_sf()
# 
# # Read annotations
# temp_annotation <- read_csv("data/labels_stage.csv")
# temp_annotation <- temp_annotation[-6:-8, ]
# temp_annotation <- bind_rows(list(All = temp_annotation, Matching = temp_annotation), .id = "opportunity")
# temp_annotation_north <- filter(temp_annotation, lat > zocalo$y)
# temp_annotation_south <- filter(temp_annotation, lat < zocalo$y)
# 
# 
# # Cumulative variance fn
# cumvar <- function(x, sd = TRUE) {
#   x <- x - x[sample.int(length(x), 1)] ## see Remark 2 below
#   n <- seq_along(x)
#   v <- (cumsum(x^2) - cumsum(x)^2 / n) / (n - 1)
#   if (sd) v <- sqrt(v)
#   v
# }
# 
# # Estimate changes in accessibility
# access_change <- access_pt %>%
#   group_by(grid, origin, opportunity) %>%
#   mutate(
#     change_net = accessibility - lag(accessibility),
#     change_rel = accessibility / lag(accessibility),
#     change_sd = sd(accessibility, na.rm = TRUE),
#     change_cumsum = cumsum(accessibility),
#     change_cumsd = cumvar(accessibility),
#     change_relcumsd = change_cumsd / mean(accessibility, na.rm = TRUE),
#     change_first = accessibility - first(accessibility),
#     change_first_r = accessibility / first(accessibility)
#   ) %>%
#   ungroup() %>% 
#   mutate(
#     router = factor(router, stage_labs, names(stage_labs)),
#     grid_labeled = factor(grid, grid_labs, names(grid_labs)),
#     opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
#   )
# 
# 
# # Label
# map2_legend_labs <- 
#   c("Low (0)", rep("", 8), "High (27)")
# map2_title <- 
#   "Variability of accessibility to employment introduced by various extensions of the MPTN between 2010 and 2019"
# map2_subtitle <- 
#   "The degree of variability is measured as the standard deviation for each origin according to the estimates of each temporal stage. \nThe maps show the cumulative standard deviation over time as standarized by type of accessibility measure."
# map2_fill <- 
#   "Variability \n(standardized SD)  "
# map2_caption <- 
#   "Source: the author based on own calculations, Censos Economicos 2014 (Inegi, n.d.), Censo de Población y Vivienda 2010 (Inegi, n.d)."
# # Col scale for routes
# col_routes <- 
#   c("gray60", "black")
# 
# 
# 
# # Map
# map_change_access <- access_change %>%
#   filter(grid == "grid_500") %>%
#   filter(!is.na(change_cumsd)) %>%
#   group_by(opportunity) %>%
#   mutate(change_scaled = scale(change_cumsd)[, 1]) %>%
#   ungroup() %>% 
#   left_join(grid_geoms, by = c('grid', "origin" = "id")) %>%
#   st_as_sf() %>%
#   ggplot() +
#   geom_sf(aes(fill = change_scaled), col = NA, alpha = 1) +
#   geom_sf(data = states, fill = NA, lwd = 0.3, col = "White", alpha = 0.75) +
#   geom_sf(data = mptn_cum, aes(col = "MPTN ext."), lwd = 0.18, alpha = 0.6) +
#   geom_sf(
#     data = mptn_evo, 
#     aes(col = "Previous \nMPTN ext."), 
#     lwd = 0.18, alpha = 0.7
#   ) +
#   geom_star(
#     data = zocalo, 
#     aes(x, y, starshape = "CBD (Zócalo)"), 
#     fill = "white", col = "black", size = 1.5, alpha = 0.9
#   ) +
#   scale_starshape(name = NULL) +
#   coord_sf(xlim = c(-98.91, -99.26), ylim = c(19.24, 19.7)) +
#   facet_grid(opportunity ~ router, switch = "y") +
#   scale_fill_viridis_b(
#     n.breaks = 10, option = "inferno", direction = -1, begin = 0.05, end = 0.95, 
#     # labels = map2_legend_labs
#   ) +
#   scale_color_manual(values = col_routes, name = NULL) +
#   labs(
#     title = map2_title,
#     subtitle = map2_subtitle,
#     fill = map2_fill,
#     # caption = map2_caption
#   ) +
#   guides(
#     fill = guide_legend(label.position = "bottom", title.vjust = 1, nrow = 1, order = 1),
#     color = guide_legend(override.aes = list(size = 1.3, col = rev(col_routes)))
#   ) +
#   annotation_north_arrow(
#     location = "tr", which_north = "true",
#     height = unit(0.3, "cm"), width = unit(0.5, "cm"),
#     style = north_arrow_orienteering(text_size = 4),
#     data = tibble(opportunity = c("All"), router = "2018 to 2019")
#   ) +
#   annotation_scale(
#     location = "bl", height = unit(0.1, "cm"), line_width = 0.75,
#     style = "ticks", text_cex = 0.5,
#     data = tibble(opportunity = c("Matching"), router = "2010 to 2012")
#   ) +
#   theme_fmap
# 
# # Annotations
# map_change_access_annotated <-
#   map_change_access +
#   geom_label_repel(
#     aes(lon, lat, label = name),
#     data = temp_annotation,
#     nudge_x = 0.06, nudge_y = 0.03, label.padding = 0.1,
#     segment.size = 0.35, segment.color = "gray40",
#     size = 1.6, alpha = 0.6
#   )
# 
# # Save map
# ggsave(
#   filename = "output/plots_maps/exploratory/map_change_access.png",
#   plot = map_change_access_annotated,
#   height = 6, 
#   width = 11, 
#   dpi = 600, 
#   bg = "white"
# )


# Gini index --------------------------------------------------------------

# Gini index
gini_temporal <- access_pt %>%
  # rbindlist(idcol = "grid") %>%
  # filter(dec_fn == "exp" & beta != 0.031) %>%
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
  # rbindlist(idcol = 'grid') %>%
  # filter( dec_fn == 'exp' & beta !=  0.031) %>%
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
  # rbindlist(idcol = "grid") %>%
  # filter(dec_fn == "exp" & beta != 0.031) %>%
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
