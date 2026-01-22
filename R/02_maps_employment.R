
###########################################################################
###########################################################################
###                                                                     ###
###                           ECONOMIC CENSUS                           ###
###                  AGGREGATE DATA FROM BLOCKS TO SAS                  ###
###                                                                     ###
###########################################################################
###########################################################################


# Date: 2024-08-19
# Author: Rafael Verduzco

# Currently it only plots economic census data on grids to maps

# This code comes from '01_census_access/R/5.2_apportion_ce_eco_to_grids.R'.
# Originally this code
# 1. Transfer the data from 2014 economic census to spatial analytical schemes (SAS)
# 2. Visualises data in maps



## ---------------------------------------------------------------
##              Visualize Economic Census data in maps          --
## ---------------------------------------------------------------


# libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggstar)
library(data.table)
library(ggspatial)

dir.create('output/plots_maps/', recursive = TRUE)


# 2.1 Map preliminaries ---------------------------------------------------

# Fn format big number
format2 <-
  function(x, digits = 1) formatC(x, format = "f", big.mark = " ", digits = digits)
# Zocalo
zocalo <- data.frame(y = 19.432413, x = -99.132938, name = "Zócalo")
# Grid labels
grid_labs <-
  c(
    "Grid 0.5 km" = "grid_500",
    "Grid 1 km" = "grid_1000",
    "Grid 2 km" = "grid_2000",
    "Grid 4 km" = "grid_4000",
    "Post code" = "postcodes"
  )
# State boundaries
states <- 
  sf::st_read("data/marco_geoestadistico_nacional_2014_version_6.2/CONTINUO_NACIONAL/ESTADOS.shp")

# Load aggregated data in grids
grids_data_list <-
  list.files(
    "data/censo_ec_2014/",
    pattern = "grid|postcodes",
    full.names = TRUE
  )
grids_data <- lapply(grids_data_list, read_csv)
# Grids
grid_pat <- "((hex|postcodes_v5)|gpkg$)(?:.+)(gpkg$|(hex|postcodes_v5))"
grid_dirs <-
  list.files(
    "data/",
    recursive = TRUE,
    pattern = grid_pat,
    full.names = TRUE
  )
grid_dirs <- grid_dirs[order(basename(grid_dirs))]
grids <- lapply(grid_dirs, st_read)
grids <- map(grids, ~ mutate(.x, id = as.numeric(id)))
# Join data
grids_data <- purrr::map2(grids_data, grids, left_join, by = "id")
names(grids_data) <-
  gsub(
    pattern = ".gpkg|hex_|_v5_tidy_20200128",
    replacement = "",
    x = basename(grid_dirs)
  )


# Create variables for avg. income, income decile and density in jenks breaks
grids_data <- lapply(names(grids_data), function(x) {
  jenk_bks <- BAMMtools::getJenksBreaks(grids_data[[x]]$oc_pe_den_sq_km, 9)
  grids_data[[x]] %>%
    mutate(
      avg_income = round(est_remun_tot / est_pe_oc, 3),
      inc_ntile = ntile(avg_income, 5),
      oc_pe_den_jenks = cut(oc_pe_den_sq_km, jenk_bks)
    )
}) %>%
  setNames(names(grids_data))



# 2.2 Employment density map -----------------------------------------------


# Re-structure data
grids_data_long <- grids_data %>%
  rbindlist(idcol = "grid") %>%
  st_as_sf() %>%
  filter(!is.na(est_ec_un) | est_ec_un > 0) %>%
  mutate(grid = factor(grid, grid_labs, names(grid_labs)))
# Estimate cuts
breaks_emden <- BAMMtools::getJenksBreaks(grids_data_long$oc_pe_den_sq_km / 1e3, 12)
break_labs <-
  paste(
    format2(breaks_emden[-length(breaks_emden)]),
    format2(breaks_emden[-1]),
    sep = "-"
  )
# Metro area bounding box
metro_bbox <- st_bbox(grids[[5]])

# Title
title_emden <- "Employment density. Greater Mexico City, 2014."
caption_emden <- "Soruce: The Author based on Censos Economicos 2014 (INEGI, n.d.)"
fill_emden <- "Occupied personnel \nper sq. km \n(Thousands)"

# Plot grid
emden_map <- 
  grids_data_long %>%
  mutate(
    emden_br = cut(
      oc_pe_den_sq_km / 1e3,
      breaks_emden,
      labels = break_labs,
      include.lowest = TRUE
    )
  ) %>%
  ggplot() +
  geom_sf(aes(fill = emden_br), col = NA) +
  geom_sf(data = states, fill = NA, size = .4, col = "White") +
  geom_star(
    data = zocalo, aes(x, y, starshape = "CBD (Zócalo)"),
    fill = "white", col = "black", size = 2.1, alpha = 0.8
  ) +
  facet_wrap(~grid, ncol = 3) +
  scale_starshape(name = NULL) +
  coord_sf(
    xlim = c(metro_bbox[1], metro_bbox[3]), 
    ylim = c(metro_bbox[2], metro_bbox[4])
    ) +
  scale_fill_viridis_d(option = "inferno", direction = -1, end = 0.95) +
  guides(fill = guide_legend(order = 1)) +
  labs(
    title = title_emden,
    fill = fill_emden,
    # caption = caption_emden
  ) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.3, "cm"), width = unit(0.5, "cm"),
    style = north_arrow_orienteering(text_size = 4),
    data = tibble(grid = "Grid 2 km")
  ) +
  annotation_scale(
    location = "bl", height = unit(0.1, "cm"), line_width = 1,
    style = "ticks", text_cex = 0.65,
    data = tibble(grid = "Grid 4 km")
  ) +
  theme_void() +
  theme(
    legend.position = c(0.85, 0.25),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.height = unit(.5, "cm"),
    legend.key.width = unit(.2, "cm")
  )

# Save plot
ggsave(
  filename = "output/plots_maps/oc_pe_den_map.png",
  plot = emden_map,
  width = 8, 
  height = 7, 
  bg = "white", 
  dpi = 600
)


# Income map --------------------------------------------------------------

# Title
title_income <- "Median salary. Greater Mexico City, 2014."
caption_income <- "Soruce: The Author based on Censos Economicos 2014 (INEGI, n.d.)"
fill_income <- "Salary paid at \nestablisments by quintile"
grids_data_long$avg_income <-
  ifelse(is.infinite(grids_data_long$avg_income), NA, grids_data_long$avg_income)

# Legend labs
income_quintile_labs <- c('1 - Lowest', 2:4, '5 - Highest')

# Plot grid
income_map <- grids_data_long %>%
  filter(!is.na(avg_income)) %>%
  mutate(
    inc_ntile = factor(inc_ntile, labels = income_quintile_labs)
  ) %>%
  ggplot() +
  geom_sf(aes(fill = factor(inc_ntile)), col = NA) +
  geom_star(
    data = zocalo, aes(x, y, starshape = "CBD (Zócalo)"),
    fill = "white", col = "black", size = 2.4
  ) +
  facet_wrap(~grid, ncol = 3) +
  scale_starshape(name = NULL) +
  scale_fill_viridis_d(option = "turbo", direction = -1, end = 0.95, begin = 0.15) +
  guides(fill = guide_legend(order = 1)) +
  labs(
    # title = title_income,
    fill = fill_income,
    # caption = caption_income
  ) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.3, "cm"), width = unit(0.5, "cm"),
    style = north_arrow_orienteering(text_size = 4),
    data = tibble(grid = "Grid 2 km")
  ) +
  annotation_scale(
    location = "bl", height = unit(0.1, "cm"), line_width = 1,
    style = "ticks", text_cex = 0.65,
    data = tibble(grid = "Grid 4 km")
  ) +
  theme_void() +
  theme(
    legend.position = c(0.85, 0.25),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.height = unit(.5, "cm"),
    legend.key.width = unit(.25, "cm"),
    panel.spacing.y = unit(0, "lines"),
    strip.text = element_text(size = 8, margin = margin(0, 0, 0, 0))
  )

# Save plot
ggsave(
  filename = "output/plots_maps/inc_decile_map.png",
  plot = income_map,
  width = 8, 
  height = 6.5, 
  bg = "white", 
  dpi = 600
)


# -------------------------------------------------------------------------
