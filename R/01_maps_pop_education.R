############################################################################
############################################################################
###                                                                      ###
###      TRANSFER DATA FROM 2010 POPULATION CENSUS TO SPATIAL GRIDS      ###
###                                                                      ###
############################################################################
############################################################################

# Date : 2024-08-19
# Author: Rafael Verduzco

# this code creates population and education maps

# This code originally comes from '01_census_access/R/5.2_apportion_ce_eco_to_grids.R'.
# Originally, it aggregates the data form blocks to different spatial grids including post code areas in the ZMVM.
# 1. Creates directories  and download SCINCE
# 2. Subsets block geometries from SINCE data only for the ZMVM
# 3. Joins the socio-demographic data (economic, education and housing) at the block level
# 4. Aggregates the data from blocks to grids (hex grids and post code areas)
# 5. Tests results in map grids (population density and education by decile)
# 6. Save results as CSV (including joining id)





##---------------------------------------------------------------
##                          Section 4                          --
##---------------------------------------------------------------


# libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggstar)
library(data.table)
library(ggspatial)

dir.create('output/plots_maps/', recursive = TRUE)


# Read data ---------------------------------------------------------------

# Read census data in grids
# List of directories
grid_csv <- list.files("data/scince_2012/", recursive = T, full.names = T)
# Load CSVs
grid_csv_list <- lapply(grid_csv, read_csv)


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
grids_data <- purrr::map2(grid_csv_list, grids, left_join, by = "id")
names(grids_data) <-
  gsub(
    pattern = ".gpkg|hex_|_v5_tidy_20200128",
    replacement = "",
    x = basename(grid_dirs)
  )


# Preliminaries -----------------------------------------------------------



# # Load OSM road network
# roadnetwork <- st_read("01_Data/osm_roadnetwork_oct2019.gpkg")
# roadnetwork <- roadnetwork %>%
#   filter(highway %in% c("primary", "motorway_link", "motorway", "primary_link", "trunk", "trunk_link"))

# Fn format big number
format2 <- function(x, digits = 1) formatC(x, format = "f", big.mark = " ", digits = digits)
# Zocalo
zocalo <- data.frame(y = 19.432413, x = -99.132938, name = "Zócalo")
# Grid labels
grid_labs <- c("Grid 0.5 km" = "grid_500", "Grid 1 km" = "grid_1000", "Grid 2 km" = "grid_2000", "Grid 4 km" = "grid_4000", "Post code" = "postcodes")
# State boundaries
states <- st_read("data/marco_geoestadistico_nacional_2014_version_6.2/CONTINUO_NACIONAL/ESTADOS.shp")

# Re-structure data
grids_data_long <- grids_data %>%
  map(~ mutate(.x, education_ntile = ntile(edu49_r, 5))) %>%
  rbindlist(idcol = "grid") %>%
  st_as_sf() %>%
  filter(!is.na(pob1) | pob1 > 0) %>%
  mutate(grid = factor(grid, grid_labs, names(grid_labs)))


# Population density map --------------------------------------------------

# Estimate cuts
breaks_popden <- BAMMtools::getJenksBreaks(grids_data_long$pop_den / 1e3, 12)
break_labs <- paste(format2(breaks_popden[-length(breaks_popden)]), format2(breaks_popden[-1]), sep = "-")
# Metro area bounging box
metro_bbox <- st_bbox(grids[[5]])

# Title
title_popden <- "Population density. Greater Mexico City, 2010."
caption_popden <- "Soruce: The Author based on Censo de Población y Vivienda 2010 (INEGI, n.d.)"
fill_popden <- "Inhabitants \nper sq. km \n(Thousands)"

# Plot grid
popden_map <- grids_data_long %>%
  mutate(
    popden_br = cut(pop_den / 1e3, breaks_popden, labels = break_labs, include.lowest = TRUE)
  ) %>%
  ggplot() +
  geom_sf(aes(fill = popden_br), col = NA) +
  geom_sf(data = states, fill = NA, size = .4, col = "White") +
  geom_star(
    data = zocalo, aes(x, y, starshape = "CBD (Zócalo)"),
    fill = "white", col = "black", size = 2.1, alpha = 0.8
  ) +
  facet_wrap(~grid, ncol = 3) +
  coord_sf(xlim = c(metro_bbox[1], metro_bbox[3]), ylim = c(metro_bbox[2], metro_bbox[4])) +
  scale_starshape(name = NULL) +
  scale_fill_viridis_d(option = "inferno", direction = -1, end = 0.95) +
  labs(
    title = title_popden,
    fill = fill_popden,
    # caption = caption_popden
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
  filename = "output/plots_maps/pop_den_map.png",
  plot = popden_map,
  width = 8, 
  height = 7, 
  bg = "white", 
  dpi = 600
)


# Education level map -----------------------------------------------------

# Title
title_edu <- "Education level. Greater Mexico City, 2010."
fill_edu <- "Median \nyears of education \nby quintile."

# Plot grid
edu_map <- grids_data_long %>%
  filter(!is.na(education_ntile)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(education_ntile)), col = NA) +
  geom_sf(data = states, fill = NA, size = .4, col = "White") +
  geom_star(
    data = zocalo, aes(x, y, starshape = "CBD (Zócalo)"),
    fill = "white", col = "black", size = 2.1, alpha = 0.8
  ) +
  facet_wrap(~grid, ncol = 3) +
  coord_sf(xlim = c(metro_bbox[1], metro_bbox[3]), ylim = c(metro_bbox[2], metro_bbox[4])) +
  scale_starshape(name = NULL) +
  scale_fill_viridis_d(option = "turbo", direction = -1, end = 0.95, begin = 0.15) +
  labs(
    title = title_edu,
    fill = fill_edu,
    # caption = caption_popden
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
  filename = "output/plots_maps/edu_decile_map.png",
  plot = edu_map,
  width = 8, 
  height = 7, 
  bg = "white", 
  dpi = 600
)
