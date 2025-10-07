

library(tidyverse)
library(sf)
library(lwgeom)


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

# Re-structure data
grids_data_long <- grids_data %>%
  bind_rows(.id = "grid") %>%
  st_as_sf() %>%
  filter(!is.na(pob1) | pob1 > 0)


# Shape Index / Compactness Index ------------------------------------------

# Transform CRS
grids_data_long <- grids_data_long %>% 
  st_transform(crs = 32614)

# Estimate compactness indices
grids_data_long <- grids_data_long %>% 
  mutate(
    perimeter_m = as.numeric(st_perimeter(.)),
    area_m2 = as.numeric(st_area(.)),
    
    # Schumm’s Compactness Ratio
    schumm_cr = perimeter_m / (2 * sqrt(pi * area_m2)),
    # Form Factor (FF)
    form_factor = (4 * pi * area_m2) / (perimeter_m^2)
)

grids_data_long %>% 
  st_drop_geometry() %>% 
  group_by(grid) %>% 
  summarise(
    schumm_cr_mean = mean(schumm_cr),
    schumm_cr_sd = sd(schumm_cr),
    schumm_cr_q1 = quantile(schumm_cr, 0.25),
    schumm_cr_q2 = quantile(schumm_cr, 0.5),
    schumm_cr_q3 = quantile(schumm_cr, 0.75),
    
    ff_mean = mean(form_factor),
    ff_sd = sd(form_factor)
  )

grids_data_long %>% 
  filter(grid == 'postcodes') %>% 
  ggplot(aes(schumm_cr)) +
  geom_histogram() 

grids_data_long %>%
  st_drop_geometry() %>% 
  group_by(grid) %>%
  summarise(
    pct_schumm_cr_2 = mean(schumm_cr > 2) * 100
  )


# Minimum Bounding Rectangle (MBR) Ratio ----------------------------------


grids_data_long <- grids_data_long %>% 
  mutate(
    bbox = map(geom, st_bbox),
    mbr_width = map_dbl(bbox, ~ .x["xmax"] - .x["xmin"]),
    mbr_height = map_dbl(bbox, ~ .x["ymax"] - .x["ymin"]),
    mbr_ratio = pmax(mbr_width, mbr_height) / pmin(mbr_width, mbr_height)
  )


grids_data_long %>% 
  st_drop_geometry() %>% 
  group_by(grid) %>% 
  summarise(
    mbr_ratio_wmean = weighted.mean(mbr_ratio, pob1),
    mbr_ratio_mean = mean(mbr_ratio),
    mbr_ratio_sd = sd(mbr_ratio),
    mbr_ratio_q1 = quantile(mbr_ratio, 0.25),
    mbr_ratio_q2 = quantile(mbr_ratio, 0.5),
    mbr_ratio_q3 = quantile(mbr_ratio, 0.75)
  )


grids_data_long %>%
  st_drop_geometry() %>% 
  group_by(grid) %>%
  summarise(
    pct_mbr_gt_2 = mean(mbr_ratio > 2) * 100,
    pct_schumm_cr_2 = mean(schumm_cr > 2) * 100
  )

grids_data_long %>% 
  filter(grid == 'Post code') %>% 
  ggplot(aes(mbr_ratio)) +
  geom_histogram() 



# # Estimate Eccentricity via Ellipse Fitting -------------------------------
# 
# 
# # Function to compute eccentricity from minimum rotated rectangle
# compute_eccentricity <- function(geom) {
#   mrr <- st_minimum_rotated_rectangle(geom)
#   coords <- st_coordinates(mrr)[1:4, ]  # Get rectangle corners
#   
#   # Compute side lengths
#   side_lengths <- sqrt(rowSums((coords - coords[c(2,3,4,1), ])^2))
#   a <- max(side_lengths) / 2  # semi-major axis
#   b <- min(side_lengths) / 2  # semi-minor axis
#   
#   if (a == 0) return(NA)
#   sqrt(1 - (b / a)^2)
# }
# 
# # Apply to your dataset
# grids_data_long <- grids_data_long %>%
#   mutate(
#     eccentricity = map_dbl(geom, compute_eccentricity)
#   )
# 
# grids_data_long %>% 
#   st_drop_geometry() %>% 
#   group_by(grid) %>% 
#   summarise(
#     ecce_mean = mean(eccentricity),
#     ecce_sd = sd(eccentricity),
#     ecce_q1 = quantile(eccentricity, 0.25),
#     ecce_q2 = quantile(eccentricity, 0.5),
#     ecce_q3 = quantile(eccentricity, 0.75)
#   )
# 
# 
# grids_data_long %>% 
#   filter(eccentricity < 0.1) %>% 
#   select(geom) %>% 
#   mapview()
