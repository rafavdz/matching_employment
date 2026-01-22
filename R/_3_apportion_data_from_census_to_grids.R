############################################################################
############################################################################
###                                                                      ###
###      TRANSFER DATA FROM 2010 POPULATION CENSUS TO SPATIAL GRIDS      ###
###                                                                      ###
############################################################################
############################################################################

# Date : 2020-02-11
# Author: Rafael Verduzco

# This code:
# Aggregates the data form blocks to different spatial grids including post code areas in the ZMVM.
# 1. Creates directories  and download SCINCE
# 2. Subsets block geometries from SINCE data only for the ZMVM
# 3. Joins the socio-demographic data (economic, education and housing) at the block level
# 4. Aggregates the data from blocks to grids (hex grids and post code areas)
# 5. Tests results in map grids (population density and education by decile)
# 6. Save results as CSV (including joining id)


# Set environment ---------------------------------------------------------

# Load packages
source("R/00_LoadPackages.R")

# Set CRS
myCRSutm <- 32614
myCRSlonglat <- 4326

# Download and unzip census data ------------------------------------------

# # Create folder's project
# dir.create("./01_Data/scince_2012")
# dir.create("./01_Data/scince_2012/raw")
# dir.create("./04_Outputs/scince_2012")
#
# # Download census data
# # Create list of states
# entidades <- c("15", "09", "13")
# # download files of all states that are part of the ZMVM (Note that these are summaries at block level of urban settlements only...Does not include rural)
# for (i in entidades) {
#   download.file(
#     url= paste0("https://www.inegi.org.mx/contenidos/masiva/indicadores/inv/", i , "_SCINCE_zip.zip"),
#     destfile = paste0("01_Data\\scince_2012\\raw\\since_", i, ".zip"),
#     quiet = FALSE, mode = "wb")}
#
# # Unzip files
# # Create a list of zip files in the folder
# zipF <- list.files(path = "./01_Data/scince_2012/raw", pattern = "*.zip", full.names = T)
# # Unzip all folders in the list
# for (i in zipF) {zip:: unzip( i, exdir = "01_Data\\scince_2012\\raw\\")}
# # Remove .zip files
# sapply(zipF, unlink)

##---------------------------------------------------------------
##                          Section 1                          --
##---------------------------------------------------------------

# Load CINSE 2012 block geometries and subset data in GMC -----------------


# Load list of municipalities within the ZMVM
muni_zmvm <- read_csv("01_Data/lista_municipios_zmvm.csv")

# Load raw shapes in a list (at block level)
manzanas_since <-
  lapply(
    list.files("./01_Data/scince_2012/raw",
      recursive = T,
      pattern = "manzanas.shp", full.names = TRUE
    ), st_read
  )
glimpse(manzanas_since)

# Remove variables that contain population by age data
manzanas_since <- lapply(manzanas_since, dplyr::select, CVEGEO, POB1)
glimpse(manzanas_since)

# Print total population in millions
sapply(manzanas_since, function(x) sum(x$POB1)) / 1000000

# Bind in one large data frame
manzanas_since_zmvm <- st_as_sf(rbindlist(manzanas_since))
# Names to low case
names(manzanas_since_zmvm) <- tolower(names(manzanas_since_zmvm))

# Check results
class(manzanas_since_zmvm)
manzanas_since_zmvm[manzanas_since_zmvm$pob1 < 1, ]

manzanas_since_zmvm[NA, ] # Some strange results, e.g. NA.1, NA.2, etc...
which(st_is_empty(manzanas_since_zmvm)) # Which is empty geometry

# What types of geometries are NOT "POLYGON"
manzanas_since_zmvm %>% filter(!grepl("POLYGON", st_geometry_type(geometry)))

glimpse(manzanas_since_zmvm)
summary(manzanas_since_zmvm)
nrow(manzanas_since_zmvm)

# Subset data for the ZMVM only
manzanas_since_zmvm$ent_mun <- substr(manzanas_since_zmvm$cvegeo, 1, 5) # First, create new variable of state and mun code
since_zmvm <- manzanas_since_zmvm[manzanas_since_zmvm$ent_mun %in% muni_zmvm$cve_ent_mu, ] # Filter muinicipalities within the zmvm based on the list

# Check resutls

# Summary
summary(since_zmvm)
glimpse(since_zmvm)

# Total population
sum(since_zmvm$pob1)
# Total population by state
since_zmvm %>%
  st_set_geometry(NULL) %>%
  mutate(ent = substr(since_zmvm$cvegeo, 1, 2)) %>%
  dplyr::group_by(ent) %>%
  dplyr::summarize(pop = sum(pob1)) # %>% {sum(.$pop)}

since_zmvm$cvegeo <- as.character(since_zmvm$cvegeo)

# Population map visualization
# since_zmvm %>%
#   filter(startsWith(cvegeo, "15109") | startsWith(cvegeo, "15108") | startsWith(cvegeo, "15020")) %>%
#   tm_shape() + tm_polygons(
#     col = "pob1", border.alpha = 0.0,
#     alpha = 0.75, palette = "viridis",
#     style = "jenks", n = 10
#   )

# Select variables
since_zmvm <- dplyr::select(since_zmvm, cvegeo, pob1)

# Transform CRS geometries and save results
since_zmvm %>%
  st_transform(myCRSlonglat) %>%
  st_write(dsn = "./01_Data/scince_2012/manzanas_scinse.gpkg")

# Remove objects from environment
rm(manzanas_since, manzanas_since_zmvm)

# Clean Global Env.
gc(reset = T)


# Rural settlements -------------------------------------------------------

# Read data
loc_rur <- list.files("./01_Data/scince_2012/raw", recursive = T, pattern = "loc_rur\\.shp", full.names = TRUE)
loc_rur <- lapply(loc_rur, st_read)
loc_rur <- bind_rows(loc_rur)
# Format names
names(loc_rur) <- tolower(names(loc_rur))

# Filter locations within GMC
loc_rur <- loc_rur %>%
  mutate(cve_ent_mun = str_sub(cvegeo, 0, 5)) %>%
  filter(cve_ent_mun %in% muni_zmvm$cve_ent_mu)

# Total population in rural locations
sum(loc_rur$pob1)

# Transform CRS geometries and save results
loc_rur %>%
  st_transform(myCRSlonglat) %>%
  st_write(dsn = "./01_Data/scince_2012/rural_locations_scinse.gpkg")


# Overall figures ---------------------------------------------------------

# Total population in GMC
sum(loc_rur$pob1) + sum(since_zmvm$pob1)
# Population in urban blocks
sum(since_zmvm$pob1)
# Proportion of rural population
sum(loc_rur$pob1) / (sum(loc_rur$pob1) + sum(since_zmvm$pob1))

##---------------------------------------------------------------
##                          Section 2                          --
##---------------------------------------------------------------


# Join the non-spatial data of interest -----------------------------------

# Load and inspect the data available
decrip_data <- read.dbf("./01_Data/scince_2012/raw/09/descriptores/desc_cpv2010.DBF")
View(decrip_data)

# Create list of variables of interest
vars <-
  c(
    "CVEGEO", # Geometry code
    "EDU39", # Población masculina de 15 años y más con educación básica completa
    "EDU40", # Población de 15 años y más con educación pos-básica.
    "EDU49_R", # Grado promedio de escolaridad
    "ECO1", # Población económicamente activa
    "ECO4", # Población ocupada
    "VIV0", # Total de viviendas
    "VIV1", # Total de viviendas habitadas
    "VIV17", # Viviendas particulares habitadas que no disponen de agua entubada en el ambito de la vivienda
    "VIV23", # Viviendas particulares habitadas que no disponen de drenaje
    "VIV24", # Viviendas particulares habitadas que disponen de luz eléctrica, agua entubada en el ambito de la vivienda y drenaje
    "VIV28"
  ) # Viviendas particulares habitadas que disponen de automómovil o camioneta

decrip_data[decrip_data$CAMPO %in% vars, ]

# Load Since data
data_edu <- lapply(list.files("./01_Data/scince_2012/", pattern = "manzanas_caracteristicas_educativas.dbf", recursive = T, full.names = T), read.dbf) %>%
  bind_rows() %>%
  as.data.frame()
data_eco <- lapply(list.files("./01_Data/scince_2012/", pattern = "manzanas_caracteristicas_economicas.dbf", recursive = T, full.names = T), read.dbf) %>%
  bind_rows() %>%
  as.data.frame()
data_viv <- lapply(list.files("./01_Data/scince_2012/", pattern = "manzanas_viviendas.dbf", recursive = T, full.names = T), read.dbf) %>%
  bind_rows() %>%
  as.data.frame()

# Inspect imports
glimpse(data_edu)
glimpse(data_eco)
glimpse(data_viv)

# Select only variables of interest
data_edu <- data_edu[, names(data_edu) %in% vars]
data_eco <- data_eco[, names(data_eco) %in% vars]
data_viv <- data_viv[, names(data_viv) %in% vars]

# Join in a single data frame
data_since_all <- left_join(data_edu, data_eco) %>% left_join(data_viv)
names(data_since_all) <- tolower(names(data_since_all))

# Inspect results
glimpse(data_since_all)
summary(data_since_all)

# Replace negative values with NAs
data_since_all <- data_since_all %>%
  mutate_at(vars(edu40:viv28), funs(replace(., . < 0, NA)))

# Inspect results
glimpse(data_since_all)
summary(data_since_all)

# Join demographic data to block geometries data
since_zmvm_data <- left_join(since_zmvm, data_since_all)

# Inspect results
glimpse(since_zmvm_data)
summary(since_zmvm_data)

# General figures for the ZMVM urban population (in millions)
names(since_zmvm_data)
round(apply(st_set_geometry(since_zmvm_data[, c(2:12)], NULL), 2, sum, na.rm = TRUE) / 1000000, 4)

# Test results in map

# Create education by decile
since_zmvm_data$edu_decile <- ntile(since_zmvm_data$edu49_r, 10)
summary(since_zmvm_data$edu49_r)

# Create map of years of education by decile at the block level
ggplot() +
  geom_sf(data = filter(since_zmvm_data, pob1 > 0), mapping = aes(fill = edu_decile), color = NA, lwd = 0) +
  scale_fill_distiller(palette = "RdBu", guide = "colorbar", trans = "reverse") +
  theme_bw()

# Select and re-order variables
str(since_zmvm_data)
since_zmvm_data <- dplyr::select(since_zmvm_data, -edu_decile)

# Write geopkg
since_zmvm_data %>%
  st_transform(myCRSlonglat) %>%
  st_write(dsn = "./01_Data/scince_2012/manzanas_scinse_data.gpkg")

# Clean Global Env.
gc(reset = T)

# Remove objects from environment
rm(list = ls())

##---------------------------------------------------------------
##                          Section 3                          --
##---------------------------------------------------------------

# Aggregate data from blocks to grids --------------------

# Load data source
# Blocks with SCINCE (Census) data (xx objects)
blocks <- st_read("./01_Data/scince_2012/manzanas_scinse_data.gpkg")

# Tot population included
sum(blocks$pob1)
# Total dwellings
sum(blocks$viv0)

# Fix invalid geometries in blocks
blocks <- blocks %>%
  st_transform(32614) %>%
  st_buffer(0) %>%
  st_transform(4326)
# See summary
blocks %>%
  st_set_geometry(NULL) %>%
  summary()

# Load grids (receiver areas)
# Create list of grids' directories
grid_dirs_list <- list.files("./04_Outputs/Spatial grids", full.names = T)
grid_dirs_list[5] <- list.files("./04_Outputs/Post codes", pattern = ".gpkg$", full.names = T)
# read grids
grids <- lapply(grid_dirs_list, st_read)
# Name list's elements
names(grids) <- gsub(".gpkg|hex_|_v5_tidy_20200128", "", basename(grid_dirs_list))
# Fix invalid geometries in postcodes
# grids$postcodes <-  grids$postcodes %>%
#   st_transform(32614) %>%  st_buffer(0) %>%
#   st_transform(4326)
grids$postcodes <- st_make_valid(grids$postcodes)

# # Visualize overlap
# blocks %>%
#   filter(grepl("^09015", cvegeo)) %>%
#   mapview(layer.name = "City block", col.region = "gray35") +
#   mapview(grids$grid_1000, lwd = 3.5, col.region = "purple",
#           alpha.region = 0, layer.name = "Grid 1 km", legend = FALSE)

# Load proportional overlay function
apportion <- function(xx, yy,
                      low_lim_sqm2 = 60, # Smallest total overlap of each xx object from which it would transfer data in sq metres,
                      low_lim_percent = 0.05, # Smallest total overlap of xx object from which it would transfer data in percentage
                      low_2rescale = 0.70) { # Smallest total overlap of xx object that would be rescaled to 1, to transfer all data from xx to yy

  # Transform to metric CRS
  # Set CRS
  myCRSutm <- 32614
  myCRSlonglat <- 4326
  print("Transforming CRS...")
  xx <- st_transform(xx, myCRSutm)
  yy <- st_transform(yy, myCRSutm)

  # Operations to transfer data from xx (giver area) to yy (receiver area)
  # Calculate xx areas in sqm.
  xx$xx_area_m2 <- st_area(xx) %>% as.numeric()
  print("Processing spatial intersection...")
  # Intersect xx and yy (takes time)
  intersection <- st_intersection(xx, yy)
  # Calculate area of xx in yy
  intersection$inters_area_m2 <- st_area(intersection) %>% as.numeric()
  # Calculate the overlap percentage of each intersection over the original (complete) area of xx
  intersection$per_overlpa <- intersection$inters_area_m2 / intersection$xx_area_m2 %>% as.numeric()
  # Transform intersection to non-spatial object
  intersection_nonspatial <- st_set_geometry(intersection, NULL)
  print("Processing after intersection...")
  # Generate summaries of the intersection by xx object
  intersection_sums_by_xx <- intersection_nonspatial %>%
    dplyr::group_by(cvegeo) %>%
    dplyr::summarise(sum_overlap_m2 = sum(inters_area_m2), sum_per_overlap = sum(per_overlpa))

  # First condition.
  # Exclude complete xx geometries from the analysis if the sum of
  # the xx intersection on yy is less than <low_lim_sqm2> AND if the sum of the
  # intersection is lower than <low_lim_percent>
  # Create key to identify xx areas to be excluded
  xx_exclude <- intersection_sums_by_xx %>%
    mutate(cond_1 = if_else(sum_per_overlap < low_lim_percent & sum_overlap_m2 < low_lim_sqm2, TRUE, FALSE)) %>%
    filter(cond_1 == TRUE) %>%
    pull(cvegeo)
  # Create subset excluding intersections under the first condition
  intersection_nonspatial <- subset(intersection_nonspatial, !(intersection_nonspatial$cvegeo %in% xx_exclude))

  # Second condition.
  # If the total percentage of overlap is more than low_2rescale, then,
  # re-scale to 1. This is to transfer the total values of XX.
  # If it is not, transfer the data according to the simple overlap proportion

  # Calculate the rescaling factor for objects under second condition
  intersection_nonspatial <- intersection_nonspatial %>%
    # To obtain the total overlapping area of xx objects in each intersection
    left_join(intersection_sums_by_xx) %>%
    # Calculate the rescaling proportion if xx objects are larger than
    # xx_2rescale_key: each intersection / sum of the xx overlap
    mutate(assign_fac = if_else(sum_per_overlap > low_2rescale,
      per_overlpa / sum_per_overlap,
      per_overlpa
    ))
  # Transfer data according to the assignment factor
  data_in_yy <- intersection_nonspatial %>%
    mutate_at(
      vars(pob1:edu40, eco1:viv28),
      list(~ round(. * intersection_nonspatial$assign_fac, 3))
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise_at(vars(pob1:edu40, eco1:viv28), sum, na.rm = T)

  # Estimate weighted population median for the average education level
  data_in_yy <- left_join(data_in_yy, intersection_nonspatial %>%
    # To estimate the population in each intersection
    mutate(pop_estimate = round(pob1 * assign_fac, 3)) %>%
    dplyr::group_by(id) %>%
    # Median of average years of education weighted by population
    dplyr::summarise(
      edu49_r = median(rep(edu49_r, times = pop_estimate), na.rm = T)
    ))

  # Results
  # Join the data to the spatial objects in yy
  data_in_yy_spat <- left_join(yy, data_in_yy)
  print("Transforming back to  WGS")
  # Transform back to WGS
  data_in_yy_spat <- st_transform(data_in_yy_spat, myCRSlonglat)

  # Return
  return(data_in_yy_spat)
}


# Apply function to aggregate from block's data to grids
grids_data <- lapply(grids, apportion, xx = blocks)
glimpse(grids_data)

# Inspect results ---------------------------------------------------------

# Variables' code
# pob1 Total population
# EDU40 Población de 15 años y más con educación pos-básica.
# ECO1 Población económicamente activa
# ECO4 Población ocupada
# VIV0 Total de viviendas
# VIV1 Total de viviendas habitadas
# VIV17 Viviendas particulares habitadas que no disponen de agua entubada en el ámbito de la vivienda
# VIV23 Viviendas particulares habitadas que no disponen de drenaje
# VIV24 Viviendas particulares habitadas que disponen de luz eléctrica, agua entubada en el ámbito de la vivienda y drenaje
# VIV28 Viviendas particulares habitadas que disponen de automóvil o camioneta
# EDU49_R Grado promedio de escolaridad

# Sum of all variables in blocks (data source)
apply(as.data.frame(blocks)[, c(2:3, 5:12)], 2, sum, na.rm = T)

# Sum of all variables in grid
for (i in grids_data) {
  print(apply(as.data.frame(i)[, c(2:11)], 2, sum, na.rm = TRUE))
}
# Difference with source ... Slightly less than the source due to rounding
# In post codes is larger due to blocks in Census out of post code areas

# Median average years of education population weighted
# assumes all people in area n have the same education level
# Median in data source
median(rep(blocks$edu49_r, times = blocks$pob1), na.rm = T)
# In grids
for (i in grids_data) {
  print(na.omit(as.data.frame(i)) %>%
    {
      median(rep(.$edu49_r, times = .$pob1), na.rm = T)
    })
}


# Estimate population density & socio demographic values ------------------

# load function
sociodem_est <- function(yy) {
  myCRSutm <- 4326
  yy <- yy %>%
    st_transform(myCRSutm) %>%
    mutate(
      area_sq_km = as.numeric(st_area(.)) / 1e6, # Area in sq kilometres
      pop_den = pob1 / area_sq_km, # Population density
      edu_decile = ntile(edu49_r, 10), # Education by decile
      edu_pop_comp_15_pct = if_else(pob1 > 0, edu40 / pob1, NA_real_), # Percentage of population 15 or older with complete post-basic education
      eco_pea_pct = if_else(pob1 > 0, eco1 / pob1, NA_real_), # Percentage of economic active population
      eco_po_pct = if_else(pob1 > 0, eco4 / pob1, NA_real_), # Percentage of occupied population
      h_inh_hou_pct = if_else(viv0 > 0, viv1 / viv0, NA_real_), # Percentage of inhabitated houses
      h_water_pct = if_else(viv1 > 0, 1 - (viv17 / viv1), NA_real_), # Percentage of inhabited houses with potable water service
      h_sewage_pct = if_else(viv1 > 0, 1 - (viv23 / viv1), NA_real_), # Percentage of inhabited houses with sewage service
      h_car_pct = if_else(viv1 > 0, 1 - (viv28 / viv1), NA_real_)
    ) %>% # Percentage of of inhabited houses with access to a car or suv
    mutate_at(vars(area_sq_km:h_car_pct), ~ case_when(!is.nan(.x) ~ .x)) %>% # NaN when 0/0, replace NaN with NA
    mutate_at(vars(area_sq_km:h_car_pct), round, 4) %>% # Round to 4 decimals
    dplyr::select(id, everything()) %>%
    st_transform(myCRSlonglat)
  return(yy)
}

# Apply function over all grids
grids_data <- lapply(grids_data, sociodem_est)
glimpse(grids_data)
lapply(grids_data, summary)

# Clean Global Env.
gc(reset = T)

##---------------------------------------------------------------
##                          Section 4                          --
##---------------------------------------------------------------


# Map tests ---------------------------------------------------------------

library(ggstar)

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
states <- st_read("01_Data/marco_geoestadistico_nacional_2014_version_6.2/CONTINUO_NACIONAL/ESTADOS.shp")

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
ggsave("./05_Plots and maps/pop_den_map.png",
  plot = popden_map,
  width = 8, height = 7, bg = "white", dpi = 600
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
ggsave("./05_Plots and maps/edu_decile_map.png",
  plot = edu_map,
  width = 8, height = 7, bg = "white", dpi = 600
)

# # Other variables ---------------------------------------------------------
#
#   # Percentage of houses with access to potable water
#   hist(grids_data[[1]]$h_water_pct)
#   ggplot() +
#     #geom_sf(data = roadnetwork, color="gray70", size = 0.01, alpha = 0.20) +
#     geom_sf(data = filter(grids_data[[1]], pob1 > 0), mapping =  aes(fill = h_water_pct), color= NA, lwd = .00, alpha = 0.60) +
#     scale_fill_distiller(palette="Reds", guide = "colorbar")  +
#     theme_map()
#
#   # Percentage of houses with access to car
#   hist(grids_data[[4]]$h_car_pct)
#   ggplot() +
#     #geom_sf(data = roadnetwork, color="gray70", size = 0.01, alpha = 0.20) +
#     geom_sf(data = filter(grids_data[[4]], pob1 > 0), mapping =  aes(fill = h_car_pct), color= NA, lwd = .00, alpha = 0.75) +
#     scale_fill_viridis_c() + theme_map()



##---------------------------------------------------------------
##                          Section 5                          --
##---------------------------------------------------------------


# Save aggregated data as csv
lapply(names(grids_data), function(x) {
  st_set_geometry(grids_data[[x]], NULL) %>%
    write_csv(
      path = paste0("./04_Outputs/scince_2012/scince_2012_", x, ".csv"),
      append = FALSE
    )
})


# Test load data ----------------------------------------------------------


# Test loading data
# List of directories
grid_csv <- list.files("./04_Outputs/scince_2012/", recursive = T, full.names = T)
# Load CSVs
grid_csv_list <- lapply(grid_csv, read_csv)
# Inspect files
grid_csv_list
glimpse(grid_csv_list)
lapply(grid_csv_list, summary)

grid_csv_list[[1]] %>% filter(is.na(h_water_pct) & viv0 > 0)
grid_csv_list[[1]] %>% filter(is.na(h_water_pct) & viv1 > 0)


# Clean env.
rm(list = ls())
gc()
