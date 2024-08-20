############################################################################
############################################################################
###                                                                      ###
###                    ESTIMATE ACCESSIBILITY METRICS                    ###
###                                                                      ###
############################################################################
############################################################################

# Date: 2024-08-19
# Author: Rafael Verduzco

# This code comes from '01_census_access/R/9.2_estimate_access'.
# This code applies the accessibility function to estimate various accessibility metrics.

# For public transport:
# It applies the function for different grids, routers (scenarios), and parameters
# For car:
# It applies the function for different grids.

# Set environment ---------------------------------------------------------

# Load packages
library(data.table)
setDTthreads(0) # Max threads
library(tidyverse)
# Load accessibility function
source("R/00_accessibility_function.R")

# Define directories ------------------------------------------------------

# original_dir <- "D:/student_onedrive/OneDrive - University of Glasgow/Research/5.Process and analyses/9. Phd_thesis/01_census_access/"
#grids <- basename(list.dirs("04_Outputs/ttm_post_otp/", recursive = F))
grids <- c("grid_1000", "grid_2000", "grid_4000", "grid_500",  "postcodes")
grids <- setNames(grids, grids)

ttm_base_path <- "data/ttm_adjusted/"

# create directory to store temp results
out <- "data/accessibility/"
dir.create(out)

# Read data at origin and destination -------------------------------------

# Read employment data (destination)
data_destination <-
  lapply(list.files("data/censo_ec_2014", full.names = T)[1:5], fread) %>%
  map(select, -est_remun_tot:-oc_pe_den_sq_km) %>%
  map(rename_at, vars(-id), ~ paste0("dest_", .)) %>%
  setNames(grids)

# Read population census data (origin)
data_origin <-
  lapply(list.files("data/scince_2012/", full.names = T), fread) %>%
  map(select, id, edu49_r, edu_decile) %>%
  map(rename_at, vars(-id), ~ paste0("or_", .)) %>%
  setNames(grids)

# Recalculate matching ntiles between education and employment salary?
ntiles <- 5
map(data_destination, ~ .x[, dest_inc_decile := ntile(dest_avg_income, ntiles)])
map(data_origin, ~ .x[, or_edu_decile := ntile(or_edu49_r, ntiles)])



# Parameters --------------------------------------------------------------

# Define accessibility parameters to estimate
acc_pars <- tribble(
  ~dec_fn, ~beta, ~mu, ~opportunity, ~mode,
  "cum", 60, 0, "all", "pt",
  "exp", 0.044, 1.654, "all", "pt",
  "exp", 0.044, 1.654, "k", "pt",
  "exp", 0.031, 1.662, "all", "pt",
  "exp", 0.031, 1.662, "k", "pt",
  "exp", 0.085, 0, "all", "car"
)
write_csv(acc_pars, "data/accessibility/access_pars.csv")

# Public transport --------------------------------------------------------


# Public transport routers
routers <- c(
  "before_2010",
  "2010_2012",
  "2012_2013",
  "2013_2015",
  "2015_2016",
  "2016_2018",
  "2018_2019"
)

# TTM paths
ttm_paths_pt <- 
  map(
    paste0(ttm_base_path, grids),
    list.files,
    full.names = TRUE, 
    pattern = "pt"
  ) %>%
  setNames(grids)

grid_n <- 5
router_n <- 1

# Run loop
# Loop level 1- Grids 1-5
for (grid_n in 1:length(grids)) {
  
  # Index
  print(paste("Processing grid", grid_n, "of", length(grids)))
  # Initialize list for current grid
  access_by_grid <- list()
  
  # Loop level 2 - Routers in current grid, 1-7
  for (router_n in 1:length(routers)) {
    # Router index
    print(paste("Router", router_n, "of", length(routers)))

    # Read TTM for grid_n and router_n
    ttm <- readRDS(ttm_paths_pt[[grid_n]][router_n])
    # Join data to ttm
    ttm[data_origin[[grid_n]], on = c(origin = "id"), or_edu_decile := or_edu_decile]
    ttm[data_destination[[grid_n]],
      on = c(destination = "id"),
      c("dest_est_ec_un", "dest_est_pe_oc", "dest_inc_decile") :=
        list(dest_est_ec_un, dest_est_pe_oc, dest_inc_decile)
    ]

    # Apply access function for different parameters
    access_all_par <- lapply(1:sum(acc_pars$mode == "pt"), function(i) {
      # Estimate accessibility
      access_origin <-
        accessibility(ttm,
          w = "dest_est_pe_oc",
          fn = acc_pars$dec_fn[i],
          beta = acc_pars$beta[i],
          mu = acc_pars$mu[i],
          opportunity = acc_pars$opportunity[i]
        )
      # Assign parameters used for estimates
      access_origin[, dec_fn := acc_pars$dec_fn[i]]
      access_origin[, beta := acc_pars$beta[i]]
      access_origin[, mu := acc_pars$mu[i]]
      access_origin[, opportunity := acc_pars$opportunity[i]]
      # Return result
      return(access_origin)
    })

    lapply(access_all_par, summary)


    # Bind rows
    access_all_par <- rbindlist(access_all_par)
    # Assign router
    access_all_par[, router := routers[router_n]]
    # Store results in list
    access_by_grid[[routers[router_n]]] <- access_all_par

    # Clean env.
    rm(ttm, access_all_par)
    gc()
  }

  # Bind grid results
  access_by_grid <- rbindlist(access_by_grid)
  # Save result by grid
  write_csv(
    access_by_grid, 
    paste0(out, "accessibility_", grids[grid_n], "_pt.csv")
  )
  
  # Clean env.
  rm(access_by_grid)
  gc()
}

# Accessibility by car ----------------------------------------------------

# TTM paths by car
ttm_paths_car <-
  list.files(ttm_base_path, recursive = TRUE, pattern = "car", full.names = TRUE)

# Run loop for each grid
for (grid_n in 1:length(grids)) {
  # Index
  print(paste("Processing grid", grid_n, "of", length(grids)))

  # Read TTM for grid_n
  ttm <- readRDS(ttm_paths_car[grid_n])
  # Join data to ttm
  ttm[data_origin[[grid_n]], on = c(origin = "id"), or_edu_decile := or_edu_decile]
  ttm[data_destination[[grid_n]],
    on = c(destination = "id"),
    c("dest_est_ec_un", "dest_est_pe_oc", "dest_inc_decile") :=
      list(dest_est_ec_un, dest_est_pe_oc, dest_inc_decile)
  ]

  # Apply access function for different parameters
  access_all_par <- lapply(6, function(i) {
    # Estimate accessibility
    access_origin <-
      accessibility(ttm,
        w = "dest_est_pe_oc",
        fn = acc_pars$dec_fn[i],
        beta = acc_pars$beta[i],
        mu = acc_pars$mu[i],
        opportunity = acc_pars$opportunity[i]
      )
    # Assign parameters used for estimates
    access_origin[, dec_fn := acc_pars$dec_fn[i]]
    access_origin[, beta := acc_pars$beta[i]]
    access_origin[, mu := acc_pars$mu[i]]
    access_origin[, opportunity := acc_pars$opportunity[i]]
    # Return result
    return(access_origin)
  })

  # Bind rows
  access_all_par <- rbindlist(access_all_par)
  # Save result by grid
  write_csv(access_all_par, paste0(out, "accessibility_", grids[grid_n], "_car.csv"))

  # Clean env.
  rm(ttm, access_all_par)
  gc()
}

# Clean env.
rm(list = ls())
gc()
