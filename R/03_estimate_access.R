############################################################################
############################################################################
###                                                                      ###
###                    ESTIMATE ACCESSIBILITY METRICS                    ###
###                                                                      ###
############################################################################
############################################################################

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

ttm_base_path <- "data/ttm_mexicocity//"

# create directory to store temp results
out <- "data/accessibility_r5r2/"
dir.create(out)

# Read data at origin and destination -------------------------------------

# Read employment data (destination)
data_destination <- list.files("data/censo_ec_2014", full.names = T)[1:5] %>% 
  lapply(fread) %>%
  map(select, -est_remun_tot:-oc_pe_den_sq_km) %>%
  map(rename_at, vars(-id), ~ paste0("dest_", .)) %>%
  setNames(grids)
data_destination <- rbindlist(data_destination, idcol = 'grid')

# Read population census data (origin)
data_origin <-list.files("data/scince_2012/", full.names = T) %>% 
  lapply(fread) %>%
  map(select, id, edu49_r, edu_decile) %>%
  map(rename_at, vars(-id), ~ paste0("or_", .)) %>%
  setNames(grids)
data_origin <- rbindlist(data_origin, idcol = 'grid')

# Recalculate matching ntiles between education and employment salary?
ntiles <- 5
data_destination[, dest_inc_decile := ntile(dest_avg_income, ntiles), by = "grid"]
data_origin[, or_edu_decile := ntile(or_edu49_r, ntiles), by = "grid"]

# Within-zone travel time
intra_ttm <- data.table::fread('data/ttm_mexicocity/intra_tt.csv')


# Parameters --------------------------------------------------------------

# Define accessibility parameters to estimate
acc_pars <- tribble(
  ~dec_fn, ~beta, ~mu, ~opportunity, ~mode,
  "cum", 60, 0, "all", "pt",
  "exp", 0.044, 1.654, "all", "pt",
  "exp", 0.044, 1.654, "k", "pt",
  "exp", 0.031, 1.662, "all", "pt",
  "exp", 0.031, 1.662, "k", "pt",
  "exp", 0.085, 1.654, "all", "car",
  "exp", 0.085, 1.654, "k", "car"
)
write_csv(acc_pars, "data/accessibility/access_pars.csv")

# Public transport --------------------------------------------------------

# Find files for PT TTM
# These travel times correspond to the new estimates by r5r, 
# and not those used in PhD originally estimated using OpenTripPlanner
ttm_paths_pt <- list.files('data/ttm_mexicocity/ttm_pt/', full.names = TRUE)


# Run loop for each TTM
for (ttm_n in ttm_paths_pt) {
  
  # Read TTM for grid_n and router_n
  ttm <- data.table::fread(ttm_n)
  
  # Join data to ttm
  # Education data
  ttm[
    data_origin, 
    on = c(grid = "grid", from_id = "id"), 
    or_edu_decile := or_edu_decile
  ]
  # Economic census data
  ttm[
    data_destination, 
    on = c(grid = "grid", to_id = "id"),
    c("dest_est_ec_un", "dest_est_pe_oc", "dest_inc_decile") :=
        list(dest_est_ec_un, dest_est_pe_oc, dest_inc_decile)
  ]
  # Within-zone data
  ttm[
    intra_ttm, 
    on = c(grid = "grid", from_id = "id"),
    intra_tt_walk := intra_tt_walk
  ]
  ttm[, travel_time := fifelse(
    from_id == to_id, 
    intra_tt_walk, 
    travel_time_p50)
  ]
  
  
  # Apply access function for different parameters
  access_all_par <- lapply(1:sum(acc_pars$mode == "pt"), function(i) {
    
    # Estimate accessibility
    access_origin <- accessibility(
      ttm = ttm,
      w = "dest_est_pe_oc",
      fn = acc_pars$dec_fn[i],
      beta = acc_pars$beta[i],
      mu = acc_pars$mu[i],
      opportunity = acc_pars$opportunity[i], 
      local_match = FALSE
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
  # Assign router
  access_all_par[, router := unique(ttm$router)]
  # Assign grid
  access_all_par[, grid := unique(ttm$grid)]
  
  # Save result by grid
  write_csv(
    access_all_par, 
    paste0(out, "accessibility_", unique(ttm$grid), "_", unique(ttm$router), "_pt.csv")
  )
  
  # Clean env.
  rm(ttm, access_all_par)
  gc()
}
  
  

# Accessibility by car ----------------------------------------------------

# TTM paths by car
ttm_paths_car <-
  list.files('data/ttm_adjusted', recursive = TRUE, pattern = "car", full.names = TRUE)


# Run loop for each TTM
for (ttm_n in ttm_paths_car) {
  # Set grid
  current_grid <- strsplit(ttm_n, "/")[[1]][3]
  
  # Read TTM for grid_n and router_n
  ttm <- read_rds(ttm_n)
  
  # Assign grid
  ttm[,grid := current_grid]
  # Rename origin destination columns
  setnames(ttm, old = c("origin", "destination"), new = c("from_id", "to_id"))
  
  # Join data to ttm
  # Education data
  ttm[
    data_origin, 
    on = c(grid = "grid", from_id = "id"), 
    or_edu_decile := or_edu_decile
  ]
  # Economic census data
  ttm[
    data_destination, 
    on = c(grid = "grid", to_id = "id"),
    c("dest_est_ec_un", "dest_est_pe_oc", "dest_inc_decile") :=
      list(dest_est_ec_un, dest_est_pe_oc, dest_inc_decile)
  ]
  
  # limit parameters for car
  acc_pars_car <- acc_pars %>% filter(mode == 'car')
  # Apply access function for different parameters
  access_all_par <- lapply(1:nrow(acc_pars_car), function(i) {
    
    # Estimate accessibility
    access_origin <- accessibility(
      ttm = ttm,
      w = "dest_est_pe_oc",
      fn = acc_pars_car$dec_fn[i],
      beta = acc_pars_car$beta[i],
      mu = acc_pars_car$mu[i],
      opportunity = acc_pars_car$opportunity[i],
      local_match = FALSE
    )
    
    # Assign parameters used for estimates
    access_origin[, dec_fn := acc_pars_car$dec_fn[i]]
    access_origin[, beta := acc_pars_car$beta[i]]
    access_origin[, mu := acc_pars_car$mu[i]]
    access_origin[, opportunity := acc_pars_car$opportunity[i]]
    
    # Return result
    return(access_origin)
  })
  
  
  # Bind rows
  access_all_par <- rbindlist(access_all_par)
  
  # Assign grid
  access_all_par[, grid := current_grid]
  
  # Save result by grid
  write_csv(
    x = access_all_par, 
    file = paste0(out, "accessibility_", current_grid, "_car.csv"), 
    append = FALSE
  )
  
  # Clean env.
  rm(ttm, access_all_par)
  gc()
}



# Clean env.
rm(list = ls())
gc()
