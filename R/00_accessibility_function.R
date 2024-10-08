############################################################################
############################################################################
###                                                                      ###
###                        ACCESSIBILITY FUNCTION                        ###
###                                                                      ###
############################################################################
############################################################################

# This function calculates accessibility for three types of decaying function:
# 1) cumulative, 2) inverse power, and 3) negative exponential.
# It can consider a mu parameter as in Thorsen & Gitlesen (1998).
# Additionally, can consider access to opportunities by type "k" (Pereira, 2019)

# Date: 29/03/2021
# Author: Rafael Verduzco

# Set environment ---------------------------------------------------------

# Load packages
library(data.table)
setDTthreads(0) # Max threads
library(tidyverse)

# Decaying functions ------------------------------------------------------

# Cumulative function
cum_fn <- function(d_ij, w, beta, ...) {
  w * ifelse(d_ij <= beta, 1, 0)
}
# Inverse power
pow_fn <- function(d_ij, w, gamma = 1, beta, mu = 0, delta_ij = 0) {
  w^gamma * d_ij^(-beta + mu * delta_ij)
}
# Negative exponential
exp_fn <- function(d_ij, w, gamma = 1, beta, mu = 0, delta_ij = 0) {
  w^gamma * exp(-beta * d_ij + mu * delta_ij)
}

# Generic accessibility function ------------------------------------------

# Load function to compute access by origin
accessibility <-
  function(ttm, fn, beta,
           mu = 0, gamma = 1,
           w = "dest_est_pe_oc",
           opportunity = "all") {
    # TTM as data table
    if (!is.data.table(ttm)) {
      ttm <- as.data.table(ttm)
    }
    # Rename variable containing weights
    names(ttm)[which(names(ttm) == w)] <- "dest_w"

    # Choose decaying fn:
    if (fn == "cum") {
      # Cumulative
      dec_fn <- cum_fn
    } else if (fn == "pow") {
      # Inv. power
      dec_fn <- pow_fn
    } else if (fn == "exp") {
      # Neg. exponential
      dec_fn <- exp_fn
    } else {
      stop("Unkown decaying function")
    }

    # Check if it includes mu parameter
    if (mu != 0 & fn != "cum") {
      # Compute delta
      ttm[, delta := fifelse(from_id == to_id, 1, 0)]
      # Compute access weights for each OD pair considering mu and delta
      ttm[, access_w := dec_fn(d_ij = travel_time, w = dest_w, gamma = gamma, beta = beta, mu = mu, delta_ij = delta)]
    } else {
      # Compute access weights for each OD pair
      ttm[, access_w := dec_fn(d_ij = travel_time, w = dest_w, gamma = gamma, beta = beta)]
    }

    # Summarise weighted opportunities
    if (opportunity == "all") {
      # Summarise access by origin
      access_dt <- ttm[, list(accessibility = sum(access_w, na.rm = T)),
        by = .(from_id)
      ]
    } else if (opportunity == "k") {
      # Make available opportunities if i = j
      ttm[, dest_inc_decile := fifelse(from_id == to_id, or_edu_decile, dest_inc_decile)]
      # Summarise by origin and k
      # If type of origin is NA, then access is NA
      access_dt <- ttm[!is.na(or_edu_decile),
        list(
          accessibility = sum(access_w, na.rm = T),
          or_dec = first(or_edu_decile)
        ),
        by = .(from_id, dest_inc_decile)
      ]
      # Subset access weights that match k in O and D and summarise by origin
      access_dt <- access_dt[or_dec == dest_inc_decile,
        .(accessibility = sum(accessibility, na.rm = T)),
        by = .(from_id)
      ]
    } else {
      stop("Unkown opportunity type")
    }

    # Return results
    return(access_dt)
  }
