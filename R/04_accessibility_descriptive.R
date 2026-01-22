

# This script corresponds to the analysis. Ch. 6, titled:
# In essence, it is an empirical review of PT accessibility in GMC.

# This code:
# 1. focuses on descriptive statistics

# Author: Rafael Verduzco

# Set environment ---------------------------------------------------------

# Load packages
library(data.table)
setDTthreads(0) # Max threads
library(tidyverse)


# Read data and preliminary transform -------------------------------------


# Grid names
grids <- c("grid_1000", "grid_2000", "grid_4000", "grid_500",  "postcodes")
# Accessibility estimates for public transport
access_pt <- 
  list.files("data/accessibility_r5r2/", full.names = TRUE, pattern = "pt") %>%
  map(fread) %>%
  bind_rows()

# Accessibility estimate for car
access_car <- 
  list.files("data/accessibility_r5r2/", full.names = TRUE, pattern = "car") %>%
  map(fread) %>%
  setNames(grids)


# Define routers (temporal stages) for public transport
routers <- unique(access_pt$postcodes$router)
router_labels <- str_to_title(gsub("_", " ", routers))


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


# Limit PT accessibility  to exponential and bind in a single DF
access_pt <- access_pt %>%
  # bind_rows(.id = "grid") %>%
  filter(dec_fn == "exp" & beta != 0.031) 

# Bind Car accessibility
access_car <- access_car %>%
  bind_rows(.id = "grid") 

## ---------------------------------------------------------------
##                      Descriptive stats                      --
## ---------------------------------------------------------------


# Descriptive statistics --------------------------------------------------

# Table, horizontal version

# Car
desc_stats_car2 <- access_car %>%
  group_by(grid, opportunity) %>%
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
    ),
    opportunity = factor(opportunity, c("all", "k"), c("All", "Matching"))
  ) %>%
  select(grid, opportunity, access_sum) %>%
  arrange(grid) %>%
  pivot_wider(names_from = grid, values_from = access_sum) %>% 
  mutate(router = 'Car')

# PT
desc_stats2 <- access_pt %>%
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

# INCLUDE NUMBER OF MISSING IN MATCHING?

# N. observations below SAS 
observations <- access_pt %>% 
  group_by(grid) %>% 
  summarise(
    n = n_distinct(from_id),
    n = format2(n, 0)
  ) %>% 
  pull(n)
col_names <- paste0(names(grid_labs), " (N=", observations[c(4, 1:3, 5)], ")")
names(desc_stats2) <- c("Type", "Stage", col_names)

desc_stats2

# Save Table: desc. stats
write_csv(desc_stats2, "output//exploratory_rq1/desc_stats2.csv")


# Boxplot accessibility distribution --------------------------------------

# Dodge summary geoms
dodge <- position_dodge(width = 0.65)

# Distribution in boxplot
access_dist_boxplot <- access_pt %>%
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


