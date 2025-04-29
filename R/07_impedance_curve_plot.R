############################################################################
############################################################################
###                                                                      ###
###                       PLOT IMPEDANCE FUNCTIONS                       ###
###                                                                      ###
############################################################################
############################################################################

# This code:
# Creates a plot illustrating accessibility impedance functions

# Preliminary -------------------------------------------------------------

library(tidyverse)

# Define impedance functions
exp_fn <- function(t_ij, beta){exp(-beta * t_ij)}
t_ij <- seq(from = 0, to = 120, by=.1)

# Estimate weights
impedance_curves <-
  data.frame(
    t_ij = t_ij,
    exp_fn1 = exp_fn(t_ij, 0.044 * 0.9),
    exp_fn2 = exp_fn(t_ij, 0.044),
    exp_fn3 = exp_fn(t_ij, 0.044 * 1.1)
  )

# Use a simple text version for the aes mapping
fun_names_simple <- "Neg. exponential"

# Selected functions ------------------------------------------------------
library(ggrepel)
library(tidyr)  # For pivot_longer
library(dplyr)  # For %>% and select
# Curve labels
exp_labs <- 
  impedance_curves[250,] %>% 
  select(t_ij, exp_fn1, exp_fn3) %>% 
  pivot_longer(2:3) %>% 
  mutate(lab = c("Less sensitive", "More sensitive"))
# Plot impedance curves
imp_functions <- 
  impedance_curves %>% 
  ggplot(aes(x=t_ij)) +
  geom_ribbon(
    aes(ymin=exp_fn1, ymax=exp_fn3),
    fill = NA,
    col = 'gray30',
    linetype = 'dashed',
    alpha = 0.4, 
    show.legend = FALSE
  ) +
  geom_line(
    aes(y = exp_fn2, col = fun_names_simple), 
    linewidth = 1.4
  ) +
  labs(
    y = "Impedance weight",
    x = "Travel cost (time)",
    col = "Decay function"
  ) +
  scale_color_viridis_d(
    option = 'turbo', 
    begin = .15, 
    end = .85,
    labels = parse(text = 'paste("Neg. exponential. ", "", paste(beta, " = 0.044. "), "", "Shade for reference +/- 10%")')
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  ) 
imp_functions

# Save plot
ggsave(
  'output/impedance_curves.png', 
  imp_functions, 
  width = 7, height = 4, dpi = 400, bg = "white"
)








