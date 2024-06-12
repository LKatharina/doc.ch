# ==============================================================================
# Compare different environments/designs and visualize the most promising ones
# ==============================================================================


# Set working directory---------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load packages-----------------------------------------------------------------
pacman::p_load(ggplot2, data.table, bayesplot, tidybayes)


# Source all scripts in the folder Functions------------------------------------
invisible(lapply(list.files("Functions", full = TRUE), source))


# Load data---------------------------------------------------------------------
sim <- readRDS(file = "../data/sim_1-500.RDS")


# Execute-----------------------------------------------------------------------
dfe_comp <- Get_Comparison(df = sim)

# Top 3 designs for plotting
sim_top3 <- sim[env_id %in% dfe_comp[1:3,]$env_id,]
sim_top3[, dfe_id := as.character(dfe_id)]

# Plot top 3 designs
Get_Plot(df = sim_top3[env_id == unique(dfe_comp$env_id)[1],], wrapby = ~trial + state)
Get_Plot(df = sim_top3[env_id == unique(dfe_comp$env_id)[2],], wrapby = ~trial)
Get_Plot(df = sim_top3[env_id == unique(dfe_comp$env_id)[3],], wrapby = ~trial)
