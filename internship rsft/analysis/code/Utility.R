# ==============================================================================
# Compare different environments/designs and visualize the most promising ones.
# 
# ==============================================================================


# Set working directory---------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load packages-----------------------------------------------------------------
pacman::p_load(ggplot2, data.table)


# Source all scripts in the folder Functions------------------------------------
lapply(list.files("Functions", full = TRUE), source)


# Execute-----------------------------------------------------------------------
dfe_comp_p <- Get_Comparison(df = sim)
dfe_comp <- unique(dfe_comp_p[, .(absolute_diff, percentage_diff, significant_diff, utility),
                              by = .(env_id)])
dfe_comp <- dfe_comp[order(-utility)]

dfe_comp_p[, Get_Plot(trial = trial, decision = decision, dfe_id = as.factor(dfe_id), budget = budget, beta_id = beta_n, subject_id = subject_n),
           by = budget]