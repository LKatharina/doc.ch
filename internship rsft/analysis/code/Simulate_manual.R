# ==============================================================================
# Simulate multiple combinations of parameters (manually) in a decision task 
# assuming random foresting and using bayesian cognitive models
# ==============================================================================


# Clear environment and set working directory-----------------------------------
rm(list = ls())
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load Package------------------------------------------------------------------
pacman::p_load(ggplot2, data.table, tidybayes, patchwork, psych, future, doFuture)
pacman::p_load_gh("cognitivemodels")


# Source Scripts----------------------------------------------------------------
invisible(lapply(list.files("Functions", full = TRUE), source))


# Set Parameters----------------------------------------------------------------
ps1 <- 0.5
s1 <- 6
s2 <- 8
pr1 <- 0.25
r1 <- 1
r2 <- 9
prior <- c(1, 1)

subject_n <- 50
beta_n <- 750
budget <- 18
dfe_n <- c(3, 10)

seed <- 42

ntrials <- 3

# Parallel execution of code: TRUE or FALSE
parallel <- FALSE



# Execute-----------------------------------------------------------------------

# Create dataset with all possible combinations of parameters
paras <- as.data.table(expand.grid(beta_n = beta_n, budget = budget, subject_n = subject_n, ntrials = ntrials, seed = seed, dfe_n = dfe_n))
paras[, `:=` (ps1 = ps1, s1 = s1, s2 = s2, pr1 = pr1, r1 = r1, r2 = r2, para_id = .I, design_id = rep(1:nrow(paras)/2), each = 2)]

# Simulate decisions from experience according to the cognitive bayesian learning model
if (parallel) {
  cores <- min(availableCores(), nrow(paras))
  paras[, core := rep(1:cores, length.out = nrow(paras))]
  registerDoFuture()
  plan(multisession)
  system.time({
    sim1 <- foreach(x = 1:cores, .export = c("Get_Predictions", "Get_Sample", "Get_Model", "data.table", "cognitivemodels", "paras", "cores"), .combine = "rbind") %dorng% {
      paras[core == x, Get_Predictions(s1 = s1, s2 = s2, r1 = r1, r2 = r2, ps1 = ps1, pr1 = pr1, dfe_n = dfe_n, subject_n = subject_n, budget = budget, beta_n = beta_n, seed = seed, ntrials = ntrials, design_id = design_id), by = para_id]
    }
  })
} else {
  system.time({
    sim <- paras[, Get_Predictions(s1 = s1, s2 = s2, r1 = r1, r2 = r2, ps1 = ps1, pr1 = pr1, dfe_n = dfe_n, subject_n = subject_n, budget = budget, beta_n = beta_n, seed = seed, ntrials = ntrials), by = para_id]
  })
}