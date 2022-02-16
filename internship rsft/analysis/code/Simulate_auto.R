# ==============================================================================
# Simulate multiple combinations of parameters in a decision task assuming
# random foresting and using bayesian cognitive models.
# ==============================================================================


# Clear environment and set working directory-----------------------------------
rm(list = ls())
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load packages-----------------------------------------------------------------
pacman::p_load(data.table, tidybayes, patchwork, psych, future, doFuture)
library(cognitivemodels)


# Source all scripts in the folder Functions------------------------------------
lapply(list.files("Functions", full = TRUE), source)


# Set Parameters----------------------------------------------------------------

# General parameters
subject_n <- 5
beta_n <- 100
dfe_n <- c(3, 10)
seed <- 42
trial_n <- 3

# All outcomes for the first gamble
`1st_gamble` <- c(1:8)
# Probabilities for first gamble
probs_1st <- seq(0.1, 0.9, 0.1)
# Minimal variance between the two gambles
min_var <- 5

# Parallel execution of code: TRUE or FALSE
parallel <- FALSE


# Create stimuli and parameter dataframes---------------------------------------

stimuli <- Get_Stimuli(rewards_x = `1st_gamble`, rewards_y = c(`1st_gamble`, max(`1st_gamble`)+1), probs_x = probs_1st, min_var = min_var, ntrial = trial_n)
paras <- Get_Paras(dfe_n = dfe_n, subject_n = subject_n, beta_n = beta_n, seed = seed, trial_n = trial_n)
stimuli_paras <- cbind(stimuli[rep(1:.N, each = nrow(paras)),], paras[rep(1:.N, times = nrow(stimuli)),])
stimuli_paras[, para_design_id := .I]
stimuli_paras <- stimuli_paras[1:500,]

# Execute-----------------------------------------------------------------------

# Simulate decisions from experience according to the cognitive bayesian learning model
if (parallel) {
  cores <- min(availableCores(), nrow(stimuli_paras))
  stimuli_paras[, core := rep(1:cores, length.out = nrow(stimuli_paras))] 
  registerDoFuture()
  plan(multisession)
  sim <- foreach(x = 1:cores, .export = c("Get_Predictions", "Get_Sample", "Get_Model", "data.table", "cognitivemodels", "stimuli_paras", "cores"), .combine = "rbind") %dorng% {
    stimuli_paras[core == x, Get_Predictions(s1 = s1, s2 = s2, r1 = r1, r2 = r2, ps1 = ps1, pr1 = pr1, dfe_n = dfe_n, subject_n = subject_n, 
                                             budget = budget, beta_n = beta_n, seed = seed, ntrials = ntrials, design_id = design_id), 
                  by = para_design_id]
  }
} else {
  system.time({
    sim <- stimuli_paras[, Get_Predictions(s1 = s1, s2 = s2, r1 = r1, r2 = r2, ps1 = ps1, pr1 = pr1, dfe_n = dfe_n, subject_n = subject_n, 
                                           budget = budget, beta_n = beta_n, seed = seed, ntrials = ntrials, design_id = design_id), 
                         by = para_design_id]
  })
}

setwd("..")
saveRDS(object = sim, file = "data/sim_1_500.RDS")


# lapply(dfe_n, function(z) {stimuli[, Get_Predictions(s1 = s1, s2 = s2, r1 = r1, r2 = r2, ps1 = ps1, pr1 = pr1, dfe_n = z, subject_n = subject_n, budget = budget, beta_n = beta_n, seed = seed, ntrials = trial_n, design_id = design_id), by = design_id]})

