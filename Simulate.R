
# Load packages
library(ggplot2)
library(stringr)
library(data.table)
library(cognitivemodels)
library(tidybayes)
library(patchwork)

source("Sample&Model.R")
source("Compare&Plot.R")
source("get_predicitions.R")

# Definition of all relevant parameters
subject_n <- 5
beta_n <- c(1, 2)
ntrials <- 3
seed <- c(42, 43)
budget <- c(9, 11)
dfe_n <- c(3, 10)



# Execution
paras <- as.data.table(expand.grid(beta_n = beta_n, budget = budget, subject_n = subject_n, ntrials = ntrials, seed = seed, dfe_n = dfe_n))
paras[, `:=` (s1 = 1, ps1 = 0.5, s2 = 2, r1 = 0, pr1 = 0.8, r2 = 5, para_id = .I)]

sim <- paras[, get_predictions(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed, ntrials), by = para_id]


