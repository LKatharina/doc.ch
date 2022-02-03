# ==============================================================================
# Simulate multiple combinations of parameters in a decision task assuming
# random foresting.
# ==============================================================================


# Clear environment and set working directory-----------------------------------
rm(list = ls())
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load Package------------------------------------------------------------------
library(ggplot2)
library(stringr)
library(data.table)
library(cognitivemodels)
library(tidybayes)
library(patchwork)
library(psych)
library(future)
library(doFuture)


# Source Scripts----------------------------------------------------------------
source("Get_Sample.R")
source("Get_Model.R")
source("Get_Comparison.R")
source("Get_Plot.R")
source("Get_Predictions.R")


# Set Parameters----------------------------------------------------------------
ps1 <- 0.5
s1 <- 6
s2 <- 8
pr1 <- 0.25
r1 <- 1
r2 <- 9
prior <- c(1, 1)

subject_n <- c(20, 50)
beta_n <- c(750, 1000)
budget <- c(18, 20)
dfe_n <- c(3, 10)

seed <- 42

ntrials <- 3


# Execution code----------------------------------------------------------------

# Create dataset with all possible combinations of parameters
paras <- as.data.table(expand.grid(beta_n = beta_n, budget = budget, subject_n = subject_n, ntrials = ntrials, seed = seed, dfe_n = dfe_n))
paras[, `:=` (ps1 = ps1, s1 = s1, s2 = s2, pr1 = pr1, r1 = r1, r2 = r2, para_id = .I)]

# Simulate decisions from experience according to the cognitive bayesian learning model
sim <- paras[, Get_Predictions(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed, ntrials), by = para_id]


# Up for debate
b1 <- beta_n[1]
b2 <- beta_n[2]
budg1 <- budget[1]
budg2 <- budget[2]
dfe1 <- dfe_n[1]
dfe2 <- dfe_n[2]
# measure <- NULL
measure <- rbind(measure,
                 cbind(mean_bps1 = c(mean(sim[dfe_id == dfe1 & budget == budg1 & beta_n == b1]$b_ps1),
                                     mean(sim[dfe_id == dfe2 & budget == budg1 & beta_n == b1]$b_ps1),
                                     mean(sim[dfe_id == dfe1 & budget == budg2 & beta_n == b1]$b_ps1),
                                     mean(sim[dfe_id == dfe2 & budget == budg2 & beta_n == b1]$b_ps1),
                                     mean(sim[dfe_id == dfe1 & budget == budg1 & beta_n == b2]$b_ps1),
                                     mean(sim[dfe_id == dfe2 & budget == budg1 & beta_n == b2]$b_ps1),
                                     mean(sim[dfe_id == dfe1 & budget == budg2 & beta_n == b2]$b_ps1),
                                     mean(sim[dfe_id == dfe2 & budget == budg2 & beta_n == b2]$b_ps1)),
                       mean_bps1 = c(mean(sim[dfe_id == dfe1 & budget == budg1 & beta_n == b1]$b_pr1),
                                     mean(sim[dfe_id == dfe2 & budget == budg1 & beta_n == b1]$b_pr1),
                                     mean(sim[dfe_id == dfe1 & budget == budg2 & beta_n == b1]$b_pr1),
                                     mean(sim[dfe_id == dfe2 & budget == budg2 & beta_n == b1]$b_pr1),
                                     mean(sim[dfe_id == dfe1 & budget == budg1 & beta_n == b2]$b_pr1),
                                     mean(sim[dfe_id == dfe2 & budget == budg1 & beta_n == b2]$b_pr1),
                                     mean(sim[dfe_id == dfe1 & budget == budg2 & beta_n == b2]$b_pr1),
                                     mean(sim[dfe_id == dfe2 & budget == budg2 & beta_n == b2]$b_pr1)),
                       expand.grid(beta_n = beta_n, budget = budg, subject_n = subjects, ntrials = ntrials, seed = seed, dfe_n = dfe)))
