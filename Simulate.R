
# Load packages
library(ggplot2)
library(stringr)
library(data.table)
library(cognitivemodels)
library(tidybayes)
library(patchwork)
library(psych)

# Source scripts
source("get_sample.R")
source("get_model.R")
source("get_comparison.R")
source("get_plot.R")
source("get_predictions.R")

# Definition of all relevant parameters
subjects <- 5
beta_n <- c(5, 10)
ntrials <- 3
seed <- 42
budget <- c(9, 11)
dfe_n <- c(3, 10)



# Execution
paras <- as.data.table(expand.grid(beta_n = beta_n, budget = budget, subject_n = subjects, ntrials = ntrials, seed = seed, dfe_n = dfe))
paras[, `:=` (ps1 = 0.5, s1 = 1, s2 = 2, pr1 = 0.8, r1 = 0, r2 = 5, para_id = .I)]

sim <- paras[1, get_predictions(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed, ntrials), by = para_id]


# desc <- c(describeBy(sim[, b_ps1], group = c(sim[, dfe_id], sim[, budget])), describeBy(sim[, b_pr1], group = sim[, dfe_id]))

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

# measures <- rbind(measures, 
#                   data.table(mean_bps1 = mean(sim[dfe_id == dfe[1] & budget == budg[1] & beta_n == beta[1]]$b_ps1),
#                              mean_bpr1 = mean(sim[dfe_id == dfe[1] & budget == budg[1] & beta_n == beta[1]]$b_pr1),
#                              beta_n = beta[1],
#                              subject_n = subject_n,
#                              dfe_n = dfe_n[1],
#                              budget = budg[1],
#                              ps1 = sim$ps1[1],
#                              s1 = sim$s1[1],
#                              s2 = sim$s2[1],
#                              pr1 = sim$pr1[1],
#                              r1 = sim$r1[1],
#                              r2 = sim$r2[1]),
#                   data.table(mean_bps1 = mean(sim[dfe_id == dfe[2] & budget == budg[1] & beta_n == beta[1]]$b_ps1),
#                              mean_bpr1 = mean(sim[dfe_id == dfe[2] & budget == budg[1] & beta_n == beta[1]]$b_pr1),
#                              beta_n = beta[1],
#                              subject_n = subject_n,
#                              dfe_n = dfe[2],
#                              budget = budg[1],
#                              ps1 = sim$ps1[1],
#                              s1 = sim$s1[1],
#                              s2 = sim$s2[1],
#                              pr1 = sim$pr1[1],
#                              r1 = sim$r1[1],
#                              r2 = sim$r2[1]),
#                   data.table(mean_bps1 = mean(sim[dfe_id == dfe[1] & budget == budg[2] & beta_n == beta[1]]$b_ps1),
#                              mean_bpr1 = mean(sim[dfe_id == dfe[1] & budget == budg[2] & beta_n == beta[1]]$b_pr1),
#                              beta_n = beta[1],
#                              subject_n = subject_n,
#                              dfe_n = dfe[1],
#                              budget = budg[2],
#                              ps1 = sim$ps1[1],
#                              s1 = sim$s1[1],
#                              s2 = sim$s2[1],
#                              pr1 = sim$pr1[1],
#                              r1 = sim$r1[1],
#                              r2 = sim$r2[1]),
#                   data.table(mean_bps1 = mean(sim[dfe_id == dfe[2] & budget == budg[2] & beta_n == beta[1]]$b_ps1),
#                              mean_bpr1 = mean(sim[dfe_id == dfe[2] & budget == budg[2] & beta_n == beta[1]]$b_pr1),
#                              beta_n = beta[1],
#                              subject_n = subject_n,
#                              dfe_n = dfe[2],
#                              budget = budg[2],
#                              ps1 = sim$ps1[1],
#                              s1 = sim$s1[1],
#                              s2 = sim$s2[1],
#                              pr1 = sim$pr1[1],
#                              r1 = sim$r1[1],
#                              r2 = sim$r2[1]),
#                   data.table(mean_bps1 = mean(sim[dfe_id == dfe[1] & budget == budg[1] & beta_n == beta[2]]$b_ps1),
#                              mean_bpr1 = mean(sim[dfe_id == dfe[1] & budget == budg[1] & beta_n == beta[2]]$b_pr1),
#                              beta_n = beta[2],
#                              subject_n = subject_n,
#                              dfe_n = dfe[1],
#                              budget = budg[1],
#                              ps1 = sim$ps1[1],
#                              s1 = sim$s1[1],
#                              s2 = sim$s2[1],
#                              pr1 = sim$pr1[1],
#                              r1 = sim$r1[1],
#                              r2 = sim$r2[1]),
#                   data.table(mean_bps1 = mean(sim[dfe_id == dfe[2] & budget == budg[1] & beta_n == beta[2]]$b_ps1),
#                              mean_bpr1 = mean(sim[dfe_id == dfe[2] & budget == budg[1] & beta_n == beta[2]]$b_pr1),
#                              beta_n = beta[2],
#                              subject_n = subject_n,
#                              dfe_n = dfe[2],
#                              budget = budg[1],
#                              ps1 = sim$ps1[1],
#                              s1 = sim$s1[1],
#                              s2 = sim$s2[1],
#                              pr1 = sim$pr1[1],
#                              r1 = sim$r1[1],
#                              r2 = sim$r2[1]),
#                   data.table(mean_bps1 = mean(sim[dfe_id == dfe[1] & budget == budg[2] & beta_n == beta[2]]$b_ps1),
#                              mean_bpr1 = mean(sim[dfe_id == dfe[1] & budget == budg[2] & beta_n == beta[2]]$b_pr1),
#                              beta_n = beta[2],
#                              subject_n = subject_n,
#                              dfe_n = dfe[1],
#                              budget = budg[2],
#                              ps1 = sim$ps1[1],
#                              s1 = sim$s1[1],
#                              s2 = sim$s2[1],
#                              pr1 = sim$pr1[1],
#                              r1 = sim$r1[1],
#                              r2 = sim$r2[1]),
#                   data.table(mean_bps1 = mean(sim[dfe_id == dfe[2] & budget == budg[2] & beta_n == beta[2]]$b_ps1),
#                              mean_bpr1 = mean(sim[dfe_id == dfe[2] & budget == budg[2] & beta_n == beta[2]]$b_pr1),
#                              beta_n = beta[2],
#                              subject_n = subject_n,
#                              dfe_n = dfe[2],
#                              budget = budg[2],
#                              ps1 = sim$ps1[1],
#                              s1 = sim$s1[1],
#                              s2 = sim$s2[1],
#                              pr1 = sim$pr1[1],
#                              r1 = sim$r1[1],
#                              r2 = sim$r2[1]))
#                   
#                   