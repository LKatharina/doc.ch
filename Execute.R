
# Load packages
library(ggplot2)
library(stringr)
library(data.table)
library(cognitivemodels)
library(tidybayes)

# Definition of all relevant parameters
S1 <- 1
S2 <- 2
R1 <- 0
R2 <- 5
PS1 <- 0.5
PR1 <- 0.8

Dfe_n <- c(3, 10)
Budget <- c(9, 11)
Subject_n <- 5

Beta_n <- 200
Ntrials <- 3

Seed <- 42


# Let it run!
source("Sample&Model.R")

d1 <- get_sample(s1 = S1, s2 = S2,
                 r1 = R1, r2 = R2,
                 ps1 = PS1, pr1 = PR1,
                 dfe_n = Dfe_n,
                 subject_n = Subject_n,
                 budget = Budget,
                 beta_n = Beta_n,
                 seed = Seed)

d2 <- get_model(df = d1, ntrials = Ntrials) 

source("Compare&Plot.R")

comparison <- get_comparison(df = d2)

p1 <- get_plot(unique(d2[budget == Budget[1]]$plot_id))
p2 <- get_plot(unique(d2[budget == Budget[2]]$plot_id))


