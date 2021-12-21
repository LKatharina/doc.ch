
# Load packages
library(ggplot2)
library(stringr)
library(data.table)
library(cognitivemodels)
library(tidybayes)
library(patchwork)

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
source("Compare&Plot.R")

d1 <- get_sample(s1 = S1, s2 = S2,
                 r1 = R1, r2 = R2,
                 ps1 = PS1, pr1 = PR1,
                 dfe_n = Dfe_n,
                 subject_n = Subject_n,
                 budget = Budget,
                 beta_n = Beta_n,
                 seed = Seed)

d2 <- get_model(df = d1, ntrials = Ntrials) 

comparison <- get_comparison(df = d2)

p1 <- get_plot(unique(d2[budget == Budget[1]]$plot_id))
p2 <- get_plot(unique(d2[budget == Budget[2]]$plot_id))

p1 + plot_spacer() + p2 +
  plot_layout(guides = "collect", widths = c(.4,.05,.4)) +
  plot_annotation(caption = "Note: Points = Median, thick line = 66% quantile intervall, thin line = 95% quantile intervall")

plot_name <-  paste0(Beta_n, "_", Dfe_n[1], "-", Dfe_n[2], "_", Budget[1], "-", Budget[2], "_", Subject_n, "_", 
                     S1, "_", S2, "_", R1, "_", R2, "_", PS1, "_", PR1)

ggsave(filename = paste0("/figures/", plot_name, ".png"), dpi = 800)



