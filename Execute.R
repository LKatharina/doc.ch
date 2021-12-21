
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


values <- data.table(S1 = rep(1, 10),
                     S2 = rep(2, 10),
                     R1 = rep(0, 10),
                     R2 = rep(5, 10),
                     PS1 = rep(0.5, 10),
                     PR1 = rep(0.8, 10),
                     Dfe_1 = rep(3, 10),
                     Dfe_2 = rep(10, 10),
                     Budget_1 = rep(9, 10),
                     Budget_2 = rep(11, 10),
                     Subject_n = rep(5, 10),
                     Beta_n = rep(200, 10),
                     Ntrials = rep(3, 10),
                     Seed = rep(42, 10))

# Let it run!
source("Sample&Model.R")
source("Compare&Plot.R")

get_predictions <- function(values) {
d1 <- get_sample(s1 = values$S1, s2 = values$S2,
                 r1 = values$R1, r2 = values$R2,
                 ps1 = values$PS1, pr1 = values$PR1,
                 dfe_n = c(values$Dfe_1, values$Dfe_2),
                 subject_n = values$Subject_n,
                 budget = c(values$Budget, values$Budget_2),
                 beta_n = values$Beta_n,
                 seed = values$Seed)



d2 <- get_model(df = d1, ntrials = values$Ntrials) 

comparison <- get_comparison(df = d2)

p1 <- get_plot(unique(d2[budget == values$Budget_1]$plot_id))
p2 <- get_plot(unique(d2[budget == values$Budget_2]$plot_id))

p1 + plot_spacer() + p2 +
  plot_layout(guides = "collect", widths = c(.4,.05,.4)) +
  plot_annotation(caption = "Note: Points = Median, thick line = 66% quantile intervall, thin line = 95% quantile intervall")

plot_name <-  paste0(Beta_n, "_", Dfe_n[1], "-", Dfe_n[2], "_", Budget[1], "-", Budget[2], "_", Subject_n, "_", 
                     S1, "_", S2, "_", R1, "_", R2, "_", PS1, "_", PR1)

ggsave(filename = paste0("/figures/", plot_name, ".png"), dpi = 800)
}

get_predictions(values = values)
