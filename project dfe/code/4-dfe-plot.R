# ==========================================================================
# Plot: DFE RSFT
# ==========================================================================


# Load Packages-------------------------------------------------------------
pacman::p_load(data.table)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)

# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

db <- read.table("../stimuli/example_bayes_data.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
db <- as.data.table(db)


# Plots ---------------------------------------------------------------------

db[, sampling := ifelse(numberOfdraws == min(numberOfdraws), "small sample", "large sample")]
db[, trial := as.factor(trial)]


ggplot(db[id %in% c("17_3_1", "17_3_2", "17_3_3", "17_10_1", "17_10_2", "17_10_3")], 
       aes(x=prhv_rsft, fill = sampling, colour = sampling))+
  theme_classic() +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values=c("blue","green")) +
  scale_colour_manual(values=c("blue","green")) +
  facet_wrap(~trial) +
  xlim(0,1)
ggsave("../figures/temp_dfe_easy.png",width = 8, height = 6)

ggplot(db[id %in% c("19_3_1", "19_3_2", "19_3_3", "19_10_1", "19_10_2", "19_10_3")], 
       aes(x=prhv_rsft, fill = sampling, colour = sampling))+
  theme_classic() +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values=c("blue","green")) +
  scale_colour_manual(values=c("blue","green")) +
  facet_wrap(~trial) +
  xlim(0,1)
ggsave("../figures/temp_dfe_hard.png",width = 8, height = 6)

