# ==========================================================================
# Plot: DFE RSFT
# ==========================================================================

# Erstellt Density Plots

# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)
memory.limit(9999999999)
# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")

stimuli <- read.table("../stimuli/dfe_stimuli-all-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
d1 <- read.table("../stimuli/dfe_stimuli-20-t5-420.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
st = read.table("../stimuli/dfe-stimuli-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
stimuli = as.data.table(stimuli)
stimuli = stimuli[nr %in% st$nr]
stimuli[,stimulinr := nr]
stimuli[,unique(nr)]
db = readRDS("../stimuli/dfe_stimuli-20-t5-d20.rds")

db = merge(db,stimuli[dh < 0.4,.(prhv,policyHV,policyLV,prstate,trial,state,stimulinr)], by=c("trial","state","stimulinr"))


# Parameter ----------------------------------------------------------------
ntrials <- 5
nconditions <- 2
conditions <- c("small sample", "large sample")
tau = 0.2


# Plots ---------------------------------------------------------------------
db[, sampling := ifelse(numberOfdraws == min(numberOfdraws), "small sample", "large sample")]
db[, trial := as.factor(trial)]
db[,wprhv := prhv_belief *prstate]
db[,wprhv_opt := prhv *prstate]

agg = db[,.(policyHV_belief = median(policyHV_belief),policyLV_belief = median(policyLV_belief)), by=c("trial","sampling")]
agg[,better := ifelse(policyHV_belief > policyLV_belief, "HV","LV")]

maxdensity <- function(budget){
  sd <- db[grepl(as.character(budget), db$id, fixed = TRUE) & b == budget,]
  sd <- as.data.frame(sd)
  vmax <- NULL
  s <- rep(conditions, each = ntrials)
  t <- rep(c(1:ntrials), nconditions)
  states = unique(sd$state)
  
  for(i in 1:(length(conditions)*ntrials)){
    j <- s[i]
    k <- t[i]
    sdb <- sd[sd$sampling == j & sd$trial == k ,]
    v = which.max(density(sdb$wprhv)$y)
    max <- density(sdb$wprhv)$x[v]
    vmax <- c(vmax, max)
    sdb <- NULL
  }

  densitypeak <- data.table(sampling = s,trial = t,peak = vmax)
  return(densitypeak)
}



peakeasy <- maxdensity(unique(db$b)[1])
peakhard <- maxdensity(19)

densplot = function(b){
  p1 <- ggplot(db[b == b],
               aes(x=wprhv, fill = sampling, colour = sampling))+
    theme_classic() +
    geom_density(alpha = 0.1) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0))+
    scale_y_continuous(limits = c(0,50), expand = c(0,0))+
    geom_vline(maxdensity(b), mapping = aes(xintercept = peak, colour= sampling), linetype = 1, size = 0.5, alpha = 1)+
    coord_flip() +
    scale_fill_manual(values=c("blue","green"), name = "Sample Size",labels = c("large sample (50)", "small sample (6)")) +
    scale_colour_manual(values=c("blue","green"), name = "Sample Size",labels = c("large sample (50)", "small sample (6)")) +
    facet_wrap(~trial, labeller = label_both) +
    xlab("Predicted Proportion of Risky Choices") +
    labs(title = "A", subtitle = "Reach 26 in 5 trials")
  return(p1)
}

densplot(38)
stimuli[dh < 0.4,b]
p1 <- ggplot(db[b == 49],
             aes(x=wprhv, fill = sampling, colour = sampling))+
  theme_classic() +
  geom_density(alpha = 0.1) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0))+
  scale_y_continuous(limits = c(0,30), expand = c(0,0))+
  geom_vline(maxdensity(58), mapping = aes(xintercept = peak, colour= sampling), linetype = 1, size = 0.5, alpha = 1)+
  coord_flip() +
  scale_fill_manual(values=c("blue","green"), name = "Sample Size",labels = c("large sample (50)", "small sample (6)")) +
  scale_colour_manual(values=c("blue","green"), name = "Sample Size",labels = c("large sample (50)", "small sample (6)")) +
  facet_wrap(~trial, labeller = label_both) +
  xlab("Predicted Proportion of Risky Choices") +
  labs(title = "A", subtitle = "Reach 26 in 5 trials")

ggsave("../figures/temp_dfe_420.png",width = 8, height = 6)


p2 <- ggplot(db[grepl(as.character(19), db$id, fixed = TRUE) & b == 19],
       aes(x=wprhv, fill = sampling, colour = sampling))+
  theme_classic() +
  geom_density(alpha = 0.1) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0))+
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  geom_vline(peakhard, mapping = aes(xintercept = peak, colour= sampling), linetype = 1, size = 0.5, alpha = 1)+
  coord_flip() +
  scale_fill_manual(values=c("blue","green")) +
  scale_colour_manual(values=c("blue","green")) +
  facet_wrap(~trial)
ggsave("../figures/temp_dfe_hard.png",width = 8, height = 6)

p1 + plot_spacer() + p2 + plot_layout(guides = "collect", widths = c(0.4,0.1,0.4))
?plot_spacer


(p1 + theme(plot.margin = unit(c(0,30,0,0), "pt"))) +
  (p2 + theme(plot.margin = unit(c(0,0,0,30), "pt"))) + plot_layout(guides = "collect")


# Alternative to plot
library(tidybayes) # for fancy density plots, google it, it is great
# define one plotting function
make_plot <- function(data) {
  the_name <- "Sample Size"
  the_labels <- c("Large (10 per option)", "Small (3 per option)")
  the_colors <- c("red", "grey25")
  ggplot(
    data = data,
    mapping = aes(x = trial, y = prhv_belief, color = sampling, fill = sampling)) +
  stat_halfeye( # uses median and QI = quantile interval (also known as the percentile interval or equi-tailed interval)
    .width = c(.66, 0.95), #use .66, .95 to show 66 and 96% HDI
    slab_alpha = 0.15,
    position = position_dodge(width = .09),
    aes(shape = sampling), point_fill = "white") +
  theme_classic() +
  scale_fill_manual(
    values=the_colors,
    name = the_name,
    labels = the_labels) +
  scale_colour_manual(
    values=the_colors,
    name = the_name, labels = the_labels) +
  scale_shape_manual(
    values = c(16, 21),
    name = the_name, labels = the_labels) +
  facet_wrap(~trial+state, labeller = label_both, scale = "free_x") +
  ylab("Predicted Proportion of Risky Choices") +
  labs(title = "Environment",
    subtitle = paste("Reach", data$b[1], "in 3 trials")) +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
}


# plot and combine
p1 <- make_plot(db[b == 43])

ggsave("../figures/temp_43_.pdf",width = 8, height = 20)

p2 <- make_plot(db[b == 19])

make_plot1 <- function(data) {
  the_name <- "Sample Size"
  the_labels <- c("Large (10 per option)", "Small (3 per option)")
  the_colors <- c("red", "grey25")
  ggplot(
    data = data,
    mapping = aes(x = trial, y = policyLV_belief, color = sampling, fill = sampling)) +
    stat_halfeye( # uses median and QI = quantile interval (also known as the percentile interval or equi-tailed interval)
      .width = c(.66, 0.95), #use .66, .95 to show 66 and 96% HDI
      slab_alpha = 0.15,
      position = position_dodge(width = .09),
      aes(shape = sampling), point_fill = "white") +
    theme_classic() +
    scale_fill_manual(
      values=the_colors,
      name = the_name,
      labels = the_labels) +
    scale_colour_manual(
      values=the_colors,
      name = the_name, labels = the_labels) +
    scale_shape_manual(
      values = c(16, 21),
      name = the_name, labels = the_labels) +
    facet_wrap(~trial, labeller = label_both, scale = "free_x") +
    ylab("Predicted Proportion of Risky Choices") +
    labs(title = "Environment",
         subtitle = paste("Reach", data$b[1], "in 3 trials")) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

p1 <- make_plot1(db[b == 23])
p1 + plot_spacer() + p2 +
  plot_layout(guides = "collect", widths = c(.4,.05,.4)) +
  plot_annotation(caption = "Note: Points = Median, thick line = 66% quantile intervall, thin line = 95% quantile intervall")

ggsave("../figures/temp_dfe_easy_hard_n50.png",width = 14, height = 4)

db
