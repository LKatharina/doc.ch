# ==========================================================================
# Plot: DFE RSFT
# ==========================================================================

# Erstellt Density Plots

# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)

# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("2-dfe-select.R")
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")



db <- read.table("../stimuli/example_bayes_20_5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
db <- as.data.table(db)

# Parameter ----------------------------------------------------------------
ntrials <- 5
nconditions <- 2
conditions <- c("small sample", "large sample")
tau = 0.2


# prstate ------------------------------------------------------------------
rsft_prstate = lapply(1:nrow(stimuli), function(i){
  d = stimuli[i,]
  # rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
  m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$pxh,d$pyh,d$pxl,d$pyl,d$b,ntrials,d$start)
  choiceprob = as.data.table(cr_softmax(x = m@extended[,.(policyHV,policyLV)],tau))
  choiceprob = cbind(m@extended[,.(trial,state)],choiceprob)
  # rsftStates(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start,choiceprob,final) # final = probability to end in a certain state
  prstates = rsftStates(d$xh,d$yh,d$xl,d$yl,d$pxh,d$pyh,d$pxl,d$pyl,d$b,ntrials,d$start,choiceprob,F)
  return(cbind(
    prstate = prstates$prstate))
}
)

for(i in 1:length(rsft_prstate)) {
  rsft_prstate[[i]] = as.data.table(rsft_prstate[[i]])
}

prstate = rbindlist(rsft_prstate)

db = cbind(db, prstate)

# Plots ---------------------------------------------------------------------
db[, sampling := ifelse(numberOfdraws == min(numberOfdraws), "small sample", "large sample")]
db[, trial := as.factor(trial)]
db[,wprhv := prhv_belief *prstate]

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

p1 <- ggplot(db[grepl(as.character(17), db$id, fixed = TRUE) & b == 17],
       aes(x=wprhv, fill = sampling, colour = sampling))+
  theme_classic() +
  geom_density(alpha = 0.1) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0))+
  scale_y_continuous(limits = c(0,12), expand = c(0,0))+
  geom_vline(peakeasy, mapping = aes(xintercept = peak, colour= sampling), linetype = 1, size = 0.5, alpha = 1)+
  coord_flip() +
  scale_fill_manual(values=c("blue","green"), name = "Sample Size",labels = c("large sample (20)", "small sample (6)")) +
  scale_colour_manual(values=c("blue","green"), name = "Sample Size",labels = c("large sample (20)", "small sample (6)")) +
  facet_wrap(~trial, labeller = label_both) +
  xlab("Predicted Proportion of Risky Choices") +
  labs(title = "A", subtitle = "Reach 17 in 3 trials")
ggsave("../figures/temp_dfe_easy.png",width = 8, height = 6)


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
    mapping = aes(x = trial, y = wprhv, color = sampling, fill = sampling)) +
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
p1 <- make_plot(db[b == 17])
p2 <- make_plot(db[b == 19])

p1 + plot_spacer() + p2 +
  plot_layout(guides = "collect", widths = c(.4,.05,.4)) +
  plot_annotation(caption = "Note: Points = Median, thick line = 66% quantile intervall, thin line = 95% quantile intervall")

ggsave("../figures/temp_dfe_easy_hard_n50.png",width = 14, height = 4)

db
