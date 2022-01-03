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

<<<<<<< HEAD
db <- read.table("../stimuli/example_bayes_data2.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
db <- as.data.table(db)
db
# Parameter ----------------------------------------------------------------
ntrials <- 3
nconditions <- 2
conditions <- c("small sample", "large sample")
=======
db <- read.table("../stimuli/example_bayes_data.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
db <- as.data.table(db)

>>>>>>> parent of acda3c3 (dfe sample size)

# Plots ---------------------------------------------------------------------

db[, sampling := ifelse(numberOfdraws == min(numberOfdraws), "small sample", "large sample")]
db[, trial := as.factor(trial)]

<<<<<<< HEAD
maxdensity <- function(budget){
  sd <- db[grepl(as.character(budget), db$id, fixed = TRUE),]
  sd <- as.data.frame(sd)
  vmax <- NULL
  s <- rep(conditions, each = ntrials)
  t <- rep(c(1:ntrials), nconditions)
  for(i in 1:6){
    j <- s[i]
    k <- t[i]
    sdb <- sd[sd$sampling == j & sd$trial == k ,]
    v = which.max(density(sdb$lvalue)$y)
    max <- density(sdb$lvalue)$x[v]
    vmax <- c(vmax, max)
    sdb <- NULL
  }

  densitypeak <- data.table(sampling = s,trial = t,peak = vmax)
  return(densitypeak)
}



peakeasy <- maxdensity(17)
peakhard <- maxdensity(19)

p1 <- ggplot(db[id %in% c("17_3_1", "17_3_2", "17_3_3", "17_10_1", "17_10_2", "17_10_3")],
       aes(x=lvalue, fill = sampling, colour = sampling))+
  theme_classic() +
  geom_density(alpha = 0.1) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0))+
  scale_y_continuous(limits = c(0,40), expand = c(0,0))+
  geom_vline(peakeasy, mapping = aes(xintercept = peak, colour= sampling), linetype = 1, size = 0.5, alpha = 1)+
  coord_flip() +
  scale_fill_manual(values=c("blue","green"), name = "Sample Size",labels = c("large sample (20)", "small sample (6)")) +
  scale_colour_manual(values=c("blue","green"), name = "Sample Size",labels = c("large sample (20)", "small sample (6)")) +
  facet_wrap(~trial, labeller = label_both) +
  xlab("Predicted Proportion of Risky Choices") +
  labs(title = "A", subtitle = "Reach 17 in 3 trials")
ggsave("../figures/temp_dfe_easy.png",width = 8, height = 6)


p2 <- ggplot(db[id %in% c("19_3_1", "19_3_2", "19_3_3", "19_10_1", "19_10_2", "19_10_3")],
       aes(x=hvalue, fill = sampling, colour = sampling))+
=======

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
>>>>>>> parent of acda3c3 (dfe sample size)
  theme_classic() +
  geom_density(alpha = 0.1) +
  scale_fill_manual(values=c("blue","green")) +
  scale_colour_manual(values=c("blue","green")) +
  facet_wrap(~trial) +
  xlim(0,1)
ggsave("../figures/temp_dfe_hard.png",width = 8, height = 6)

<<<<<<< HEAD
p1 + plot_spacer() + p2 + plot_layout(guides = "collect", widths = c(0.4,0.1,0.4))
?plot_spacer


(p1 + theme(plot.margin = unit(c(0,30,0,0), "pt"))) +
  (p2 + theme(plot.margin = unit(c(0,0,0,30), "pt"))) + plot_layout(guides = "collect")


# Alternative to plot
library(tidybayes) # for fancy density plots, google it, it is great
# define one plotting function
make_plot <- function(i) {
  the_name <- "Sample Size"
  the_labels <- c("Large (10 per option)", "Small (3 per option)")
  the_colors <- c("red", "grey25")
  ggplot(
    data = db[id %in% i],
    mapping = aes(x = trial, y = prhv_rsft, color = sampling, fill = sampling)) +
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
    subtitle = paste("Reach", db[id %in% i]$b[1], "in 3 trials")) +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
}

p + p1

# plot and combine
p1 <- make_plot(c("17_3_1", "17_3_2", "17_3_3", "17_10_1", "17_10_2","17_10_3"))
p2 <- make_plot(c("19_3_1", "19_3_2", "19_3_3", "19_10_1", "19_10_2","19_10_3"))

p1 + plot_spacer() + p2 +
  plot_layout(guides = "collect", widths = c(.4,.05,.4)) +
  plot_annotation(caption = "Note: Points = Median, thick line = 66% quantile intervall, thin line = 95% quantile intervall")

ggsave("../figures/temp_dfe_easy_hard_n50.png",width = 14, height = 4)
=======
>>>>>>> parent of acda3c3 (dfe sample size)
