# ==============================================================================
# Erstellt einen Datensatz mit verschiedenen Beta-Grössenen, um die optimale 
# Beta-Grösse in einem Vergleich eruieren zu können
# ==============================================================================


# Clear environment-------------------------------------------------------------
rm(list = ls())
gc()


# Load Packages-----------------------------------------------------------------
library(ggplot2)
library(data.table)
library(tidybayes)
library(patchwork)
library(psych)
library(future)
library(magrittr)
library(dplyr)


# Source Scripts----------------------------------------------------------------
source("analysis/code/Get_Sample.R")
source("analysis/code/Get_Betas.R")


# Set Parameters----------------------------------------------------------------
subjects <- 5
betas <- c(seq(1, 9, 1), seq(10, 100, 10), seq(200, 1000, 100), 2000, 3000)
ntrials <- 3
seeds <- seq(1, 100, 1)
budget <- 18
dfes <- c(3, 10)
prior <- c(1, 1)
ps1 <- 0.5
s1 <- 6
s2 <- 8
pr1 <- 0.25
r1 <- 1
r2 <- 9



# Create Beta-dataset-----------------------------------------------------------

# Create dataset with all possible combinations of parameters
paras <- as.data.table(expand.grid(beta_n = betas, budget = budget, subject_n = subjects, dfe_n = dfes, seed = seeds))
paras[, `:=` (ps1 = ps1, s1 = s1, s2 = s2, pr1 = pr1, r1 = r1, r2 = r2, para_id = .I)]

# Calculate beta-samples for all combinations of parameters
beta_sample_f <- Get_Betas(paras)
# beta_sample <- paras[, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]

# Create dataset with mean, sd, median and quartiles comparisons between DFE-values and the related variations
beta_coll <- beta_sample_f[, .(mean_diff_dfe = abs(mean(b_ps1[dfe_id == dfes[1]]) - mean(b_ps1[dfe_id == dfes[2]])), 
                               sd_diff_dfe = abs(sd(b_ps1[dfe_id == dfes[1]]) - sd(b_ps1[dfe_id == dfes[2]])),
                               median_diff_dfe = abs(median(b_ps1[dfe_id == dfes[1]]) - median(b_ps1[dfe_id == dfes[2]])),
                               `1stq_diff_dfe` = abs(quantile(b_ps1[dfe_id == dfes[1]], prob = .25) - quantile(b_ps1[dfe_id == dfes[2]], prob = .25)),
                               `3rdq_diff_dfe` = abs(quantile(b_ps1[dfe_id == dfes[1]], prob = .75) - quantile(b_ps1[dfe_id == dfes[2]], prob = .75)),
                               var_diff_dfe = abs(var(b_ps1[dfe_id == dfes[1]]) - var(b_ps1[dfe_id == dfes[2]])),
                               var_dfe1 = abs(var(b_ps1[dfe_id == dfes[1]])),
                               var_dfe2 = abs(var(b_ps1[dfe_id == dfes[2]]))),
                           keyby = .(beta_n, seed)]
beta_coll <- beta_coll[, .(mean = mean(mean_diff_dfe),
                           sd = mean(sd_diff_dfe),
                           median = mean(median_diff_dfe),
                           Q1 = mean(`1stq_diff_dfe`),
                           Q3 = mean(`3rdq_diff_dfe`),
                           variance = mean(var_diff_dfe),
                           var_dfe1 = mean(var_dfe1),
                           var_dfe2 = mean(var_dfe2)), 
                       by = .(beta_n)]
beta_coll <- beta_coll %>%
  tibble() %>% 
  mutate(across(.cols = c(2:9), 
                ~round(.x, 3)))

beta_coll[, (2:9) := round(.SD, 3), .SDcols = c(2:9)]
 

ticks <- c(1,2,3,4,5,7,10,20,30,40,50,75,100,200,300,400,500,750,1000,2000,3000)
plotlabels <- c("Mean", "SD", "Median", "Q1", "Q3", "Variance")

for (i in 1:6) {
  assign(paste0("plot_", plotlabels[i]),
         ggplot(mapping = aes_string(x = beta_coll$beta_n, y = beta_coll[[i+1]])) +
    geom_line(size = .7) +
    theme_minimal() +
    scale_x_continuous(trans = "log2", breaks = ticks) +
    labs(x = "Beta size",
         y = paste0(plotlabels[i], " difference b_ps1")) +
    theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
}

main_plot <- ggpubr::ggarrange(plot_Mean, plot_SD, plot_Median, plot_Q1, plot_Q3, plot_Variance, 
                               labels = c("Mean", "SD", "Median", "1st quantile", "3rd quantile", "Variance"),
                               ncol = 3, nrow = 2, hjust = c(-2.2, -4.1, -1.6, -1, -1, -1.4), vjust = 3) %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob(label = "Differences in samplesizes (3, 10) depending on betasizes (100 seeds)", 
                                                  face = "bold", size = 18))
main_plot

ggsave(filename = "analysis/figures/Betasize_differences.png", plot = main_plot, height = 20, width = 35, units = "cm")


# cols <- colnames(beta_coll[c(2:7)])
# lapply(names(beta_coll[c(2:7)]),
#        function(a, b = plotlabels, c = ylabels) 
#          assign(paste0("plot_", b),
#                 ggplot(mapping = aes(x = beta_coll$beta_n, y = a)) +
#                   geom_line(size = .7) +
#                   theme_minimal() +
#                   scale_x_continuous(trans = "log2",
#                                      breaks = ticks) +
#                   labs(x = "Beta size",
#                        y = paste0(c, " difference b_ps1")) +
#                   theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
#                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))))
# 
# models <- colnames(beta_coll[,c(2:7)])
# plotlist <- list()
# for (i in 1:length(models))
# {
#   plotlist[[i]] <- data.frame(modname = models[i], 
#                               xvar = c(1:10), 
#                               yvar = rnorm(10), 
#                               avar = c(1:10),
#                               bvar = rnorm(10, mean = 1),
#                               stringsAsFactors = FALSE)   
#   
# }
# 
# alldata <- do.call(rbind, plotlist)
# 
# ggplot(alldata, aes(xvar, yvar)) + 
#   geom_line() + 
#   geom_line(aes(avar,bvar)) +
#   facet_grid(~modname)
# 
# 
# 
# plot_m <- ggplot(data = beta_coll, mapping = aes(x = beta_n, y = mean)) +
#   geom_line(size = .7) +
#   theme_minimal() +
#   scale_x_continuous(trans = "log2",
#                      breaks = ticks) +
#   labs(x = "Beta size",
#        y = "Mean difference b_ps1") +
#   theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#   
# plot_sd <- ggplot(data = beta_coll, mapping = aes(x = beta_n, y = sd)) +
#   geom_line(size = .7) +
#   theme_minimal() +
#   scale_x_continuous(trans = "log2",
#                      breaks = ticks) +
#   labs(x = "Beta size",
#        y = "SD difference b_ps1") +
#   theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_med <- ggplot(data = beta_coll, mapping = aes(x = beta_n, y = median)) +
#   geom_line(size = .7) +
#   theme_minimal() +
#   scale_x_continuous(trans = "log2",
#                      breaks = ticks) +
#   labs(x = "Beta size",
#        y = "Median difference b_ps1") +
#   theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_q1 <- ggplot(data = beta_coll, mapping = aes(x = beta_n, y = Q1)) +
#   geom_line(size = .7) +
#   theme_minimal() +
#   scale_x_continuous(trans = "log2",
#                      breaks = ticks) +
#   labs(x = "Beta size",
#        y = "Q1 difference b_ps1") +
#   theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_q3 <- ggplot(data = beta_coll, mapping = aes(x = beta_n, y = Q3)) +
#   geom_line(size = .7) +
#   theme_minimal() +
#   scale_x_continuous(trans = "log2",
#                      breaks = ticks) +
#   labs(x = "Beta size",
#        y = "Q3 difference b_ps1") +
#   theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_var <- ggplot(data = beta_coll, mapping = aes(x = beta_n, y = variance)) +
#   geom_line(size = .7) +
#   theme_minimal() +
#   scale_x_continuous(trans = "log2",
#                      breaks = ticks) +
#   labs(x = "Beta size",
#        y = "Variance difference b_ps1") +
#   theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# Combine current data with overall data
if (!exists("beta_summary")) {
  beta_summary <- beta_coll[, id := paste(budget, ps1, s1, s2, pr1, r1, r2, sep = "_")]
} else {
  beta_summary <- rbind(beta_summary, beta_coll[, id := paste(budget, ps1, s1, s2, pr1, r1, r2, sep = "_")])
}




data_plot <- beta_sample_f[beta_n %in% list(10, 50, 250, 1000, 3000)]
the_name <- "Sample Size"
the_labels <- c("Large (10 per option)", "Small (3 per option)")
the_colors <- c("red", "gray25")
ggplot(
  data = data_plot,
  mapping = aes(x = seed, y = b_ps1, color = dfe_id, fill = dfe_id)) +
  stat_halfeye( # uses median and QI = quantile interval (also known as the percentile interval or equi-tailed interval)
    .width = c(.66, 0.95), #use .66, .95 to show 66 and 96% HDI
    slab_alpha = 0.15,
    position = position_dodge(width = .09),
    aes(shape = dfe_id), point_fill = "white") +
  theme_classic() +
  scale_fill_manual(
    values = the_colors,
    name = the_name, labels = the_labels) +
  scale_colour_manual(
    values = the_colors,
    name = the_name, labels = the_labels) +
  scale_shape_manual(
    values = c(16, 21),
    name = the_name, labels = the_labels) +
  facet_wrap(~ beta_n + seed, labeller = label_both, scale = "free_x") +
  ylab("Predicted Proportion of Risky Choices") +
  ylim(c(0,1)) +
  labs(title = "Environment",
       subtitle = paste("Reach", data_plot$budget[1], "in 3 trials"),
       caption = paste0("Beta_n = ", max(data_plot$beta_id)/max(data_plot$subject_id), 
                        ", Subject_n = ", max(data_plot$subject_id)/(length(unique(data_plot$dfe_id))*length(unique(data_plot$dfe_id))))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.key.size = unit(.2, 'cm'),
        legend.key.height= unit(.5, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5))



