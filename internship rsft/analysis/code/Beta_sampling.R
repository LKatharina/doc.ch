# ==============================================================================
# Erstellt einen Datensatz mit verschiedenen Beta-Grössenen, um die optimale 
# Beta-Grösse in einem Vergleich eruieren zu können
# ==============================================================================


# Clear environment and set working directory-----------------------------------
rm(list = ls())
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load Packages-----------------------------------------------------------------
library(ggplot2)
library(data.table)
library(tidybayes)
library(patchwork)
library(future)
library(magrittr)
library(doFuture)


# Source Scripts----------------------------------------------------------------
source("Get_Sample.R")
source("Get_Betas.R")


# Set Parameters----------------------------------------------------------------
subjects <- 50
betas <- 750
ntrials <- 3
seeds <- seq(1, 10, 1)
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
# beta_sample <- paras[, Get_Sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]

# Create dataset with mean, sd, median and quartiles comparisons between DFE-values and the related variations
beta_coll <- beta_sample_f[, .(mean_dfe1 = mean(b_ps1[dfe_id == dfes[1]]), 
                               sd_dfe1 = sd(b_ps1[dfe_id == dfes[1]]),
                               median_dfe1 = median(b_ps1[dfe_id == dfes[1]]),
                               Q1_dfe1 = quantile(b_ps1[dfe_id == dfes[1]], prob = .25),
                               Q3_dfe1 = quantile(b_ps1[dfe_id == dfes[1]], prob = .75),
                               var_dfe1 = var(b_ps1[dfe_id == dfes[1]]),
                               mean_dfe2 = mean(b_ps1[dfe_id == dfes[2]]),
                               sd_dfe2 = sd(b_ps1[dfe_id == dfes[2]]),
                               median_dfe2 = median(b_ps1[dfe_id == dfes[2]]),
                               Q1_dfe2 = quantile(b_ps1[dfe_id == dfes[2]], prob = .25),
                               Q3_dfe2 = quantile(b_ps1[dfe_id == dfes[2]], prob = .75),
                               var_dfe2 = var(b_ps1[dfe_id == dfes[2]])),
                           keyby = .(beta_n, seed)]
beta_coll <- beta_coll[, lapply(.SD, var), by = .(beta_n)]


ticks <- c(1,2,3,4,5,7,10,20,30,40,50,75,100,200,300,400,500,750,1000,2000,3000)
plotlabels <- c("Mean", "SD", "Median", "Q1", "Q3", "Variance")

for (i in 1:(length(beta_coll)-2)) {
  if (i < 7) {
    assign(paste0("plot_", plotlabels[i], "_1"),
           ggplot(mapping = aes_string(x = beta_coll[[1]], y = beta_coll[[i+2]])) +
             geom_line(size = .7) +
             theme_minimal() +
             scale_x_continuous(trans = "log2", breaks = ticks) +
             labs(x = "Beta size",
                  y = paste0(plotlabels[i], " b_ps1")) +
             theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
    if (i == 6) {
      main_plot1 <- ggpubr::ggarrange(plot_Mean_1, plot_SD_1, plot_Median_1, plot_Q1_1, plot_Q3_1, plot_Variance_1, 
                                     labels = c("Mean", "SD", "Median", "1st quantile", "3rd quantile", "Variance"),
                                     ncol = 3, nrow = 2, hjust = c(-2.2, -4.1, -1.6, -1, -1, -1.4), vjust = 2) %>% 
        ggpubr::annotate_figure(top = ggpubr::text_grob(label = "Differences in beta distributions between seeds (samplesize 3, 100 seeds)", 
                                                        face = "bold", size = 18))
      ggsave(filename = "analysis/figures/Betasize_variance_dfe1.png", plot = main_plot1, height = 20, width = 35, units = "cm")
    }
  }
  if (i >= 7) {
    assign(paste0("plot_", plotlabels[i-6], "_2"),
           ggplot(mapping = aes_string(x = beta_coll[[1]], y = beta_coll[[i+2]])) +
             geom_line(size = .7) +
             theme_minimal() +
             scale_x_continuous(trans = "log2", breaks = ticks) +
             labs(x = "Beta size",
                  y = paste0(plotlabels[i-6], " b_ps1")) +
             theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
    if (i == 12) {
      main_plot2 <- ggpubr::ggarrange(plot_Mean_2, plot_SD_2, plot_Median_2, plot_Q1_2, plot_Q3_2, plot_Variance_2, 
                                     labels = c("Mean", "SD", "Median", "1st quantile", "3rd quantile", "Variance"),
                                     ncol = 3, nrow = 2, hjust = c(-2.2, -4.1, -1.6, -1, -1, -1.4), vjust = 2) %>% 
        ggpubr::annotate_figure(top = ggpubr::text_grob(label = "Differences in beta distributions between seeds (samplesize 10, 100 seeds)", 
                                                        face = "bold", size = 18))
      ggsave(filename = "analysis/figures/Betasize_variance_dfe2.png", plot = main_plot2, height = 20, width = 35, units = "cm")
    }
  }
}

main_plot1
main_plot2




# Same code but for variations in subject size==================================


# Source Scripts----------------------------------------------------------------
source("analysis/code/Get_Sample_Sub.R")


# Set Parameters----------------------------------------------------------------
subjects <- c(seq(1, 9, 1), seq(10, 100, 10), seq(200, 500, 100), 1000)
betas <- 750
ntrials <- 3
seeds <- seq(1, 10, 1)
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
# beta_sample_f <- Get_Betas(paras)
subject_sample <- paras[, Get_Sample_Sub(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]

# Create dataset with mean, sd, median and quartiles comparisons between DFE-values and the related variations
subject_coll <- subject_sample[, .(mean_dfe1 = mean(b_ps1[dfe_id == dfes[1]]), 
                             sd_dfe1 = sd(b_ps1[dfe_id == dfes[1]]),
                             median_dfe1 = median(b_ps1[dfe_id == dfes[1]]),
                             Q1_dfe1 = quantile(b_ps1[dfe_id == dfes[1]], prob = .25),
                             Q3_dfe1 = quantile(b_ps1[dfe_id == dfes[1]], prob = .75),
                             var_dfe1 = var(b_ps1[dfe_id == dfes[1]]),
                             mean_dfe2 = mean(b_ps1[dfe_id == dfes[2]]),
                             sd_dfe2 = sd(b_ps1[dfe_id == dfes[2]]),
                             median_dfe2 = median(b_ps1[dfe_id == dfes[2]]),
                             Q1_dfe2 = quantile(b_ps1[dfe_id == dfes[2]], prob = .25),
                             Q3_dfe2 = quantile(b_ps1[dfe_id == dfes[2]], prob = .75),
                             var_dfe2 = var(b_ps1[dfe_id == dfes[2]])),
                         keyby = .(subject_n, seed)]
subject_coll <- subject_coll[, lapply(.SD, var), by = .(subject_n)]
# subject_coll[, (2:13) := round(.SD, 3), .SDcols = c(2:13)]


ticks <- c(1,2,3,4,5,7,10,20,30,40,50,75,100,200,300,400,500,750,1000,2000,3000)
plotlabels <- c("Mean", "SD", "Median", "Q1", "Q3", "Variance")

for (i in 1:(length(subject_coll)-2)) {
  if (i < 7) {
    assign(paste0("plot_", plotlabels[i], "_1"),
           ggplot(mapping = aes_string(x = subject_coll[[1]], y = subject_coll[[i+2]])) +
             geom_line(size = .7) +
             theme_minimal() +
             scale_x_continuous(trans = "log2", breaks = ticks) +
             labs(x = "Subject size",
                  y = paste0(plotlabels[i], " b_ps1")) +
             theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
    if (i == 6) {
      main_plot1 <- ggpubr::ggarrange(plot_Mean_1, plot_SD_1, plot_Median_1, plot_Q1_1, plot_Q3_1, plot_Variance_1, 
                                      labels = c("Mean", "SD", "Median", "1st quantile", "3rd quantile", "Variance"),
                                      ncol = 3, nrow = 2, hjust = c(-2.2, -4.1, -1.6, -1, -1, -1.4), vjust = 2) %>% 
        ggpubr::annotate_figure(top = ggpubr::text_grob(label = "Differences in beta distributions between seeds (samplesize 3, beta 750, 100 seeds)", 
                                                        face = "bold", size = 18))
      ggsave(filename = "analysis/figures/Subjectsize_variance_dfe1.png", plot = main_plot1, height = 20, width = 35, units = "cm")
    }
  }
  if (i >= 7) {
    assign(paste0("plot_", plotlabels[i-6], "_2"),
           ggplot(mapping = aes_string(x = subject_coll[[1]], y = subject_coll[[i+2]])) +
             geom_line(size = .7) +
             theme_minimal() +
             scale_x_continuous(trans = "log2", breaks = ticks) +
             labs(x = "Subject size",
                  y = paste0(plotlabels[i-6], " b_ps1")) +
             theme(plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
    if (i == 12) {
      main_plot2 <- ggpubr::ggarrange(plot_Mean_2, plot_SD_2, plot_Median_2, plot_Q1_2, plot_Q3_2, plot_Variance_2, 
                                      labels = c("Mean", "SD", "Median", "1st quantile", "3rd quantile", "Variance"),
                                      ncol = 3, nrow = 2, hjust = c(-2.2, -4.1, -1.6, -1, -1, -1.4), vjust = 2) %>% 
        ggpubr::annotate_figure(top = ggpubr::text_grob(label = "Differences in beta distributions between seeds (samplesize 10, beta 750, 100 seeds)", 
                                                        face = "bold", size = 18))
      ggsave(filename = "analysis/figures/Subjectsize_variance_dfe2.png", plot = main_plot2, height = 20, width = 35, units = "cm")
    }
  }
}

main_plot1
main_plot2



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



