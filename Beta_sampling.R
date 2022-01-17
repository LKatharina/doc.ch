
library(ggplot2)
library(data.table)
library(tidybayes)
library(patchwork)
library(psych)

rm(list = ls())
gc()

source("get_sample.R")

subjects <- 5
betas <- c(seq(1, 10, 1), seq(10, 100, 10), seq(250, 1000, 250), 2000, 3000)
ntrials <- 3
seeds <- seq(1, 10, 1)
budget <- 8
dfes <- c(3, 10)
prior <- c(1, 1)
ps1 <- 0.5
s1 <- 1
s2 <- 2
pr1 <- 0.8
r1 <- 0
r2 <- 4

paras <- as.data.table(expand.grid(beta_n = betas, budget = budget, subject_n = subjects, dfe_n = dfes, seed = seeds))
paras[, `:=` (ps1 = ps1, s1 = s1, s2 = s2, pr1 = pr1, r1 = r1, r2 = r2, para_id = .I)]

beta_sample <- paras[, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]

overview <- beta_sample[, .(mean_diff_dfe = abs(mean(b_ps1[dfe_id == dfes[1]]) - mean(b_ps1[dfe_id == dfes[2]])), 
                            sd_diff_dfe = abs(sd(b_ps1[dfe_id == dfes[1]]) - sd(b_ps1[dfe_id == dfes[2]])),
                            median_diff_dfe = abs(median(b_ps1[dfe_id == dfes[1]]) - median(b_ps1[dfe_id == dfes[2]])),
                            fstq_diff_dfe = abs(quantile(b_ps1[dfe_id == dfes[1]], prob = .25) - quantile(b_ps1[dfe_id == dfes[2]], prob = .25)),
                            thrq_diff_dfe = abs(quantile(b_ps1[dfe_id == dfes[1]], prob = .75) - quantile(b_ps1[dfe_id == dfes[2]], prob = .75)),
                            var_dfe1 = abs(var(b_ps1[dfe_id == dfes[1]])),
                            var_dfe2 = abs(var(b_ps1[dfe_id == dfes[2]]))),
                        keyby = .(beta_n, seed)]
overview <- overview[, .(mean_diff_dfe = mean(mean_diff_dfe),
                         sd_diff_dfe = mean(sd_diff_dfe),
                         median_diff_dfe = mean(median_diff_dfe),
                         fstq_diff_dfe = mean(fstq_diff_dfe),
                         thrq_diff_dfe = mean(thrq_diff_dfe),
                         var_dfe1 = var(var_dfe1),
                         var_dfe2 = var(var_dfe2)), 
                     by = .(beta_n)]
overview[, (2:6) := round(.SD, 3), .SDcols = c(2:6)]

# beta_sample <- NULL
# for (i in 1:length(seed)) {
#   if (i == 1) {
#     beta_sample <- paras[, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed == seed[i])]
#   } else {
#     beta_sample <- rbind(beta_sample, paras[, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed == seed[i])])
#   }
# }

# beta_sample <- c(sapply(X = paras, FUN = get_sample, s1 = s1, s2 = s2, r1 = r1, r2 = r2, ps1 = ps1, pr1 = pr2, dfe_n = dfe_n, subject_n = subject_n, budget = budget, beta_n = beta_n, prior = prior, seed = seed))

if (!exists("data_summary")) {
  data_summary <- overview[, id := paste(budget, ps1, s1, s2, pr1, r1, r2, sep = "_")]
} else {
  data_summary <- rbind(data_summary, overview[, id := paste(budget, ps1, s1, s2, pr1, r1, r2, sep = "_")])
}




data_plot <- beta_sample[beta_n %in% list(10, 50, 250, 1000, 3000)]
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



