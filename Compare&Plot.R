
# Compare & Plot

# Function to compare draw-sizes
get_comparison <- function(df) {
  
  mean_prhv <- d2[, .(prhv_rsft = mean(prhv_rsft)), 
                  by = .(dfe_id, trial)]
  
  dfe <- unique(mean_prhv$dfe_id)
  dfe_comp <- mean_prhv[, .(trial = c("1", "2", "3"),
                            absolute_dif = c(prhv_rsft[trial == 1 & dfe_id == dfe[2]] - prhv_rsft[trial == 1 & dfe_id == dfe[1]],
                                             prhv_rsft[trial == 2 & dfe_id == dfe[2]] - prhv_rsft[trial == 2 & dfe_id == dfe[1]],
                                             prhv_rsft[trial == 3 & dfe_id == dfe[2]] - prhv_rsft[trial == 3 & dfe_id == dfe[1]]),
                            percentage_dif = c((1 / prhv_rsft[trial == 1 & dfe_id == dfe[1]] 
                                                * prhv_rsft[trial == 1 & dfe_id == dfe[2]] - 1) * 100,
                                               (1 / prhv_rsft[trial == 2 & dfe_id == dfe[1]] 
                                                * prhv_rsft[trial == 2 & dfe_id == dfe[2]] - 1) * 100,
                                               (1 / prhv_rsft[trial == 3 & dfe_id == dfe[1]] 
                                                * prhv_rsft[trial == 3 & dfe_id == dfe[2]] - 1) * 100),
                            significant_dif = c(ifelse(test = sign(prhv_rsft[trial == 1 & dfe_id == dfe[1]] - 0.5) != 
                                                         sign(prhv_rsft[trial == 1 & dfe_id == dfe[2]] - 0.5),
                                                       yes = "YES",
                                                       no = "NO"),
                                                ifelse(test = sign(prhv_rsft[trial == 2 & dfe_id == dfe[1]] - 0.5) != 
                                                         sign(prhv_rsft[trial == 2 & dfe_id == dfe[2]] - 0.5),
                                                       yes = "YES",
                                                       no = "NO"),
                                                ifelse(test = sign(prhv_rsft[trial == 3 & dfe_id == dfe[1]] - 0.5) != 
                                                         sign(prhv_rsft[trial == 3 & dfe_id == dfe[2]] - 0.5),
                                                       yes = "YES",
                                                       no = "NO")))]
  return(dfe_comp)
}

# Function to plot predicted proportion of risky choices
get_plot <- function(i) {
  d2[, dfe_id := factor(dfe_id)]
  the_name <- "Sample Size"
  the_labels <- c("Large (10 per option)", "Small (3 per option)")
  the_colors <- c("red", "grey25")
  ggplot(
    data = d2[plot_id %in% i],
    mapping = aes(x = trial, y = prhv_rsft, color = dfe_id, fill = dfe_id)) +
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
    facet_wrap(~ trial, labeller = label_both, scale = "free_x") +
    ylab("Predicted Proportion of Risky Choices") +
    ylim(c(0,1)) +
    labs(title = "Environment",
         subtitle = paste("Reach", d2[plot_id %in% i]$budget[1], "in 3 trials"),
         caption = paste0("Beta_n = ", max(d2[plot_id %in% i]$beta_id)/max(d2[plot_id %in% i]$subject_id), 
                          ", Subject_n = ", max(d2[plot_id %in% i]$subject_id)/length(unique(d2[plot_id %in% i]$dfe_id)))) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

