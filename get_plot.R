
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
         caption = paste0("Beta_n = ", max(d2$beta_id)/max(d2$subject_id), 
                          ", Subject_n = ", max(d2$subject_id)/(length(unique(d2$dfe_id))*length(unique(d2$dfe_id))))) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.key.size = unit(.2, 'cm'),
          legend.key.height= unit(.5, 'cm'),
          legend.key.width= unit(.5, 'cm'),
          legend.title = element_text(size = 5),
          legend.text = element_text(size = 5))
}

