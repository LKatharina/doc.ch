# ==============================================================================
# Function to plot the distribution of choices in a specific environment design
# by sample size
# ==============================================================================

Get_Plot <- function(df, wrapby = ~trial) {
  the_name <- "Sample Size"
  the_labels <- c("Large (10 per option)", "Small (3 per option)")
  the_colors <- c("red", "grey25")
  ggplot(data = df,
         mapping = aes(y = prhv_rsft, fill = dfe_id)) +
    stat_slab(alpha = .15) +
    stat_pointinterval(mapping = aes(color = dfe_id),
                       position = position_dodge(width = .1, preserve = "single")) +
    theme_classic() +
    scale_fill_manual(values = the_colors,
                      name = the_name, labels = the_labels) +
    scale_colour_manual(values = the_colors,
                        name = the_name, labels = the_labels) +
    scale_shape_manual(values = c(16, 21),
                       name = the_name, labels = the_labels) +
    scale_x_continuous(breaks = NULL) +
    facet_wrap(wrapby, labeller = label_both, scale = "free_x") +
    labs(title = paste0("Environment ", df$env_id[1]),
         subtitle = paste("Reach", df$budget[1], "in 3 trials"),
         caption = paste0("Beta_n = ", df$beta_n[1], 
                          ", Subject_n = ", df$subject_n[1]),
         x = NULL,
         y = "Predicted Proportion of Risky Choices")
}