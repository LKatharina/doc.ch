
Get_Myplot <- function(df) {
  ggplot(data = df, 
         mapping = aes(fill = dfe_id, 
                       x = prhv_rsft)) +
    stat_slab(alpha = .15) +
    stat_pointinterval(mapping = aes(color = dfe_id),
                       position = position_dodge(width = .1, preserve = "single")) +
    scale_fill_manual(values = c("red", "grey25")) +
    scale_colour_manual(values = c("red", "grey25")) +
    labs(title = "Environment",
         subtitle = paste("Reach", df$budget[1], "in 3 trials"),
         x = "Predicted Proportion of Risky Choices",
         y = NULL,
         caption = paste0("Beta_n = ", df$beta_n[1], 
                          ", Subject_n = ", df$subject_n[1])) +
    scale_y_continuous(breaks = NULL) +
    theme_minimal() +
    coord_flip()
}
