
get_predictions <- function(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed, ntrials) {
  # saves all args and values to get predictions
  # args <- as.list(match.call())
  # print(args)
  # calls the function get_sample and implicitly passes the arguments in args to this function
  # d1 <- do.call(what = get_sample, args = args[1:11])
  d1 <- get_sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior = c(1,1), seed = seed)
  return(get_model(df = d1, ntrials = ntrials))
}



all_comp <- NULL
get_predictions2 <- function(values) {
  d1 <- get_sample(s1 = values$S1, s2 = values$S2,
                   r1 = values$R1, r2 = values$R2,
                   ps1 = values$PS1, pr1 = values$PR1,
                   dfe_n = c(values$Dfe_1, values$Dfe_2),
                   subject_n = values$Subject_n,
                   budget = c(values$Budget_1, values$Budget_2),
                   beta_n = values$Beta_n,
                   seed = values$Seed)
  
  d2 <- get_model(df = d1, ntrials = values$Ntrials)
  
  
  
  rel_name <- paste0(values$Beta_n, "_", values$Dfe_1, "-", values$Dfe_2, "_", values$Budget_1, "-", 
                     values$Budget_2, "_", values$Subject_n, "_", values$S1, "_", values$S2, "_", values$R1, "_", 
                     values$R2, "_", values$PS1, "_", values$PR1)
  
  # assign(paste0("comparison_", rel_name), get_comparison(df = d2))
  new_comp <- get_comparison(df = d2)
  all_comp <- rbind(all_comp, new_comp)
  
  p1 <- get_plot(unique(d2[budget == unique(values$Budget_1)]$plot_id))
  p2 <- get_plot(unique(d2[budget == unique(values$Budget_2)]$plot_id))
  
  p1 + plot_spacer() + p2 +
    plot_layout(guides = "collect", widths = c(.4,.05,.4)) +
    plot_annotation(caption = "Note: Points = Median, thick line = 66% quantile intervall, thin line = 95% quantile intervall")
  
  ggsave(filename = paste0("plot_", rel_name, ".png"), width = 1920, height = 1080, units = "px")
}
