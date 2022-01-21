
Get_Model <- function(df, ntrials, choicerule = "softmax", tau = 0.2) {
  # rsft_model <- hm1988(~ r1 + b_pr1  + r2 + b_pr2 | s1 + b_ps1 + s2 + b_ps2,
  #                      trials = ".ALL",
  #                      data = df,
  #                      budget = ~budget,
  #                      initstate = ~start,
  #                      nt = ntrials,
  #                      states = ".ALL",
  #                      choicerule = choicerule,
  #                      fix = list(tau = tau))
  
  # predict(rsft_model)
  
  # rsft_sim <- data.table(trial = (ntrials + 1) - rsft_model$get_timehorizons(),
  #                        state = rsft_model$get_states(),
  #                        prhv_rsft = rsft_model$predict("response"),
  #                        prstate = rsft_model$predict("pstate"),
  #                        rvalue = predict(rsft_model, type="values")[,1],
  #                        svalue = predict(rsft_model, type="values")[,2])
  
  df_b1 <- df[beta_n == unique(beta_n)[1]]
  df_b2 <- df[beta_n == unique(beta_n)[2]]

  rsft_model_b1 <- hm1988(~ r1 + b_pr1  + r2 + b_pr2 | s1 + b_ps1 + s2 + b_ps2,
                          trials = ".ALL",
                          data = df_b1,
                          budget = ~budget,
                          initstate = ~start,
                          nt = ntrials,
                          states = ".ALL",
                          choicerule = choicerule,
                          fix = list(tau = tau))

  predict(rsft_model_b1)

  rsft_sim_b1 <- data.table(trial = (ntrials + 1) - rsft_model_b1$get_timehorizons(),
                            state = rsft_model_b1$get_states(),
                            prhv_rsft = rsft_model_b1$predict("response"),
                            prstate = rsft_model_b1$predict("pstate"),
                            rvalue = predict(rsft_model_b1, type="values")[,1],
                            svalue = predict(rsft_model_b1, type="values")[,2])

  rsft_model_b2 <- hm1988(~ r1 + b_pr1  + r2 + b_pr2 | s1 + b_ps1 + s2 + b_ps2,
                          trials = ".ALL",
                          data = df_b2,
                          budget = ~budget,
                          initstate = ~start,
                          nt = ntrials,
                          states = ".ALL",
                          choicerule = choicerule,
                          fix = list(tau = tau))
  
  predict(rsft_model_b2)

  rsft_sim_b2 <- data.table(trial = (ntrials + 1) - rsft_model_b2$get_timehorizons(),
                            state = rsft_model_b2$get_states(),
                            prhv_rsft = rsft_model_b2$predict("response"),
                            prstate = rsft_model_b2$predict("pstate"),
                            rvalue = predict(rsft_model_b2, type="values")[,1],
                            svalue = predict(rsft_model_b2, type="values")[,2])
  
  # rsft_sim[, beta_id := cumsum(trial == 1)]
  rsft_sim_b1[, beta_id := cumsum(trial == 1)]
  rsft_sim_b2[, beta_id := cumsum(trial == 1)]
  
  # db <- merge(rsft_sim, df)
  db_b1 <- merge(rsft_sim_b1, df_b1)
  db_b2 <- merge(rsft_sim_b2, df_b2)
  
  db <- db_b1[db_b2]
  
  setcolorder(db, neworder = c("env_id", "dfe_id", "subject_id", "beta_id", 
                               "budget", "trial", "state", "prhv_rsft", 
                               "prstate", "rvalue", "svalue", "count_s1", 
                               "count_s2", "count_r1", "count_r2"))
  
  db[, plot_id := paste(beta_n, budget, dfe_id, trial, sep = "_")]
  
  return(db)
}
