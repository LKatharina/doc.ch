
Get_Paras <- function(dfe_n, subject_n, beta_n, seed, trial_n) {
  
  paras <- data.table(dfe_n = dfe_n,
                      subject_n = subject_n,
                      beta_n = beta_n,
                      seed = seed,
                      ntrials = trial_n,
                      para_id = .I)
  return(paras)
}
