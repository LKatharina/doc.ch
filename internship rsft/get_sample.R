
get_sample <- function(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n = 100, prior = c(1,1), seed = 42) {
  set.seed(seed)
  
  ps2 <- 1-ps1
  pr2 <- 1-pr1
  dfe_l <- length(dfe_n)
  budget_l <- length(budget)
  beta_l <- length(beta_n)
  
  count_s1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = ps1))
  count_r1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = pr1))
  
  freq_rown <- subject_n * dfe_l * budget_l * beta_l
  
  data_freq <- data.table(count_s1 = rep(count_s1, times = budget_l * beta_l),
                          count_r1 = rep(count_r1, times = budget_l * beta_l),
                          dfe_id = rep(dfe_n, each = subject_n, times = budget_l * beta_l),
                          budget = rep(budget, each = subject_n * dfe_l, times = beta_l),
                          # subject_nr = rep(1:subject_n, times = dfe_l * budget_l),
                          subject_id = 1:freq_rown,
                          beta_n = rep(beta_n, each = subject_n * dfe_l * budget_l),
                          s1 = rep(s1, times = freq_rown),
                          s2 = rep(s2, times = freq_rown),
                          r1 = rep(r1, times = freq_rown),
                          r2 = rep(r2, times = freq_rown),
                          ps1 = rep(ps1, times = freq_rown),
                          ps2 = rep(ps2, times = freq_rown),
                          pr1 = rep(pr1, times = freq_rown),
                          pr2 = rep(pr2, times = freq_rown),
                          seed = rep(seed, times = freq_rown))
  
  data_freq[, `:=` (count_s2 = dfe_id - count_s1, 
                    count_r2 = dfe_id - count_r1,
                    env_id = paste(ps1, s1, s2, pr1, r1, r2, budget, sep = "_"))]
  
  data_beta <- data_freq[rep(1:.N, each = beta_n[1])]
  # data_beta <- data_freq[rep(1:.N, beta_n)]
  
  
  data_beta[, `:=` (b_ps1 = rbeta(.N, count_s1 + prior[1], count_s2 + prior[2]),
                    b_pr1 = rbeta(.N, count_r1 + prior[1], count_r2 + prior[2])),
            by = .(dfe_id, subject_id, budget)]
  
  data_beta[, `:=` (b_ps2 = 1 - b_ps1,
                    b_pr2 = 1 - b_pr1,
                    start = 0,
                    beta_id = 1:nrow(data_beta))]
 
  return(data_beta)
}

