# ==============================================================================
# Function to simulate random foresting and make beta draws for all outcomes
# ==============================================================================

Get_Sample <- function(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n = 100, prior = c(1,1), seed = 42, design_id) {
  set.seed(seed)
  
  ps2 <- 1-ps1
  pr2 <- 1-pr1
  
  
  count_s1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = ps1))
  count_r1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = pr1))
  
  data_freq <- data.table(count_s1 = count_s1,
                          count_r1 = count_r1,
                          dfe_id = dfe_n,
                          budget = budget,
                          subject_id = 1:subject_n,
                          subject_n = subject_n,
                          beta_n = beta_n,
                          s1 = s1,
                          s2 = s2,
                          r1 = r1,
                          r2 = r2, 
                          ps1 = ps1,
                          ps2 = ps2,
                          pr1 = pr1,
                          pr2 = pr2,
                          seed = seed,
                          design_id = design_id)
  
  data_freq[, `:=` (count_s2 = dfe_id - count_s1, 
                    count_r2 = dfe_id - count_r1,
                    env_id = paste0("S", ps1, "_", s1, "_", s2, "_", "R", pr1, "_", r1, "_", r2, "_", "B", budget))]
  
  data_beta <- data_freq[rep(1:.N, each = beta_n[1])]
  
  data_beta[, `:=` (b_ps1 = round(rbeta(.N, count_s1 + prior[1], count_s2 + prior[2]), 3),
                    b_pr1 = round(rbeta(.N, count_r1 + prior[1], count_r2 + prior[2]), 3)),
            by = .(subject_id)]
  
  data_beta[, `:=` (b_ps2 = 1 - b_ps1,
                    b_pr2 = 1 - b_pr1,
                    start = 0,
                    beta_id = 1:nrow(data_beta))]
  
  return(data_beta)
}

