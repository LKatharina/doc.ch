# ==============================================================================
# Function to compute the utility of each environment design
# ==============================================================================

Get_Comparison <- function(df) {
  
  dfe_comp <- df[, .(decision = mean(prhv_rsft),
                     budget = mean(budget),
                     beta_n = mean(beta_n),
                     subject_n = mean(subject_n)),
                 by = .(env_id, dfe_id)]
  
  dfe <- unique(dfe_comp$dfe_id)
  
  dfe_comp[, `:=` (absolute_diff = decision[dfe_id == dfe[1]] - decision[dfe_id == dfe[2]],
                   percentage_diff = abs(100 / decision[dfe_id == dfe[1]] * decision[dfe_id == dfe[2]] - 100),
                   significant_diff = ifelse(test = sign(decision[dfe_id == dfe[1]] - 0.5) != sign(decision[dfe_id == dfe[2]] - 0.5),
                                             yes = 1,
                                             no = 0)),
           by = env_id]
  
  dfe_comp[, utility := percentage_diff * significant_diff]
  
  dfe_comp <- unique(dfe_comp[, .(absolute_diff, percentage_diff, significant_diff, utility),
                                by = env_id])
  
  dfe_comp <- dfe_comp[order(-utility)]
  
  return(dfe_comp)
}