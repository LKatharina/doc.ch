# ==============================================================================
# Function to apply Get_Sample and Get_Model to a stimuli/parameter data set 
# ==============================================================================

Get_Predictions <- function(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed, ntrials, design_id) {
  d1 <- Get_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior = c(1,1), seed, design_id)
  return(Get_Model(df = d1, ntrials = ntrials))
}