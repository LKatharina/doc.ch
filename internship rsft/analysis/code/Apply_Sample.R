
Apply_Sample <- function(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed) {
  return(Get_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior = c(1,1), seed))
}

Apply_Sample_Beta <- function(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed) {
  return(Get_Sample_Beta(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior = c(1,1), seed))
}
