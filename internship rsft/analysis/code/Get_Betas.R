
Get_Betas <- function(data) {
  
  rown <- nrow(paras)
  l1 <- c(1:(rown/4))
  l2 <- c((rown/4+1):(rown/2))
  l3 <- c((rown/2+1):(rown*.75))
  l4 <- c((rown*.75+1):rown)
  
  plan(multisession)
  
  b1 %<-% {
    data[l1, Get_Sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
  }
  b2 %<-% {
    data[l2, Get_Sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
  }
  b3 %<-% {
    data[l3, Get_Sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
  } 
  b4 %<-% {
    data[l4, Get_Sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
  } 
  return(rbind(b1, b2, b3, b4))
  
}