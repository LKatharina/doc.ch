
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


 
Get_Betas_2 <- function(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed) {
  # cores <- 4
  # data[, core := c(rep(1, times = nrow(data)-floor(nrow(data)/cores)*cores), 
  #                  rep(c(1:cores), each = floor(nrow(data)/cores)))]
  # 
  # cl <- parallel::makeCluster(2)
  # doParallel::registerDoParallel(cl)
  
  # y <- foreach(x = 1:cores, .combine = 'rbind') %dopar% {
  #   Get_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior = c(1,1), seed = seed)
  #   # Get_Sample(s1 = paras$s1[core == x], s2 = paras$s2[core == x], r1 = paras$r1[core == x], r2 = paras$r2[core == x], ps1 = paras$ps1[core == x], pr1 = paras$pr1[core == x], dfe_n = paras$dfe_n[core == x], subject_n = paras$subject_n[core == x], budget = paras$budget[core == x], beta_n = paras$beta_n[core == x], prior = paras$prior[core == x], seed = paras$seed[core == x])
  # }
  
  # registerDoFuture()
  # plan(multisession)
  # 
  y <- foreach(x = 1:cores) %dopar% {
    Get_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior = c(1,1), seed)
  }
  return(y)
}
  
  