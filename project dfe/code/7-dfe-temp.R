# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)


# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")
stimuli <- fread("../stimuli/dfe_stimuli-all-10-t3.csv")
stimuli = stimuli[40879,]
xh = stimuli$xh
yh = stimuli$yh
pxh = stimuli$pxh
pyh = stimuli$pyh
xl = stimuli$xl
yl = stimuli$yl
pxl = stimuli$pxl
pyl = stimuli$pyl
trial = 3
state = 18
start = 0
ss = 2
b = stimuli$b
ntrials = 3
computeExpectedReward = function(ss,xh,yh,pxh,xl,yl,pxl,b,trial,state,nbetadraw = 800,tau = 0.2,prior = c(1,1)){
  
  # number of possible occurrence of the outcome xh, given the number of draws ss
  ob = 0:ss
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2))) # alle Kombinationen von möglichen Obs der Optionen miteinander
  
  # Beta: Beliefs about the probabilities
  belx = vector("list", (ss+1))
  
  # Sampling from beta distribution
  for(i in 1:(ss+1)){
    belx[[i]] = rbeta(nbetadraw,ob[i] + prior[1], ss-ob[i] + prior[2])
  }
  bely = lapply(belx, function(x){ 1 - x })
  
  # data table
  data = data.table(xh = xh, yh = yh, pxh = pxh, pyh = 1 - pxh,
                    xl = xl, yl = yl, pxl = pxl, pyl = 1 - pxl,
                    bxh = unlist(belx[com[,1]]), byh = unlist(bely[com[,1]]),
                    bxl = unlist(belx[com[,2]]), byl = unlist(bely[com[,2]]),
                    start = start, b = b, id_betadraw = rep(1:nbetadraw,length(belx)), count_xh = rep(com[,1]-1,each = nbetadraw),
                    count_xl = rep(com[,2]-1,each = nbetadraw), id_binom = rep(1:nrow(com),each = nbetadraw))
  
  data[,c("bxh","byh","bxl","byl") := lapply(.SD,round,digits = 3), .SDcols = c("bxh","byh","bxl","byl")]
 
  
  rsft_dfd <- hm1988(
    ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
    trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
    data = data[1,],            # our data (as before)
    budget = ~b,       # name of our budget column in our data
    initstate = 0,     # name of our starting-state column in our data
    ntrials = ntrials,            # we always 5 trials therefore I hard-code this
    states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
    choicerule = "softmax",
    fix = list(tau = tau))
  
  rsft_dfd_sim <- data.table(
       trial = (ntrials+1) - rsft_dfd$get_timehorizons(), # get trials that are *remaining*
       state =   rsft_dfd$get_states(),         # get possible states
       prhv_rsft =    predict(rsft_dfd)  # get pr(hv) prediction
  )
  
  sim = data[rep(1:nrow(data), each=nrow(rsft_dfd_sim))]
  rsft_dfd_sim = rsft_dfd_sim[rep(1:nrow(rsft_dfd_sim), nbetadraw*nrow(com))]
  sim = cbind(sim,rsft_dfd_sim)
  
  
  rsft_dfe = lapply(1:nrow(data), function(i){
    d = data[i,]
    m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$bxh,d$byh,d$bxl,d$byl,d$b,3,0)
    choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV,policyLV)],0.2))
    print(i)
    return(data.table(
      policyHV_belief = m@compact$policyHV,
      policyLV_belief = m@compact$policyLV,
      state = m@compact$state,
      trial = m@compact$trial,
      prhv_belief = choiceprob$policyHV,
      nr = i
    ))
  }
  )
  
  rsft_dfe_sim = rbindlist(rsft_dfe)

  table(rsft_dfe_sim$nr)
  rbindlist(rsft_dfe[c(1,2)])
  rsft_dfe_sim[nr %in% c(rsft_dfe_sim[,.N, by = "nr"][N != 10,]$nr)]
  
  rsft_dfe <- hm1988(
    ~ xh + bxh + yh + byh | xl + bxl  + yl + byl,  # our formula (as before)
    trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
    data = data,            # our data (as before)
    budget = ~b,       # name of our budget column in our data
    initstate = 0,     # name of our starting-state column in our data
    ntrials = ntrials,            # we always 5 trials therefore I hard-code this
    states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
    choicerule = "softmax",
    fix = list(tau = tau))
  
  data$prhv_dfe = predict(rsft_dfe)
  
  # Wahrscheinlichkeiten hineinmultiplizieren (Verbundwahrscheinlichkeit) = erwartete Wahrscheinlichkeit berechnen
  # Aufsummieren der Eintrage über Vektoren
  binomh = dbinom(0:ss,ss,pxh)
  binoml = dbinom(0:ss,ss,pxl)
  
  #
  data[,pr_joint := binomh[(count_xh+1)] * binoml[(count_xl+1)]]
  data[,wprhv_dfe := prhv_dfe * pr_joint]
  
  return(list(ER,opt))
}

computeAbserdiff = function(ss,xh,yh,pxh,xl,yl,pxl,b,trial,state){
  binomh = dbinom(0:ss,ss,pxh)
  binoml = dbinom(0:ss,ss,pxl)
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2))) # N! / (N-k)! * k! = Kombination ohne Wiederholung
  ER_opt = computeExpectedReward(ss,xh,yh,pxh,xl,yl,pxl,b,trial,state)
  ER = ER_opt[[1]]
  opt = ER_opt[[2]]
  subdiff = abs(ER[,1] - ER[,2]) %*% (binomh[com[,1]] * binoml[com[,2]])
  optdiff = abs(opt[1,1] - opt[1,2])
  return(list(subdiff = subdiff[1,1], optdiff = unname(optdiff)))
}

computePrOptimal = function(ss,xh,yh,pxh,xl,yl,pxl,b,trial,state){
  binomh = dbinom(0:ss,ss,pxh)
  binoml = dbinom(0:ss,ss,pxl)
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2)))
  ER_opt = computeExpectedReward(ss,xh,yh,pxh,xl,yl,pxl,b,trial,state)
  ER = cr_argmax(ER_opt[[1]])
  opt = cr_argmax(ER_opt[[2]])
  
  propt = as.numeric((opt[,1] - opt[,2]) == (ER[,1] - ER[,2])) %*% (binomh[com[,1]] * binoml[com[,2]])
  return(propt[,1])
}


