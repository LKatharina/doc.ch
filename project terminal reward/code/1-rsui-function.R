#============================================================================
# Terminal Reward Functions
# 
#============================================================================

# Load Packages -------------------------------------------------------------
pacman::p_load(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R") 

# Transform into data table -------------------------------------------------
library(ggplot2)


#----------------------------------------------------------------------------
data = data.table(data)

tau = 0.02
kvalues = c(0.0001,0.2,0.4,0.5,0.7,1,2,5,10,20)

# Functions -----------------------------------------------------------------


ChangeReward = function(data,tau,k,Rfunction){
  stimuli = data[trial == 1]
  stimuli = stimuli[duplicated(nr) == F]
  
  
  rsft_reward = lapply(1:nrow(stimuli), function(i){
    d = stimuli[i,]
    # rsftModel(xh, yh, xl, yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start, Rfunction, k)
    m <- rsftModel(d$xh, d$yh, d$xl, d$yl,
                   d$pxh, d$pyh, d$pxl, d$pyl,
                   d$budget,
                   timeHorizon = 5,
                   d$start,
                   Rfunction = Rfunction,
                   k)
    
    choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV, policyLV)], tau))
    return(cbind(
      m@compact,
      prhv = choiceprob$policyHV,
      parameter = k))
  }
  )
  
  rsft_reward = rbindlist(rsft_reward)
  rsft_reward[, nr := cumsum(trial == 1)]
  return(rsft_reward)
}

RiskseekingUnderImpossibility = function(data){
# Optimal Model with Step Function ------------------------------------------
stimuli = data[trial == 1]
stimuli = stimuli[duplicated(nr) == F]

rsft = lapply(1:nrow(stimuli), function(i){
  
  d = stimuli[i,]
  
  m = rsftModel(d$xh, d$yh, d$xl, d$yl,
                 d$pxh, d$pyh, d$pxl, d$pyl,
                 d$budget,
                 timeHorizon = 5,
                 d$start)
  
  choiceprob = as.data.table(cr_softmax(x = m@extended[, .(policyHV, policyLV)], tau))
  choiceprob = cbind(m@extended[, .(trial, state)], choiceprob)
  
  # rsftStates(xh, yh, xl, yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start,choiceprob, final)
  prstates = rsftStates(d$xh, d$yh, d$xl, d$yl,
                        d$pxh, d$pyh, d$pxl, d$pyl,
                        d$budget,
                        timeHorizon = 5,
                        d$start,
                        choiceprob,
                        F)
  
  choiceprob = as.data.table(cr_softmax(x = m@compact[, .(policyHV, policyLV)], tau))
  
  return(cbind(
    m@compact,
    prhv = choiceprob$policyHV,
    prstate = prstates$prstate,
    parameter = -99))
}
)

rsft = rbindlist(rsft)
rsft[, nr := cumsum(trial == 1)] 
optdata = merge(data,rsft, by = c("trial","state","nr"))
optdata$stepLV = optdata$policyLV
optdata$stepHV = optdata$policyHV

# Is needed to exclude states, where ER != 0
data = merge(data,rsft[,.(policyHV, policyLV, trial, state, nr, prstate)], by = c("trial", "state", "nr"))
setnames(data, "policyHV", "stepHV")
setnames(data, "policyLV", "stepLV")
data = data[order(pid, nr, trial, state),]


# Smooth Model -----------------------------------------------------------
smooth = lapply(1:length(kvalues), function(i){
  sm = ChangeReward(data, tau ,kvalues[i],"logistic")
  sm = merge(data, sm, by = c("trial", "state", "nr"))
  return(cbind(sm))
}
)

smooth = rbindlist(smooth)
data = rbind(optdata,smooth)

#  Observed data and predictions -----------------------------------------

# Effect of difficulty
data[,.(mean(choice),mean(prhv)), by= c("parameter","difficulty")]


# Risk Seeking under impossibility
impossible = data[stepHV == 0 & stepLV == 0]
impossible[, mean(choice)]
impossible[, .(choice = mean(choice), reward = mean(prhv)), by=c("parameter")]
return(impossible)
}


# TK Value Function  ----------------------------------------------------
# alpha = 0.88
# TK = lapply(1:length(alpha), function(i){
#   sm = ChangeReward(data, 0.1,alpha,"logistic")
#   sm = merge(data,sm, by = c("trial","state","nr"))
#   return(cbind(sm))
# }
# )
# 
# TK = rbindlist(TK)
# 
# # Effect of difficulty
# TK[,.(mean(choice),mean(prhv)), by= c("k","difficulty")]
# 
# # Risk Seeking under impossibility
# TK = TK[stepHV == 0 & stepLV == 0]
# TK[,mean(choice)]
# TK[,.(choice = mean(choice), reward = mean(prhv)),by=c("k")]
# TK[,.N,by=c("k")]



# smooth reward function --------------------------------------------------
# rsft_model <- hm1988(
#   ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
#   trials = ~trial,        # NEW: ".ALL" will predict for *all possible* trials
#   data = data,            # our data (as before)
#   budget = ~budget,       # name of our budget column in our data
#   initstate = ~start,     # name of our starting-state column in our data
#   ntrials = 5,            # we always 5 trials therefore I hard-code this
#   states = ~state,        # NEW: ".ALL" will predict for *all possible* states
#   choicerule = "softmax",
#   fix = list(tau = 0.1))
# 
# data$rsft_risky = predict(rsft_model, type="values")[,1]
# data$rsft_safe = predict(rsft_model, type="values")[,2]
# 
# d = data[rsft_risky == 0 & rsft_safe == 0]
# 
# R = function(budget,state){
#   y = 1/(1+exp(-0.5*(state-budget)))
#   return(y)
# }
# 
# rsft_model1 <- hm1988(
#   ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
#   trials = ~trial,        # NEW: ".ALL" will predict for *all possible* trials
#   data = d,         # our data (as before)
#   budget = ~budget,      # name of our budget column in our data
#   initstate = ~start,    # name of our starting-state column in our data
#   ntrials = 5,            # we always 5 trials therefore I hard-code this
#   states = ~state,        # NEW: ".ALL" will predict for *all possible* states
#   choicerule = "softmax",
#   fix = list(tau = 0.1),
#   fitnessfun = R)
# 
# d$prhv = predict(rsft_model1)
# predict(rsft_model1, type = "values")
# 
# d[,.(mean(choice), mean(prhv))]
# 
# 
# rsft_model1 <- hm1988(
#   ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
#   trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
#   data = d,         # our data (as before)
#   budget = ~budget,      # name of our budget column in our data
#   initstate = ~start,    # name of our starting-state column in our data
#   ntrials = 2,            # we always 5 trials therefore I hard-code this
#   states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
#   choicerule = "softmax",
#   fix = list(tau = 0.1),
#   fitnessfun = R)


