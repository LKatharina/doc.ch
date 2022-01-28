setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("rsft1988.R")
source("softmax.R")
source("rsft1988-probstates.R")

phv <- c(0.5,0.5,0,0) #c(pxHV, pyHV, 0, 0)
plv <- c(0,0,1,0) #c(0, 0, pxLV, pyLV)
outcomes <- c(0,2,1,0) #c(xHV,yHV,xLV,yLV)
goal = 7
Noutcomes = length(outcomes)
timeHorizon = 5
start = 0
choiceprob = 0.5

# rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
m <- rsftModel(0,2,0,1.2,0.5,0.5,0.1,0.9,3,2,0)
choiceprob = as.data.table(cr_softmax(x = m@extended[,.(policyHV,policyLV)],0.2))
choiceprob = cbind(m@extended[,.(trial,state)],choiceprob)


xh = 5
yh = 0
pxh = 0.5
pyh = 0.5
xl = 4
yl = 4
pxl = 1
pyl = 0
budget = 10
data = data.table(xh = xh, yh = yh, pxh = pxh, pyh = pyh, xl = xl, yl = yl, pxl = pxl, pyl = pyl, budget = budget, start = 0)


rsft_model <- hm1988(
  ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
  trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
  data = data,         # our data (as before)
  budget = ~budget,      # name of our budget column in our data
  initstate = ~start,    # name of our starting-state column in our data
  ntrials = 2,            # we always 5 trials therefore I hard-code this
  states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
  choicerule = "softmax",
  fix = list(tau = 0.2))

predict(rsft_model)
predict(rsft_model, type = "values")

# rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
m <- rsftModel(data$xh, data$yh, data$xl, data$yl, data$pxh, data$pyh, data$pxl, data$pyl, data$budget, 2,0)
choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV,policyLV)],0.2))

m <- rsftModel(data$xh, data$yh, data$xl, data$yl, data$pxh, data$pyh, data$pxl, data$pyl, data$budget, 2,0,evmax)

evmax = function(budget,state){
  state
}
rsft_model <- hm1988(
  ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
  trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
  data = data,         # our data (as before)
  budget = ~budget,      # name of our budget column in our data
  initstate = ~start,    # name of our starting-state column in our data
  ntrials = 2,            # we always 5 trials therefore I hard-code this
  states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
  choicerule = "softmax",
  fix = list(tau = 0.2),
  fitnessfun = evmax)


# rsftStates(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start,choiceprob,final) # final = probability to end in a certain state
prstates = rsftStates(0,2,0,1.2,0.5,0.5,0.1,0.9,3,2,0,0.5,F)
prstates = rsftStates(0,2,0,1,0.5,0.5,0,1,3,2,0,choiceprob,F)
finalprstates = rsftStates(0,2,0,1,0.5,0.5,0,1,3,2,0,choiceprob,T)
prstates[,sum(prstate)]
finalprstates[,sum(prstate)]
