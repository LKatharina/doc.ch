###########################################################
# Houston and McNamara 1988: RISK SENSITIVE FORAGING MODEL
# 
#
############################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("rsft1988.R") # rsft model
source("softmax.R") # choice rule
source("rsft1988-probstates.R") # prstate
source("rsft1988-rewardfunctions.R") # different reward functions

# Stimuli ===================================================================
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


# Run Model ==================================================================
# rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start,Rfunction,gstates,gtrials)
# Rfunction: if no reward function specified the default step function is used
# gstates, gtrials: if no trials and states are entered the function makes predictions for all possible trials and states

# Step Reward Function (default), all states and trials
m <- rsftModel(data$xh, data$yh, data$xl, data$yl, data$pxh, data$pyh, data$pxl, data$pyl, data$budget, 2,0)
m@compact[,.(policyHV,policyLV)] # Tree without duplications
m@extended[,.(policyHV,policyLV)] # Full Tree (is needed to calculate prstate)

# Step Reward Function (default), given subset of states and trials
m <- rsftModel(data$xh, data$yh, data$xl, data$yl, data$pxh, data$pyh, data$pxl, data$pyl, data$budget, 2,0,gtrials = c(2,2),gstates = c(4,5))

# Expected Value maximizing reward function (load rsft1988-rewardfunctions.R first)
m <- rsftModel(data$xh, data$yh, data$xl, data$yl, data$pxh, data$pyh, data$pxl, data$pyl, data$budget, 2,0,evmax)


# Choice Rule ================================================================
#choiceprob(datatable containing values (x and y), tau)
m <- rsftModel(data$xh, data$yh, data$xl, data$yl, data$pxh, data$pyh, data$pxl, data$pyl, data$budget, 2,0)
choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV,policyLV)],0.2)) 


# PrState ====================================================================
# First run rsft model and choice rule
# rsftStates(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start,choiceprob,final) # final = probability to end in a certain state
m <- rsftModel(data$xh, data$yh, data$xl, data$yl, data$pxh, data$pyh, data$pxl, data$pyl, data$budget, 2,0)
choiceprob = as.data.table(cr_softmax(x = m@extended[,.(policyHV,policyLV)],0.2))
choiceprob_st = cbind(m@extended[,.(trial,state)],choiceprob)
prstates = rsftStates(data$xh, data$yh, data$xl, data$yl, data$pxh, data$pyh, data$pxl, data$pyl, data$budget, 2,0, choiceprob_st, F) # Without final state
prstates = rsftStates(data$xh, data$yh, data$xl, data$yl, data$pxh, data$pyh, data$pxl, data$pyl, data$budget, 2,0, choiceprob_st, T) # only final state


# Run RSFT Model data table ==================================================
xh = 2
yh = 0
pxh = 0.5
pyh = 0.5
xl = 1
yl = 0
pxl = 1
pyl = 0
budget = 3
d2 = data.table(xh = xh, yh = yh, pxh = pxh, pyh = pyh, xl = xl, yl = yl, pxl = pxl, pyl = pyl, budget = budget, start = 0)
s = rbind(data,d2)

rsft = lapply(1:nrow(s), function(i){
  d = s[i,]
  # rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
  m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$pxh,d$pyh,d$pxl,d$pyl,d$budget,2,0)
  choiceprob = as.data.table(cr_softmax(x = m@extended[,.(policyHV,policyLV)],0.2))
  choiceprob = cbind(m@extended[,.(trial,state)],choiceprob)
  # rsftStates(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start,choiceprob,final) # final = probability to end in a certain state
  prstates = rsftStates(d$xh,d$yh,d$xl,d$yl,d$pxh,d$pyh,d$pxl,d$pyl,d$budget,2,0,choiceprob,F)
  choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV,policyLV)],0.2))
  return(cbind(
    m@compact,
    prhv = choiceprob$policyHV,
    prstate = prstates$prstate))
}
)

rsftdata = rbindlist(rsft)
db = cbind(db,rsftdata$prstate)