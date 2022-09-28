###########################################################
# Houston and McNamara 1988: RISK SENSITIVE FORAGING MODEL
#
#
############################################################

# Notizen
# Wahrscheinlichkeiten der 2 Optionen
# k <- c(0.1,0.8,0.1) #c(pxHV, pyHV, 0, 0)
# l <- c(0.4,0.2,0.4) #c(0, 0, pxLV, pyLV)
# x <- c(0,1,2,0) #c(xHV,yHV,xLV,yLV)

# Packages =========================================================================
library(data.table)


# Model ============================================================================
setClass(Class="RSFT",
         representation(
           compact = "data.table",
           extended = "data.table"
         )
)


# Reward function ===========================================================
# Standard optimal model uses a step reward function.
# model with subjective rewards uses a logistic reward function.

# Step reward function ------------------------------------------------------
step = function(goal,state) { ifelse(state >= goal, 1, 0) }

# EV maximizing reward function ---------------------------------------------
evmax = function(goal,state){
  state
}

# smooth reward function ----------------------------------------------------
logistic = function(goal,state,k){
  y = 1/(1+exp(-k*(state-goal)))
  return(y)
}


KT = function(goal,state,k){
  y = ifelse(state >= goal, state^k, -2*(state^k)) 
  return(y)
}

# Optimal Model =================================================================
rsftModel <- function(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start, Rfunction = "step", k = NULL, gstates = NULL, gtrials = NULL){
  originaloutcomes = c(xh,yh,xl,yl)
  
  #define variables -------------------------------------------------------------
  notzero = c(pxh,pyh,pxl,pyl)
  if(any(notzero == 0)){
    if(pxh == 0){
      xh = 0
    }
    if(pyh == 0){
      yh = 0
    }
    if(pxl == 0){
      xl = 0
    }
    if(pyl == 0){
      yl = 0
    }
  }  else { }

  if(any(c(pxh,pyh) == 0) & any(c(pxl,pyl) == 0)){
    if(pxh == 0){
      phv = c(pyh,0)
      if(pxl == 0){
        plv = c(0,pyl)
        outcomes = c(yh,yl)}
      if(pyl == 0){
        plv = c(0,pxl)
        outcomes = c(yh,xl)
      }
    }
    if(pyh == 0){
      phv = c(pxh,0)
      if(pxl == 0){
        plv = c(0,pyl)
        outcomes = c(xh,yl)
      }
      if(pyl == 0){
        plv = c(0,pxl)
        outcomes = c(xh,xl)
      }
    }
  } else {
    phv = c(pxh,pyh,0,0) #c(pxHV, pyHV, 0, 0)
    plv = c(0,0,pxl,pyl) #c(0, 0, pxLV, pyLV)
    outcomes = c(xh,yh,xl,yl) #c(xHV,yHV,xLV,yLV)
  }

  goal = goal
  Noutcomes = length(outcomes)
  timeHorizon = timeHorizon
  start = start
  
  # rsft1988 Functions =========================================================


  # function for decision tree ---------------------------------------------------
  growTree = function(outcomes, p, timeHorizon, start){
    results = vector("list", timeHorizon) #list with length = timeHorizon --> store states and probabilities for each trial
    
    # Fill list
    results[[1]] = rbind(outcomes + start, p) # states and probs after the 1st choice
    
    for(t in 2:timeHorizon) {
      states = NULL
      for(i in results[[t-1]][1,]){
        states = c(states, (i + outcomes))
      }
      probStates = rep(p, length(outcomes)^(t-1)) 
      results[[t]] = rbind(states, probStates)
    }
    return(results)
  }


  # function policies in T-1 ------------------------------------------------------------
  makePoliciesLast <- function(outcomes, ERlasttrial){
    y = 1
    policy = NULL
    index = 1
    z = Noutcomes

    for(i in 1:(length(ERlasttrial)/Noutcomes)){
      policy[index] <- sum(ERlasttrial[y:z])
      y = z + 1
      z = z + Noutcomes
      index = index + 1
    }
    return(policy)
  }


  # function policy for one option in a given trial ----------------------------------------
  makePolicyElse = function(states, optimalPolicy, t){
    policy = c(NULL)
    temp = c(NULL)
    y = 1
    z = Noutcomes
    for(i in 1:(length(states[[t]][2,]))){
      temp <- states[[t]][2,]*optimalPolicy[2,]
    }
    for(a in 1:(length(states[[t]][2,])/Noutcomes)){
      policy[a] = sum(temp[y:z])
      y = z + 1
      z = z + Noutcomes
    }
    return(policy)
  }


  # function to determine optimal choice in a given trial ------------------------
  determineOptimal = function(policyHV,policyLV){
    optimalPolicy = vector("list", 1)
    option = c(NULL)
    optimal = c(NULL)

    for(i in 1:length(policyHV)) {
      if (policyHV[i] > policyLV[i]) {
        option[i] = 1
        optimal[i] = policyHV[i]
      } else if (policyHV[i] == policyLV[i]) {
        option[i] = 0
        optimal[i] = policyHV[i]
      } else {
        option[i] = 2
        optimal[i] = policyLV[i]
      }
    }
    optimalPolicy = rbind(option,optimal)
    return(optimalPolicy)
  }


  # function backward optimal choice selection ---------------------------------
  makePoliciesElse = function(optimalPolicy) {
    tab <- vector("list", timeHorizon - 1)
    possiblePolicy <- vector("list", 1)
    for (t in (timeHorizon - 1):1) {
      policyHV = makePolicyElse(statesHV, optimalPolicy = optimalPolicy, t = t)
      policyLV = makePolicyElse(statesLV, optimalPolicy = optimalPolicy, t = t)
      optimalPolicy = determineOptimal(policyHV = policyHV, policyLV = policyLV)
      tab[[t]] <- rbind(optimalPolicy, policyHV, policyLV)
    }
    return(tab)
  }
  
  # function delete impossible states --------------------------------------------
  deleteImpossible = function(dt){
    for(i in 2:timeHorizon){
      repeatestates = rep(dt[trial == i]$state, each = length(outcomes))
      
      if(sum(notzero == 0) == 2){
        compare = rep(dt[trial == i]$state, each = length(outcomes)) - outcomes
      } else {
        compare = rep(dt[trial == i]$state, each = length(outcomes)) - originaloutcomes
      }
      
      dcompare = data.table(trial = i, state = repeatestates, comparevalue = compare)
      dcompare[,contain := ifelse(compare %in% alloptimal[trial == i-1,state],1,0)]
      check = dcompare[,.(checkvalues = sum(contain)),by="state"]
      vcheck = check[checkvalues != 0,state]
      dt = dt[trial != i | trial == i & (state %in% vcheck)]
    }
    return(dt)
  }
  

  # Run Functions from rsft1988 functions ----------------------------------------

  # HV Option: Tree with Probabilities, terminal reward, ER in the last trial
  # LV Option: Tree with Probabilities, terminal reward, ER in the last trial
  statesHV = growTree(outcomes, p = phv, timeHorizon = timeHorizon, start = start)
  statesLV = growTree(outcomes, p = plv, timeHorizon = timeHorizon,  start = start)
  

  # calculates reward based on final states (e.g. step function: 1 if final state >= goal, else 0)
  if(Rfunction == "evmax"){
    terminalRewardHV = evmax(goal = goal,state = statesHV[[timeHorizon]][1,])
    terminalRewardLV = evmax(goal = goal,state = statesLV[[timeHorizon]][1,])
  } else if(Rfunction == "logistic") {
    terminalRewardHV = logistic(goal = goal,state = statesHV[[timeHorizon]][1,],k=k)
    terminalRewardLV = logistic(goal = goal,state = statesLV[[timeHorizon]][1,],k=k)
  } else if(Rfunction == "TK"){
    terminalRewardHV = TK(goal = goal,state = statesHV[[timeHorizon]][1,],k=k)
    terminalRewardLV = TK(goal = goal,state = statesLV[[timeHorizon]][1,],k=k)
  } else {
    terminalRewardHV = step(goal = goal,state = statesHV[[timeHorizon]][1,])
    terminalRewardLV = step(goal = goal,state = statesLV[[timeHorizon]][1,])
  }
  
  ERlastHV = statesHV[[timeHorizon]][2,] * terminalRewardHV # probabilities * terminal reward
  ERlastLV = statesLV[[timeHorizon]][2,] * terminalRewardLV

  # rsft values
  policyHV = makePoliciesLast(outcomes = outcomes, ERlasttrial = ERlastHV) # sum ERlasttrial for HV
  policyLV = makePoliciesLast(outcomes = outcomes, ERlasttrial = ERlastLV) # sum ERlasttrial for LV
  optimalPolicyLast = determineOptimal(policyHV = policyHV, policyLV = policyLV) # optimal option
  optBeforeLast = makePoliciesElse(optimalPolicy = optimalPolicyLast) # Backward induction


  # Combined list with all trials and states --------------------------------------

  # make list with all policies
  lasttrial = rbind( optimalPolicyLast, policyHV, policyLV)
  alloptimal = vector("list", timeHorizon)

  for(t in 1:(timeHorizon-1)){
    alloptimal[[t]] = rbind(optBeforeLast[[t]])
  }

  alloptimal[[timeHorizon]] = rbind(lasttrial)


  # add states
  alloptimal[[1]] = as.data.table(t(rbind(trial = 1, state = start, alloptimal[[1]], ph = 1, pl = 1)))

  t = 2
   for(i in 1:(length(statesHV)-1)) {
    alloptimal[[t]] = as.data.table(t(rbind(trial = t, state = statesHV[[i]][1,],alloptimal[[t]],ph = statesHV[[i]][2,], pl = statesLV[[i]][2,])))
    t = t + 1
   }
  
  completealloptimal = rbindlist(alloptimal)
  completealloptimal = completealloptimal[order(trial,state),.(trial,state,policyHV,policyLV,ph,pl)]

 
  # delete impossible and remove dublicated
  alloptimal = completealloptimal[(ph != 0 | pl != 0),]
  alloptimal = alloptimal[order(trial,state),.(trial,state,policyHV,policyLV)]

  # remove duplicated
  alloptimal = unique(alloptimal)

  # delete impossible states
  if(any(notzero == 0) & !any(originaloutcomes == 0)){
    alloptimal_withoutZero = alloptimal[trial > 1 & state != 0,]
    alloptimal_t1 = alloptimal[trial == 1]
    alloptimal = rbind(alloptimal_t1,alloptimal_withoutZero)
  }
  

  #completealloptimal = completealloptimal[(ph != 0 | pl != 0),]
  alloptimal = alloptimal[order(trial,state),.(trial,state,policyHV,policyLV)]
  completealloptimal = completealloptimal[order(trial,state),.(trial,state,policyHV,policyLV)]

  alloptimal = deleteImpossible(alloptimal)
  #completealloptimal = deleteImpossible(completealloptimal)
  
  ERdata = function(compact=alloptimal, extended=completealloptimal){
    return(new("RSFT",
               compact = compact,
               extended = extended))
  }
  
  TMfinal = ERdata()
  
  # Filter all or given trials and states
  if(is.null(gtrials) & is.null(gstates)){
    return(TMfinal)
  } else {
    com = cbind(1:length(gtrials),1:length(gstates))
    selectedComps = NULL
    selectedCompsext = NULL
    for(i in 1:nrow(com)){
      d =  TMfinal@compact[(trial == gtrials[com[i,1]]) & (state == gstates[com[i,1]])]
      selectedComps = rbind(selectedComps,d)
      dext =  TMfinal@extended[(trial == gtrials[com[i,1]]) & (state == gstates[com[i,1]])]
      selectedCompsext = rbind(selectedCompsext,dext)
    }
    TMfinal@compact = selectedComps
    TMfinal@extended = selectedCompsext
    return(TMfinal)
  }
  
}


