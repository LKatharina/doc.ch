###########################################################
# Houston and McNamara 1988: RISK SENSITIVE FORAGING MODEL
#
#
############################################################

# Notizen
# Wahrscheinlichkeiten der 2 Optionen
# k <- c(0.1,0.8,0.1) #c(pxHV, pyHV, 0, 0)
# l <- c(0.4,0.2,0.4) #c(0, 0, pxLV, pyLV)
# x <- c(0,1,2) #c(xHV,yHV,xLV,yLV)

# Packages =========================================================================
library(data.table)

# Reward Functions =================================================================

# Model ============================================================================
setClass(Class="RSFT",
         representation(
           compact = "data.table",
           extended = "data.table"
         )
)


# Reward function --------------------------------------------------------------
R = function(goal,state) { ifelse(state >= goal, 1, 0) }

rsftModel <- function(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start, Rfunction = R, gstates = NULL, gtrials = NULL){

  #define variables -------------------------------------------------------------
  phv = c(pxh,pyh,0,0) #c(pxHV, pyHV, 0, 0)
  plv = c(0,0,pxl,pyl) #c(0, 0, pxLV, pyLV)
  outcomes = c(xh,yh,xl,yl) #c(xHV,yHV,xLV,yLV)
  goal = goal
  Noutcomes = length(outcomes)
  timeHorizon = timeHorizon
  start = start


  # rsft1988 Functions =========================================================


  # function for decision tree ---------------------------------------------------
  growTree = function(outcomes, p, timeHorizon, start){
    results = vector("list", timeHorizon)
    results[[1]] = rbind(outcomes + start, p)

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

  # Run Functions from rsft1988 functions ----------------------------------------

  # HV Option: Tree with Probabilities, terminal reward, ER in the last trial
  statesHV = growTree(outcomes, p = phv, timeHorizon = timeHorizon, start = start)
  terminalRewardHV = Rfunction(goal = goal,state = statesHV[[timeHorizon]][1,])
  ERlastHV = statesHV[[timeHorizon]][2,] * terminalRewardHV

  # LV Option: Tree with Probabilities, terminal reward, ER in the last trial
  statesLV = growTree(outcomes, p = plv, timeHorizon = timeHorizon,  start = start)
  terminalRewardLV  <- Rfunction(goal = goal,state = statesLV[[timeHorizon]][1,])
  ERlastLV = statesLV[[timeHorizon]][2,] * terminalRewardLV

  # rsft values
  policyHV = makePoliciesLast(outcomes = outcomes, ERlasttrial = ERlastHV)
  policyLV = makePoliciesLast(outcomes = outcomes, ERlasttrial = ERlastLV)
  optimalPolicyLast = determineOptimal(policyHV = policyHV, policyLV = policyLV)
  optBeforeLast = makePoliciesElse(optimalPolicy = optimalPolicyLast)


  # Combined list with all trials and states --------------------------------------

  # make list with all policies
  lasttrial = rbind( optimalPolicyLast, policyHV, policyLV)
  alloptimal = vector("list", timeHorizon)

  for(t in 1:(timeHorizon-1)){
    alloptimal[[t]] = rbind(optBeforeLast[[t]])
  }

  alloptimal[[timeHorizon]] = rbind(lasttrial)


  # add states
  alloptimal[[1]] = as.data.table(t(rbind(trial = 1, state = start, alloptimal[[1]])))

  t = 2
   for(i in 1:(length(statesHV)-1)) {
    alloptimal[[t]] = as.data.table(t(rbind(trial = t, state = statesHV[[i]][1,],alloptimal[[t]])))
    t = t + 1
   }
  
  completealloptimal = rbindlist(alloptimal)
  completealloptimal = completealloptimal[order(trial,state),.(trial,state,policyHV,policyLV)]
  
  # remove duplicated
  t = 2
  for(t in 2:timeHorizon){
    alloptimal[[t]] = cbind(alloptimal[[t]][!duplicated(alloptimal[[t]][,2]),])
  }

  alloptimal = rbindlist(alloptimal)
  alloptimal = alloptimal[order(trial,state),.(trial,state,policyHV,policyLV)]
  
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


