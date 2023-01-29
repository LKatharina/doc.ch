###########################################################
# Houston and McNamara 1988: RISK SENSITIVE FORAGING MODEL
#
#
############################################################

# Probstates --> Does code also work for T-1 and option with more than 2 outcomes? 

# Comments: If n outcomes of the risky option != n outcomes of the safe option --> add 0 with p = 0.
# extended = only use for prstate calculation
# riskyX = c(xh,yh,......,zh) # Outcomes of the safe option
# riskyP <- c(pxh,pyh....,pzh) # Probabilities of the risky option
# safeX = c(xl,yl,......,zl) # Outcomes of the safe option
# safeP <- c(pxl,pyl....,pzl) # Probabilities of the risky option

# Packages =====================================================================
library(data.table)

# Reward Functions =============================================================

# Model ========================================================================
setClass(Class="RSFT",
         representation(
           compact = "data.table",
           extended = "data.table"
         )
)


# Reward function --------------------------------------------------------------

# Step reward function ---------------------------------------------------------
step = function(goal,state) { ifelse(state >= goal, 1, 0) }

# EV maximizing reward function ------------------------------------------------
evmax = function(goal,state){
  state
}

# smooth reward function -------------------------------------------------------
logistic = function(goal,state,k){
  y = 1/(1+exp(-k*(state-goal)))
  return(y)
}


KT = function(goal,state,k){
  y = ifelse(state >= goal, state^k, -2*(state^k)) 
  return(y)
}

rsftModel <- function(riskyX, safeX, riskyP, safeP, goal, timeHorizon, start, Rfunction = "step", k = NULL, gstates = NULL, gtrials = NULL){
  
  # Preprocess choice options ==================================================
  originaloutcomes = c(riskyX,safeX)
  originalriskyP = riskyP
  originalsafeP = safeP
  
  # Delete outcomes with p=0 when both options contain such zero outcomes.
  if(any(riskyP == 0) & any(safeP == 0)){
    nzRiskyP = which(riskyP != 0)
    nzSafeP = which(safeP != 0)
    riskyP = riskyP[nzRiskyP]
    riskyX = riskyX[nzRiskyP]
    safeP = safeP[nzSafeP]
    safeX = safeX[nzSafeP]
  } else { }
  
  # Make length of outcome vectors and probability vectors equal
  diffX = length(riskyX) - length(safeX)
  if(diffX > 0){
      safeX = c(safeX, rep(0,diffX))
      safeP = c(safeP, rep(0,diffX))
  } else if(diffX < 0){
    riskyX = c(riskyX, rep(0,abs(diffX)))
    riskyP = c(riskyP, rep(0,abs(diffX)))
  } else {
      
  }
  
  # changes 0 outcome with p = 0 --> necessary to find impossible states
  if(any(riskyP == 0)){
    riskyX[which(riskyP == 0)] = max(riskyX)
  } else { }
  
  if(any(safeP == 0)){
    safeX[which(safeP == 0)] = max(safeX)
  } else { }
  
  transformedX = c(riskyX,safeX) # contains no zero outcomes

  outcomes = c(riskyX, safeX)
  notzero = c(riskyP, safeP)
  # set value of outcomes with p=0 to 0 --> Necessary to build tree
  if(any(notzero == 0)){
    for(i in 1:length(notzero)){
      if(notzero[i] == 0){
        outcomes[i] = 0
      } else { }
    }
  }

  phv = c(riskyP, rep(0, length(safeP)))
  plv = c(rep(0, length(riskyP)),safeP)

  goal = goal
  Noutcomes = length(outcomes)
  timeHorizon = timeHorizon
  start = start
  
  
  # rsft1988 Functions =========================================================


  # function for decision tree -------------------------------------------------
  growTree = function(outcomes, p, timeHorizon, start){
    results = vector("list", timeHorizon)
    results[[1]] = rbind(outcomes + start, p)
    
    if(timeHorizon > 1){
      for(t in 2:timeHorizon) {
        states = NULL
        for(i in results[[t-1]][1,]){
          states = c(states, (i + outcomes))
        }
        probStates = rep(p, length(outcomes)^(t-1))
        results[[t]] = rbind(states, probStates)
      }
    }
    
    return(results)
  }


  # function policies in T-1 ---------------------------------------------------
  makePoliciesLast <- function(ERlasttrial){
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


  # function policy for one option in a given trial ----------------------------
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


  # function to determine optimal choice in a given trial ----------------------
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
  
  
  # function delete impossible states ------------------------------------------
  deleteImpossible = function(dt){
    if(timeHorizon > 1){
      for(i in 2:timeHorizon){
        repeatestates = rep(dt[trial == i]$state, each = length(outcomes))
        compare = rep(dt[trial == i]$state, each = length(outcomes)) - transformedX
        dcompare = data.table(trial = i, state = repeatestates, comparevalue = compare)
        dcompare[,contain := ifelse(compare %in% alloptimal[trial == i-1,state],1,0)]
        check = dcompare[,.(checkvalues = sum(contain)),by="state"]
        vcheck = check[checkvalues != 0,state]
        dt = dt[trial != i | trial == i & (state %in% vcheck)]
      }
    }
    return(dt)
  }
  

  # Run Functions from rsft1988 functions ======================================

  # HV Option: Tree with Probabilities, terminal reward, ER in the last trial
  # LV Option: Tree with Probabilities, terminal reward, ER in the last trial
  statesHV = growTree(outcomes, p = phv, timeHorizon = timeHorizon, start = start)
  statesLV = growTree(outcomes, p = plv, timeHorizon = timeHorizon,  start = start)
  
  # Terminal Reward Function = determines the reward for ending in a final state
  # 𝑅(𝑠𝑇−1 + 𝑥𝑖, 𝑔)
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
  
  # Expected Reward in T-1
  # 𝐸𝑅(𝑜, 𝑠, 𝑇 − 1) = ∑ 𝑅(𝑠𝑇−1 + 𝑥𝑖, 𝑔) × 𝑝𝑖
  # 1. Probability to end in a final state * terminal Reward: 𝑅(𝑠𝑇−1 + 𝑥𝑖, 𝑔) × 𝑝𝑖
  ERlastHV = statesHV[[timeHorizon]][2,] * terminalRewardHV #ERR
  ERlastLV = statesLV[[timeHorizon]][2,] * terminalRewardLV #ERS
  # 2. sum up for each option
  policyHV = makePoliciesLast(ERlasttrial = ERlastHV) #ERR
  policyLV = makePoliciesLast(ERlasttrial = ERlastLV) #ERS
  
  # Determine optimal policy: 𝑎∗(𝑠, 𝑇 − 1) = 𝑀𝑎𝑥{𝐸𝑅𝑅, 𝐸𝑅𝑆}.
  optimalPolicyLast = determineOptimal(policyHV = policyHV, policyLV = policyLV)
  
  
  # Process of backward optimal strategy selection
  #𝐸𝑅𝑅(𝑜𝑅, 𝑠, 𝑇 − 2) = ∑ 𝑎∗(𝑠𝑇−2 + 𝑥𝑖, 𝑇 − 1) × 𝑝𝑖 𝑖
  if(timeHorizon > 1){
    optBeforeLast = makePoliciesElse(optimalPolicy = optimalPolicyLast)
  }
  


  # Combined list with all trials and states ===================================

  # make list with all policies
  lasttrial = rbind( optimalPolicyLast, policyHV, policyLV)
  completealloptimal = vector("list", timeHorizon)
  
  if(timeHorizon > 1){
    for(t in 1:(timeHorizon-1)){
      completealloptimal[[t]] = rbind(optBeforeLast[[t]])
    }
  }
  
  completealloptimal[[timeHorizon]] = rbind(lasttrial)
  
  
  # add states (states in trial 1)
  completealloptimal[[1]] = as.data.table(t(rbind(trial = 1, state = start, completealloptimal[[1]], ph = 1, pl = 1)))
  
  # add states for higher trials
  if(timeHorizon > 1){
  t = 2
   for(i in 1:(length(statesHV)-1)) {
     completealloptimal[[t]] = as.data.table(t(rbind(trial = t, state = statesHV[[i]][1,],completealloptimal[[t]],ph = statesHV[[i]][2,], pl = statesLV[[i]][2,])))
    t = t + 1
   }
  }
  
  completealloptimal = rbindlist(completealloptimal)
  completealloptimal = completealloptimal[order(trial,state),.(trial,state,policyHV,policyLV,ph,pl)]

  
 
  # Processing list ============================================================
  # delete impossible states (not complete, only delete states which resulted from "getting" an outcome with p=0 in the previous trial (t before current trial))
  # does not delete states that resulted from "getting" an outcome with p=0 before the previous trial (t=1 to t(before current trial-1))
  alloptimal = completealloptimal[(ph != 0 | pl != 0),]
  alloptimal = alloptimal[order(trial,state),.(trial,state,policyHV,policyLV)]

  # remove duplicated
  alloptimal = unique(alloptimal)

  
  # deletes states of 0 if there is no original outcome that offers 0 and there are probabilities = 0 (because for outcomes with p=0 we artificially set the outcome value = 0)
  if(any(notzero == 0) & !any(originaloutcomes == 0)){
    alloptimal_withoutZero = alloptimal[trial > 1 & state != 0,] 
    alloptimal_t1 = alloptimal[trial == 1]
    alloptimal = rbind(alloptimal_t1,alloptimal_withoutZero)
  }
  
  alloptimal = alloptimal[order(trial,state),.(trial,state,policyHV,policyLV)]
  completealloptimal = completealloptimal[order(trial,state),.(trial,state,policyHV,policyLV)]
  
  # delete impossible states (deletes states that resulted from adding an possible outcome to an impossible outcome)
  alloptimal = deleteImpossible(alloptimal)
  
  
  ERdata = function(compact=alloptimal, extended=completealloptimal){
    return(new("RSFT",
               compact = compact,
               extended = extended)) # extended = only use for prstate calculation
  }
  
  TMfinal = ERdata()
  
  # Filter all or given trials and states ======================================
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
