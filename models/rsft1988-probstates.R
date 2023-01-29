###########################################################
# Houston and McNamara 1988: RISK SENSITIVE FORAGING MODEL
# Calculates Probability for the states
#
############################################################

library(data.table)

setClass(Class="prstates",
         representation(
           prstate = "data.table",
           prfinal = "data.table"
         )
)

# PRSTATE ======================================================================

rsftStates <- function(riskyX, safeX, riskyP, safeP, goal, timeHorizon, start,choiceprob,extendedModel = NULL,final){
  if(!is.null(extendedModel)){
    choiceprob = cbind(extendedModel[,.(trial,state)],choiceprob)
  } 
  
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
  
  
  # Function: DECISION TREE ====================================================
  growTree = function(outcomes, phv, plv, timeHorizon, start){
    results = vector("list", timeHorizon)
    results[[1]] = rbind(state = (outcomes + start), pr = phv + plv, probHV = phv, probLV = plv, chv = 1, clv = 1)
    
    # build decision tree (trial 1 missing) ------------------------------------
    for(t in 2:timeHorizon){
      states = NULL
      probHV = NULL
      probLV = NULL
      pr = NULL
      chv = NULL
      clv = NULL
      
      for(i in results[[t-1]][1,]){
        states = c(states, (i + outcomes))
      }
      
      for(i in results[[t-1]][2,]){
        probHV = rep(phv, Noutcomes^(t-1))
        probLV = rep(plv, Noutcomes^(t-1))
        pr = probHV + probLV
      }
      
      results[[t]] = rbind(state = states, pr = pr, probHV = probHV, probLV = probLV)
    }
    
    # decision tree: add trial 1
    allstates = vector("list", timeHorizon+1)
    allstates[[1]] = rbind(state = (start), pr = 1, probHV = 1, probLV = 1, trial = 1)
    
    for(t in 2:(timeHorizon+1)){
      allstates[[t]] = rbind(state = results[[t-1]][1,], pr = results[[t-1]][2,], trial = t, probHV = results[[t-1]][3,], probLV = results[[t-1]][4,])
    }
    
    # decision decision: expand states -----------------------------------------
    expallstates = vector("list", timeHorizon+1)
    
    for(t in 1:timeHorizon){
      factor = Noutcomes^(timeHorizon+1-t) #state repetitions 
      states = as.vector(rep(allstates[[t]][1,], each=factor))
      pr = as.vector(rep(allstates[[t]][2,], each=factor))
      szenario = 1:Noutcomes^timeHorizon
      probHV = as.vector(rep(allstates[[t]][4,], each=factor))
      probLV = as.vector(rep(allstates[[t]][5,], each=factor))
      
      # Determine if scenario is original or not
      if(t == 1){
        del = c(1,rep(0,Noutcomes^timeHorizon-1))
      } else {
        del = c(rep(c(1,rep(0,Noutcomes^timeHorizon / Noutcomes^(t-1)-1)),Noutcomes)) # 1 = original, 0 = extensions (delete later)
      }
      expallstates[[t]] = rbind(state = states, pr=pr,trial = t, szenario = szenario, probHV= probHV, probLV = probLV, del = del)
    }
    
    # add final state
    expallstates[[timeHorizon+1]] = allstates[[timeHorizon+1]]
    expallstates[[timeHorizon+1]] = rbind(state = expallstates[[timeHorizon+1]][1,],
                                          pr = expallstates[[timeHorizon+1]][2,],
                                          trial = expallstates[[timeHorizon+1]][3,],
                                          szenario = 1:Noutcomes^timeHorizon,
                                          probHV = expallstates[[timeHorizon+1]][4,],
                                          probLV = expallstates[[timeHorizon+1]][5,],
                                          del = 1
                                          )
    # data table
    for(t in 1:(length(expallstates))) {
      expallstates[[t]] = as.data.table(t(rbind(expallstates[[t]])))
    }
    
    return(rbindlist(expallstates))
  }

  # FUNCTION: Weighting probabilities with pred choices ========================
  expandchoiceprob = function(choiceprob, timeHorizon){
    results = vector("list", timeHorizon+1)
    results[[1]] = rbind(state = start, chv = 1, clv = 1, trial = 1)

    for(t in 2:(timeHorizon+1)){
      chv = NULL
      clv = NULL
      states = NULL
      for(i in choiceprob[trial == (t-1),state]){
        states = c(states,(i + outcomes))
      }
      for(i in choiceprob[trial == (t-1),policyHV]){
        chv = c(chv,c(rep(i,Noutcomes/2),rep(0,Noutcomes/2)))
      }
      for(i in choiceprob[trial == (t-1),policyLV]){
        clv = c(clv,c(rep(0,Noutcomes/2),rep(i,Noutcomes/2)))
      }
      
      results[[t]] = rbind(state = states, chv = chv, clv = clv, trial = t)
    }
    
    # expand states
    expallstates = vector("list", timeHorizon+1)
    for(t in 1:(timeHorizon+1)){
      factor = Noutcomes^(timeHorizon+1-t)
      states = as.vector(rep(results[[t]][1,], each=factor))
      chv = as.vector(rep(results[[t]][2,], each=factor))
      clv = as.vector(rep(results[[t]][3,], each=factor))
      szenario = 1:Noutcomes^timeHorizon
      expallstates[[t]] = rbind(state = states, trial = t, szenario = szenario, chv= chv, clv= clv)
    }
    
    for(t in 1:(length(expallstates))) {
      expallstates[[t]] = as.data.table(t(rbind(expallstates[[t]])))
    }
    
    return(rbindlist(expallstates))
  }
  
  # APPLY FUNCTIONS=============================================================
  
  # States and objective probabilities
  states = growTree(outcomes, phv = phv, plv, timeHorizon = timeHorizon, start = start)
  states = states[order(szenario,trial)]
  states[del == 1][order(trial,state)]
  
  # multiply probabilities of the outcomes with choice probabilities
  if(class(choiceprob)[1] == "data.table"){
    choiceprobext = expandchoiceprob(choiceprob = choiceprob, timeHorizon = timeHorizon) # use predicted choices
    choiceprobext =  choiceprobext[order(szenario,trial)]
    d = cbind(states, choiceprobext[,.(chv,clv)])
    d[, pr := ifelse(probHV != 0,probHV*chv,probLV*clv)]
  } else {
    d = states
    d[,pr := ifelse(probHV != 0,probHV*0.5,probLV*0.5)] # Here I assume random choices
    d[trial == 1, pr := 1]
  }

  d[,cumprob := cumprod(pr),by=szenario]
  smalld = d[del == 1] #keep originals
  probstates = smalld[order(trial,state),.(prstate = sum(cumprob)), by=c("trial","state")]
  probstates = probstates[prstate != 0]
  
  prstateTable = new("prstates",
                     prstate = probstates[trial != max(trial)],
                     prfinal = probstates[trial == max(trial),.(state = state, prstate)])
  
  if(final == T){
    return(prstateTable@prfinal)
  } else {
    return(prstateTable@prstate)
  }
  
}
  