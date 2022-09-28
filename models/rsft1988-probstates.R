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

# FUNCTIONS ====================================================================

rsftStates <- function(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start,choiceprob,extendedModel = NULL,final){
  if(class(choiceprob)[1] == "data.table"){
    choiceprob = cbind(extendedModel[,.(trial,state)],choiceprob)
  } else {
    
  }
  
  
  originaloutcomes = c(xh,yh,xl,yl)
  
  notzero = c(pxh,pyl,pxl,pyl)
  if(any(notzero== 0)){
    if(pxh == 0){
      xh = 0
    } else if(pyh == 0){
      xh = 0
    } else if(pxl == 0){
      xl = 0
    } else if(pyl == 0){
      yl = 0
    } else { }
  }  else { }
  
  
  #define variables -------------------------------------------------------------
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
  
  
  # function for decision tree ---------------------------------------------------
  growTree = function(outcomes, phv, plv, timeHorizon, start){
    results = vector("list", timeHorizon)
    results[[1]] = rbind(state = (outcomes + start), pr = phv + plv, probHV = phv, probLV = plv, chv = 1, clv = 1)
    
    for(t in 2:timeHorizon) {
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

        #probstates = c(probstates, rep((i * p * choiceprob),length(outcomes)^(t-1)))
      }
      results[[t]] = rbind(state = states, pr = pr, probHV = probHV, probLV = probLV)
    }
    
    # Add trial 1
    allstates = vector("list", timeHorizon+1)
    allstates[[1]] = rbind(state = (start), pr = 1, probHV = 1, probLV = 1, trial = 1)
    
    for(t in 2:(timeHorizon+1)){
      allstates[[t]] = rbind(state = results[[t-1]][1,], pr = results[[t-1]][2,], trial = t, probHV = results[[t-1]][3,], probLV = results[[t-1]][4,])
    }
    
    # expand states
    expallstates = vector("list", timeHorizon+1)
    for(t in 1:timeHorizon){
      factor = Noutcomes^(timeHorizon+1-t)
      states = as.vector(rep(allstates[[t]][1,], each=factor))
      pr = as.vector(rep(allstates[[t]][2,], each=factor))
      szenario = 1:Noutcomes^timeHorizon
      probHV = as.vector(rep(allstates[[t]][4,], each=factor))
      probLV = as.vector(rep(allstates[[t]][5,], each=factor))
      if(t == 1){
        del = c(1,rep(0,Noutcomes^timeHorizon-1))
      } else {
        del = c(rep(c(1,rep(0,Noutcomes^timeHorizon / Noutcomes^(t-1)-1)),Noutcomes)) # 1 = original, 0 = extensions (delete later)
      }
      expallstates[[t]] = rbind(state = states, pr=pr,trial = t, szenario = szenario, probHV= probHV, probLV = probLV, del = del)
    }
    
    expallstates[[timeHorizon+1]] = allstates[[timeHorizon+1]]
    expallstates[[timeHorizon+1]] = rbind(state = expallstates[[timeHorizon+1]][1,],
                                          pr = expallstates[[timeHorizon+1]][2,],
                                          trial = expallstates[[timeHorizon+1]][3,],
                                          szenario = 1:Noutcomes^timeHorizon,
                                          probHV = expallstates[[timeHorizon+1]][4,],
                                          probLV = expallstates[[timeHorizon+1]][5,],
                                          del = 1
                                          )
    
    for(t in 1:(length(expallstates))) {
      expallstates[[t]] = as.data.table(t(rbind(expallstates[[t]])))
    }
    
    return(rbindlist(expallstates))
  }

  # function: Weighting probabilities with pred choices -----------------
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
  