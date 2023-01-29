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


computeExpectedReward = function(ss,xh,yh,pxh,xl,yl,pxl,b,trial,state){

  ob = 0:ss
  belx = (ob + 1)/ (ob + 1 + ss-ob + 1)
  bely = 1 - belx
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2)))
  
  data = data.table(xh = xh, yh = yh, pxh = pxh, pyh = 1 - pxh,
                    xl = xl, yl = yl, pxl = pxl, pyl = 1 - pxl,
                    bxh = belx[com[,1]], byh = bely[com[,1]],
                    bxl = belx[com[,2]], byl = bely[com[,2]],
                    t = trial, s = state, b = b)
  
  
  rsft_opt = lapply(1:nrow(data), function(i){
    d = data[i,]
    # rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
    m <- rsftModel(c(d$xh,d$yh),c(d$xl,d$yl),c(d$pxh,d$pyh),c(d$pxl,d$pyl),d$b,ntrials,0,gtrials = d$t, gstates = d$s)
    choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV,policyLV)],0.2))
    print(i)
    return(as.data.table(cbind(
      xh = m@compact$policyHV,
      xh = m@compact$policyLV)))
  }
  )
  
  rsft_opt = rbindlist(rsft_opt)
  opt = as.matrix(rsft_opt)
  colnames(opt) = c("xh","yh")
  
  
  rsft_sub = lapply(1:nrow(data), function(i){
    d = data[i,]
    # rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
    m <- rsftModel(c(d$xh,d$yh),c(d$xl,d$yl),c(d$bxh,d$byh),c(d$bxl,d$byl),d$b,ntrials,0,gtrials = d$t, gstates = d$s)
    choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV,policyLV)],0.2))
    print(i)
      return(as.data.table(cbind(
        xh = m@compact$policyHV,
        xh = m@compact$policyLV)))
  }
  )
  
  rsft_sub = rbindlist(rsft_sub)
  ER = as.matrix(rsft_sub)
  colnames(ER) = c("xh","yh")
  
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


