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


#stimuli1 <- read.table("../stimuli/dfe_stimuli-all-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
#stimuli1 = as.data.table(stimuli1)
#stimuli1 = stimuli1[nr == "420"]
#stimuli <- read.table("../stimuli/dfe-stimuli-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
#stimuli <- fread("../stimuli/dfe_stimuli-all-10-t3.csv")
#stimuli <- fread("../stimuli/dfe_stimuli-10-t3-3108.csv")
stimuli <- fread("../stimuli/dfe_stimuli-10-t3-3057.csv")

#stimuli[,optdiff := abs(hvalue - lvalue)]
#stimuli[,sumoptdiff := sum(optdiff), by="nr"]

#ex = stimuli[!(lvalue %in% c(0,1)) & !(hvalue %in% c(0,1)) & dh != 1 & dl != 1][order(-optdiff, -sumoptdiff)][6,]

ntrials = 3

# stimuli1[,t := 1]
# stimuli1[,s := 0]


computeExpectedReward = function(ss,xh,yh,pxh,xl,yl,pxl,b,t,s){

  ob = 0:ss
  belx = (ob + 1)/ (ob + 1 + ss-ob + 1)
  bely = 1 - belx
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2)))
  
  data = data.table(xh = xh, yh = yh, pxh = pxh, pyh = 1 - pxh,
                    xl = xl, yl = yl, pxl = pxl, pyl = 1 - pxl,
                    bxh = belx[com[,1]], byh = bely[com[,1]],
                    bxl = belx[com[,2]], byl = bely[com[,2]],
                    t = t, s = s, b = b)
  
  
  rsft_opt = lapply(1:nrow(data), function(i){
    d = data[i,]
    # rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
    m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$pxh,d$pyh,d$pxl,d$pyl,d$b,ntrials,0,gtrials = d$t, gstates = d$s)
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
    m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$bxh,d$byh,d$bxl,d$byl,d$b,ntrials,0,gtrials = d$t, gstates = d$s)
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

computeAbserdiff = function(ss,xh,yh,pxh,xl,yl,pxl,b,t,s){
  binomh = dbinom(0:ss,ss,pxh)
  binoml = dbinom(0:ss,ss,pxl)
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2))) # N! / (N-k)! * k! = Kombination ohne Wiederholung
  ER_opt = computeExpectedReward(ss,xh,yh,pxh,xl,yl,pxl,b,t,s)
  ER = ER_opt[[1]]
  opt = ER_opt[[2]]
  subdiff = abs(ER[,1] - ER[,2]) %*% (binomh[com[,1]] * binoml[com[,2]])
  optdiff = abs(opt[1,1] - opt[1,2])
  return(list(subdiff = subdiff[1,1], optdiff = unname(optdiff)))
}

computePrOptimal = function(ss,xh,yh,pxh,xl,yl,pxl,b,t,s){
  binomh = dbinom(0:ss,ss,pxh)
  binoml = dbinom(0:ss,ss,pxl)
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2)))
  ER_opt = computeExpectedReward(ss,xh,yh,pxh,xl,yl,pxl,b,t,s)
  ER = cr_argmax(ER_opt[[1]])
  opt = cr_argmax(ER_opt[[2]])
  
  propt = as.numeric((opt[,1] - opt[,2]) == (ER[,1] - ER[,2])) %*% (binomh[com[,1]] * binoml[com[,2]])
  return(propt[,1])
}

d = ex



plotdata_opt = data.table(ss = 1:50)
# Proportion of choosing better option
#d = stimuli[nr %in% ex$nr & trial == 1]
d =stimuli1[trial == 1]
d[, s := state]
d[, t := trial]
plotdata_opt[,pr := computePrOptimal(ss,xh = d$xh, d$yh, d$pxh ,d$xl ,d$yl ,d$pxl , d$b, d$t, d$s), by = ss]


# Advantage
#advantage = stimuli[!(lvalue %in% c(0,1)) & !(hvalue %in% c(0,1)) & dh != 1 & dl != 1][order(-optdiff)][1,]
# d = stimuli1[nr %in% ex$nr & trial == 1]
# d[,s := 0]
# d[,t := 1]
plotdata_adv = data.table(ss = 1:50)
advantage = ex[ trial == 3 & state == 16][, s := state]
advantage[,t := trial]
d = advantage

plotdata_adv[,c("subdiff", "optdiff") := computeAbserdiff(ss,xh = d$xh, d$yh, d$pxh ,d$xl ,d$yl ,d$pxl , d$b, d$t, d$s), by = ss]


#saveRDS(plotdata, "../stimuli/plotdata-t3-50.rds")

