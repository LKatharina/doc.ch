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


stimuli <- read.table("../stimuli/dfe-stimuli-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
stimuli = as.data.table(stimuli)
stimuli[,s := 0]
stimuli[,t := 1]

ntrials = 5
d = stimuli[1,]

xh = d$xh 
yh = d$yh
pxh = d$pxh
xl = d$xl
yl = d$yl
pxl = d$pxl
b = d$b
t = d$t
s = d$s
ss = 4

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
    m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$pxh,d$pyh,d$pxl,d$pyl,d$b,5,0,gtrials = d$t, gstates = d$s)
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
    m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$bxh,d$byh,d$bxl,d$byl,d$b,5,0,gtrials = d$t, gstates = d$s)
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
  
  # rsft_opt <- hm1988(
  #   ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
  #   trials = ~t,        # NEW: ".ALL" will predict for *all possible* trials
  #   data = data,            # our data (as before)
  #   budget = ~b,       # name of our budget column in our data
  #   initstate = 0,     # name of our starting-state column in our data
  #   ntrials = ntrials,            # we always 5 trials therefore I hard-code this
  #   states = ~s,        # NEW: ".ALL" will predict for *all possible* states
  #   choicerule = "softmax",
  #   fix = list(tau = 0.2))
  # 
  # opt = predict(rsft_opt, type="values")
  # 
  # rsft_sub <- hm1988(
  #   ~ xh + bxh + yh + byh | xl + bxl  + yl + byl,  # our formula (as before)
  #   trials = ~t,        # NEW: ".ALL" will predict for *all possible* trials
  #   data = data,            # our data (as before)
  #   budget = ~b,       # name of our budget column in our data
  #   initstate = 0,     # name of our starting-state column in our data
  #   ntrials = ntrials,            # we always 5 trials therefore I hard-code this
  #   states = ~s,        # NEW: ".ALL" will predict for *all possible* states
  #   choicerule = "softmax",
  #   fix = list(tau = 0.2))
  # 
  # ER =  predict(rsft_sub, type="values")
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
  optdiff = abs(opt[,1] - opt[,2])
  return(c(subdiff = subdiff, optdiff = optdiff))
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

d = stimuli[1,]
plotdata = data.table(ss = 1:20)

plotdata[,pr := computePrOptimal(ss,xh = d$xh, d$yh, d$pxh ,d$xl ,d$yl ,d$pxl , d$b, d$t, d$s), by = ss]


ggplot(plotdata, aes(x = ss, y = pr))+
  geom_line() +
  scale_x_continuous(breaks = 1:max(plotdata$ss)) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1))

fwrite(plotdata, "../stimuli/plotdata-420-30-t5.csv")

?scale_x_continuous
