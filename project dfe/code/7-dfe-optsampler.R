# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)
?argmax
?cr_argmax
# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

db <- read.table("../stimuli/example_bayes_data2pr.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
db <- as.data.table(db)
db


dbinom(0:5,5,0.76)

# Binomialprop * Chosen Option für alle Kombinationen

# Beobachtungen + Beliefs
ss = 2

computeExpectedReward = function(ss){
  ntrials = 2
  b = 4
  t = 1
  ob = 0:ss
  belx = (ob + 1)/ (ob + 1 + ss-ob + 1)
  bely = 1 - belx
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2)))
  
  
  
  data = data.table(xh = 2.00, yh = 0.00, pxh = 0.99, pyh = 1 - 0.99,
                    xl = 4, yl = 0.00, pxl = 0.55, pyl = 1 - 0.55,
                   bxh = belx[com[,1]], byh = bely[com[,1]],
                   bxl = belx[com[,2]], byl = bely[com[,2]],
                   t = t, s = 0)
                   
  
  rsft_opt <- hm1988(
    ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
    trials = ~t,        # NEW: ".ALL" will predict for *all possible* trials
    data = data,            # our data (as before)
    budget = b,       # name of our budget column in our data
    initstate = 0,     # name of our starting-state column in our data
    ntrials = ntrials,            # we always 5 trials therefore I hard-code this
    states = ~s,        # NEW: ".ALL" will predict for *all possible* states
    choicerule = "softmax",
    fix = list(tau = 0.2))
  
  opt = predict(rsft_opt, type="values")
  
  rsft_sub <- hm1988(
    ~ xh + bxh + yh + byh | xl + bxl  + yl + byl,  # our formula (as before)
    trials = ~t,        # NEW: ".ALL" will predict for *all possible* trials
    data = data,            # our data (as before)
    budget = b,       # name of our budget column in our data
    initstate = 0,     # name of our starting-state column in our data
    ntrials = ntrials,            # we always 5 trials therefore I hard-code this
    states = ~s,        # NEW: ".ALL" will predict for *all possible* states
    choicerule = "softmax",
    fix = list(tau = 0.2))
  
  ER =  predict(rsft_sub, type="values")
  return(list(ER,opt))
}

computeAbserdiff = function(ss){
  binomh = dbinom(0:ss,ss,0.99)
  binoml = dbinom(0:ss,ss,0.55)
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2)))
  ER_opt = computeExpectedReward(ss)
  ER = ER_opt[[1]]
  opt = ER_opt[[2]]
  subdiff = abs(ER[,1] - ER[,2]) %*% (binomh[com[,1]] * binoml[com[,2]])
  optdiff = abs(opt[,1] - opt[,2])
  return(c(subdiff = subdiff, optdiff = optdiff))
}

computePrOptimal = function(ss){
  binomh = dbinom(0:ss,ss,0.99)
  binoml = dbinom(0:ss,ss,0.55)
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2)))
  ER_opt = computeExpectedReward(ss)
  ER = cr_argmax(ER_opt[[1]])
  opt = cr_argmax(ER_opt[[2]])
  
  propt = as.numeric((opt[,1] > opt[,2]) == (ER[,1] > ER[,2])) %*% (binomh[com[,1]] * binoml[com[,2]])
  return(propt[,1])
}

cr_argmax(computeExpectedReward(4)[[1]])
plotdata = data.table(ss = 1:10)
plotdata[,pr := computePrOptimal(ss), by = ss]

ggplot(plotdata, aes(x = ss, y = pr))+
  geom_line() +
  scale_x_continuous(breaks = 1:max(plotdata$ss))
