# ==========================================================================
# Data Table DFE
# ==========================================================================

# Load Packages-------------------------------------------------------------
pacman::p_load(data.table)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)

# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("2-dfe-select.R")


# Variables ----------------------------------------------------------------
# outcomes and probabilities
pxh = stimuli$pxh[1]
pxl = stimuli$pyl[1]
xh = stimuli$xh[1]
yh = stimuli$yh[1]
xl = stimuli$xl[1]
yl = stimuli$yl[1]

#RSFT task
t = 3 #trials
b = c(17,19) # budget

# Sampling
small = 3 # samplesize: large vs. small
large = 10 # samplesize: large vs. small
sampleNR = 3 # number of different samples (binominal distribution)

# Beta distribution
ndrawsbeta = 100 # draws from beta distribution


createData <- function(pxh,pxl,xh,yh,yl,b,small,large, sampleNR, ndrawsbeta){
  count_xh = NULL
  count_xl = NULL
  count_player = c(small,large)
  tau = 0.2
  choicerule = "softmax"
  
  for (player in count_player) {
    for(sample in 1:sampleNR){
      outh = rbinom(n=player, size=1, prob=pxh)
      outl = rbinom(n=player, size=1, prob=pxl)
      count_xh = c(count_xh,sum(outh))
      count_xl = c(count_xl,sum(outl))
    }
  }
  
  data = data.table(numberOfdraws = rep(count_player,each = sampleNR),
             sampleNR = rep(1:sampleNR,2),
             count_xh = count_xh,
             count_xl = count_xl,
             xh = rep(xh,length(count_xh)),
             yh = rep(yh,length(count_xh)),
             xl = rep(xl,length(count_xh)),
             yl = rep(yl,length(count_xh)),
             pxh = rep(pxh,length(count_xh)),
             pyh = rep((1-pxh), length(count_xh)),
             pxl = rep(pxl,length(count_xh)),
             pyl = rep((1-pxl),length(count_xh))
             )
  
  x = 1
  d <- data
  while(x < length(b)){
    data = rbind(data,d)
    x = x + 1
  }
    
  vb = rep(b,each = sampleNR * length(count_player))
  data[,b := vb]
  data[,count_yh := numberOfdraws - count_xh]
  data[,count_yl := numberOfdraws - count_xl]
  
  M = bayes_beta_c( ~ count_xh + count_xl,
                     data = data,
                     format = "count",
                     fix = list(delta = 1,
                                priorpar = c(1,1),
                                sigma = 0.01))
  data[, pmax := predict(M)]
  data[, nr := 1:nrow(data)]
  data[, id := paste(b,numberOfdraws,sampleNR, sep="_")]
  
  i <- 1
  data_bayes <- lapply(1:nrow(data), function(i) {
    j = 1
    bxh <- rbeta(n = ndrawsbeta,data$count_xh[i], data$count_yh[i])
    bxl <- rbeta(n = ndrawsbeta,data$count_xl[i], data$count_yl[i])
    d <- data[j,]
    while(j < ndrawsbeta){
       d <- rbind(d,data[i,])
       j = j + 1
    }
    dat <- cbind(d,bxh,bxl)
    return(dat)
  }
  )
  
  # Make the list into one data table (data frame)
  data_bayes <- rbindlist(data_bayes)
  data_bayes[, byh := 1 - bxh]
  data_bayes[, byl := 1 - bxl]
  data_bayes[, start := 0]
  data_bayes[, betadraw := 1:nrow(data_bayes)]
  
  
  # 1. RSFT model -----------------------------------------
  rsft_model <- hm1988(
    ~ xh + bxh + yh + byh | xl + bxl  + yl + byl,  # our formula (as before)
    trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
    data = data_bayes,         # our data (as before)
    budget = ~b,      # name of our budget column in our data
    initstate = ~start,    # name of our starting-state column in our data
    ntrials = t,            # we always 5 trials therefore I hard-code this
    states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
    choicerule = choicerule,
    fix = list(tau = tau))
  
  predict(rsft_model)
  
  
  rsft_sim <- data.table(
    trial = (t + 1) - rsft_model$get_timehorizons(), # get trials that are *remaining*
    state =   rsft_model$get_states(),         # get possible states
    prhv_rsft =    rsft_model$predict("response"),  # get pr(hv) prediction
    prstate = rsft_model$predict("pstate"), # get pr(state)
    hvalue = predict(rsft_model, type="values")[,1],
    lvalue = predict(rsft_model, type="values")[,2]
  )
  
  
  # Stimuli nr
  rsft_sim[, betadraw := cumsum(trial == 1)]
  db <- merge(rsft_sim,data_bayes)
  
  return(db)
}

db <- createData(pxh = pxh, pxl = pxl, xh = xh, yh = yh, yl = yl, b = b, small = small, large = large
           ,sampleNR = sampleNR, ndrawsbeta = ndrawsbeta)

fwrite(db, "../stimuli/example_bayes_data.csv")

