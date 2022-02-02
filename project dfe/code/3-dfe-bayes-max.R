# ==========================================================================
# Creates rsft predictions based on probability beliefs
# ==========================================================================


# Erstellt Datensatz mit RSFT Vorhersagen basierend auf den Beliefs
# über die Wahrscheinlichkeiten.
# Die Probanden kennen nicht nur die Outcomes, die Wahrscheilichkeiten werden
# durch Erfahrung gelernt (Wiederholtes Samples aus der Binominalverteilung).
# Das Lernen wird durch ein Bayesianisches Lernmodell simuliert, mit jedem Zug aus der
# Binominalverteilung werden die Beliefs über die Wahrscheinlichkeiten angepasst.
# Es gibt wenig Sampler (sample size = 3) und viel Sampler (Sample size = 10)
# Aus der gelernten Wahrscheinlichkeitsverteilung (Beta-Verteilung) wird 100 mal
# gezogen und für jede Ziehung macht RSFT Vorhersagen.


# Load Packages-------------------------------------------------------------
pacman::p_load(data.table)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)

# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")
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
nt = 5 #trials
b = c(stimuli$b[1]) # budget

# Sampling
small = 2 # samplesize: large vs. small
large = 10 # samplesize: large vs. small
sampleNR = 100 # number of different samples (binominal distribution) / number of participants per sample size

# Beta distribution
ndrawsbeta = 100 # draws from beta distribution
seed = 5000

# Function for simulation of learning and choices
createData <- function(pxh,pxl,xh,yh,yl,b,small,large, sampleNR, ndrawsbeta, seed = 5000, choicerule = "softmax", tau = 0.2, prior = c(1,1)){
  #set.seed(seed)
  # count_xh = NULL
  # count_xl = NULL
  count_player = c(small,large)
  
  # Sampling from binominal distribution
  # n: Number of random draws = number of persons = length of output vector
  # size: Number of trials = numnber of samples by person = max count
  count_xh <- c(sapply(count_player, rbinom, n = sampleNR, prob = pxh))
  count_xl <- c(sapply(count_player, rbinom, n = sampleNR, prob = pxl))
  
  nb <- length(b)
  nrowsdata <- sampleNR * length(count_player) * nb
  data = data.table(
    numberOfdraws = rep(count_player, each = sampleNR, times = nb),
    sampleNR = rep(1:sampleNR, times = length(count_player) * nb),
    count_xh = rep(count_xh, nb),
    count_xl = rep(count_xl, nb),
    b = rep(b, each = sampleNR * length(count_player)),
    xh = rep(xh, nrowsdata),
    yh = rep(yh, nrowsdata),
    xl = rep(xl, nrowsdata),
    yl = rep(yl, nrowsdata),
    pxh = rep(pxh, nrowsdata),
    pyh = rep((1-pxh), nrowsdata),
    pxl = rep(pxl, nrowsdata),
    pyl = rep((1-pxl), nrowsdata)
  )
  
  data[,count_yh := numberOfdraws - count_xh]
  data[,count_yl := numberOfdraws - count_xl]
  data[, nr := 1:nrow(data)]
  data[, id := paste(b,numberOfdraws,sampleNR, sep="_")]
  
  # max beta
  data[, c("mbxh", "mbxl") := .(
    ((count_xh + prior[1]) /  (count_xh + prior[1] +count_yh + prior[2])),
    ((count_xl + prior[1]) /  (count_xl + prior[1] +count_yl + prior[2]))
  ),
  by = .(numberOfdraws, sampleNR, b)
  ]
  
  data[, mbyh := 1 - mbxh]
  data[, mbyl := 1 - mbxl]
  data[, start := 0]
  data[, betadraw := 1:nrow(data)]
  cols = c("mbxh","mbyh","mbxl","mbyl")
  data[,c("mbxh","mbyh","mbxl","mbyl") := lapply(.SD,round,digits = 2), .SDcols = cols]
  
  
  #  RSFT model
  rsft = lapply(1:nrow(data), function(i){
    d = data[i,]
    # rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
    m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$mbxh,d$mbyh,d$mbxl,d$mbyl,d$b,5,0)
    choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV,policyLV)],0.2))
    print(i)
    return(cbind(
      policyHV_belief = m@compact$policyHV,
      policyLV_belief = m@compact$policyLV,
      state = m@compact$state,
      trial = m@compact$trial,
      prhv_belief = choiceprob$policyHV
    ))
  }
  )
  
  
  
  for(i in 1:length(rsft)) {
    rsft[[i]] = as.data.table(rsft[[i]])
    print(i)
  }
  
  rsft_sim = rbindlist(rsft)
  
  
  # rsft_model <- hm1988(
  #   ~ xh + bxh + yh + byh | xl + bxl  + yl + byl,  # our formula (as before)
  #   trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
  #   data = data_bayes,         # our data (as before)
  #   budget = ~b,      # name of our budget column in our data
  #   initstate = ~start,    # name of our starting-state column in our data
  #   nt = nt,            # we always 5 trials therefore I hard-code this
  #   states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
  #   choicerule = choicerule,
  #   fix = list(tau = tau))
  # 
  # predict(rsft_model)
  # 
  # 
  # rsft_sim <- data.table(
  #   trial = (nt + 1) - rsft_model$get_timehorizons(), # get trials that are *remaining*
  #   state =   rsft_model$get_states(),         # get possible states
  #   prhv_rsft =    rsft_model$predict("response"),  # get pr(hv) prediction
  #   prstate = rsft_model$predict("pstate"), # get pr(state)
  #   policyHV = predict(rsft_model, type="values")[,1],
  #   policyLV = predict(rsft_model, type="values")[,2]
  # )
  
  
  # Stimuli nr
  rsft_sim[, betadraw := cumsum(trial == 1)]
  db <- merge(rsft_sim,data, by="betadraw")
  
  return(db)
}


db <- createData(pxh = pxh, pxl = pxl, xh = xh, yh = yh, yl = yl, b = b, small = small, large = large
                 ,sampleNR = sampleNR, ndrawsbeta = ndrawsbeta)

fwrite(db, "../stimuli/example_bayes_m20_5.csv")

