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
nt = 3 #trials
b = c(17,19) # budget

# Sampling
small = 3 # samplesize: large vs. small
large = 10 # samplesize: large vs. small
<<<<<<< HEAD
sampleNR = 30 # number of different samples (binominal distribution) / number of participants per sample size
=======
sampleNR = 3 # number of different samples (binominal distribution) / number of participants per sample size
>>>>>>> parent of acda3c3 (dfe sample size)

# Beta distribution
ndrawsbeta = 100 # draws from beta distribution


# Function for simulation of learning and choices
createData <- function(pxh,pxl,xh,yh,yl,b,small,large, sampleNR, ndrawsbeta, seed = 42, choicerule = "softmax", tau = 0.2, prior = c(1,1)){
  set.seed(seed)
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
<<<<<<< HEAD
=======
  
  M = bayes_beta_c( ~ count_xh + count_xl,
                     data = data,
                     format = "count",
                     fix = list(delta = 1,
                                priorpar = c(1,1),
                                sigma = 0.01))
  data[, pmax := predict(M)]
>>>>>>> parent of acda3c3 (dfe sample size)
  data[, nr := 1:nrow(data)]
  data[, id := paste(b,numberOfdraws,sampleNR, sep="_")]
  
 
  # make a longer data frame (expand data)
  data_bayes <- data[rep(1:nrowsdata, each = ndrawsbeta)]
  
  # Sampling from beta distribution
  data_bayes[, c("bxh", "bxl") := .(
      rbeta(.N, count_xh + prior[1], count_yh + prior[2]),
      rbeta(.N, count_xl + prior[1], count_yl + prior[2])
    ),
    by = .(numberOfdraws, sampleNR, b)
    ]
  
  data_bayes[, byh := 1 - bxh]
  data_bayes[, byl := 1 - bxl]
  data_bayes[, start := 0]
  data_bayes[, betadraw := 1:nrow(data_bayes)]
  
  
  # RSFT model
  rsft_model <- hm1988(
    ~ xh + bxh + yh + byh | xl + bxl  + yl + byl,  # our formula (as before)
    trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
    data = data_bayes,         # our data (as before)
    budget = ~b,      # name of our budget column in our data
    initstate = ~start,    # name of our starting-state column in our data
    nt = nt,            # we always 5 trials therefore I hard-code this
    states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
    choicerule = choicerule,
    fix = list(tau = tau))
  
  predict(rsft_model)
  
  
  rsft_sim <- data.table(
    trial = (nt + 1) - rsft_model$get_timehorizons(), # get trials that are *remaining*
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

<<<<<<< HEAD
fwrite(db, "../stimuli/example_bayes_data2.csv")
=======
fwrite(db, "../stimuli/example_bayes_data.csv")
>>>>>>> parent of acda3c3 (dfe sample size)

