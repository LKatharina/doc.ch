# ==========================================================================
# apply dfe-bayes.R
# ==========================================================================
pacman::p_load(data.table)
library(cognitivemodels)
library(cognitiveutils)

# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")
source("3-dfe-bayes.R")

db <- read.table("../stimuli/dfe-stimuli-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
db <- as.data.table(db)

# #RSFT task
nt = 5 #trials
# b = c(stimuli$b[1]) # budget
# 
# # Sampling
small = 3 # samplesize: large vs. small
large = 25 # samplesize: large vs. small
sampleNR = 100 # number of different samples (binominal distribution) / number of participants per sample size
# # Beta distribution
ndrawsbeta = 500 # draws from beta distribution
seed = 42


dataset = function(stimuli){
  bayes_data = lapply(1:nrow(stimuli), function(i){
    d = stimuli[i,]
    db <- createData(pxh = d$pxh, pxl = d$pxl, xh = d$xh, yh = d$yh, xl = d$xl, yl = d$yl, b = d$b, small = small, large = large
                     , sampleNR = sampleNR, ndrawsbeta = ndrawsbeta)
    
    return(cbind(
      db,
      stimulinr = d$nr
    ))
  }
  )
  
  d1 = rbindlist(bayes_data)
  return(d1)
}

dataset(db[1,],"dfe_stimuli-20-t5-420")


fwrite(dataset(db[dhbin == "(0.2,0.4]"]), "../stimuli/dfe_stimuli-20-t5-d20.csv")
fwrite(dataset(db[dhbin == "(0.4,0.6]"]), "../stimuli/dfe_stimuli-20-t5-d40.csv")
fwrite(dataset(db[dhbin == "(0.6,0.8]"]), "../stimuli/dfe_stimuli-20-t5-d60.csv")
fwrite(dataset(db[dhbin == "(0.8,0.9]"]), "../stimuli/dfe_stimuli-20-t5-d80.csv")



