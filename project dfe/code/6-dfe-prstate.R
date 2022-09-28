# ==========================================================================
# Plot: DFE RSFT
# ==========================================================================

# Erstellt Density Plots

# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)

# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

db <- read.table("../stimuli/example_bayes_data2.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
db <- as.data.table(db)

source("../../models/rsft1988.R")
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")

s = db[trial == 1]
s =s[1:2,]

rsft = lapply(1:nrow(s), function(i){
  d = s[i,]
  # rsftModel(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start)
  m <- rsftModel(d$xh,d$yh,d$xl,d$yl,d$bxh,d$byh,d$bxl,d$byl,d$b,3,0)
  choiceprob = as.data.table(cr_softmax(x = m@extended[,.(policyHV,policyLV)],0.2))
  # rsftStates(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start,choiceprob,final) # final = probability to end in a certain state
  prstates = rsftStates(d$xh,d$yh,d$xl,d$yl,d$bxh,d$byh,d$bxl,d$byl,d$b,3,0,choiceprob,m@extended, F)
  choiceprob = as.data.table(cr_softmax(x = m@compact[,.(policyHV,policyLV)],0.2))
  return(cbind(
    m@compact,
    prhv = choiceprob$policyHV,
    prstate = prstates$prstate))
}
)

rsftdata = rbindlist(rsft)

db = cbind(db,rsftdata$prstate)
fwrite(db, "../stimuli/example_bayes_data2pr.csv")