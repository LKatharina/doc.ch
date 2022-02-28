#===========================================================================
# Apply opt sampler functions
#
#===========================================================================

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
source("7-dfe-optsampler.R")

ntrials = 3

#stimuli1 <- read.table("../stimuli/dfe_stimuli-all-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA")
#stimuli <- read.table("../stimuli/dfe-stimuli-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))

stimuli <- fread("../stimuli/dfe_stimuli-all-10-t3.csv")
#stimuli <- fread("../stimuli/dfe_stimuli-10-t3-3108.csv")
#stimuli <- fread("../stimuli/dfe_stimuli-10-t3-3057.csv")

stimuli[,optdiff := abs(hvalue - lvalue)]
stimuli[,sumoptdiff := sum(optdiff), by="nr"]

#s = stimuli[!(lvalue %in% c(0,1)) & !(hvalue %in% c(0,1)) & dh != 1 & dl != 1 & hvalue != lvalue][order(-optdiff)][trial == 3]

# Proportion of choosing better option
#d = stimuli[nr %in% ex$nr & trial == 1]
opt = stimuli[!(lvalue %in% c(0,1)) & !(hvalue %in% c(0,1)) & dh != 1 & dl != 1 & hvalue != lvalue][order(-sumoptdiff,-optdiff)][trial == 1]
dopt = opt[nr %in% c(676,3057,3108)]


plotdata_opt = lapply(1:nrow(dopt), function(i){
  d = dopt[i]
  d[, s := state]
  d[, t := trial]
plotdata_opt = data.table(ss = 1:10)
plotdata_opt[,pr := computePrOptimal(ss,xh = d$xh, d$yh, d$pxh ,d$xl ,d$yl ,d$pxl , d$b, d$t, d$s), by = c("ss")]
return(cbind(
  plotdata_opt,
  nr = d$nr))
}
)

plotdata_opt = rbindlist(plotdata_opt)
plotdata_opt = merge(plotdata_opt,dopt[,.(xh,yh,pxh,pyh,xl,yl,pxl,pyl,b,trial,nr)], by="nr")
plotdata_opt[,stimuli := paste0(xh," (",100*pxh,"%) or ",yh, " vs. ", xl," (",100*pxl,"%) or ",yl)]

saveRDS(plotdata_opt, "../stimuli/plotdata_opt-t1-30.rds")

# Advantage
# 3108: 16 / 3110: 18 / 3057: 16
sel = stimuli[nr %in% c(3108,3057,3110)][trial == 3]

advantage = stimuli[nr == 3057 & state == 16 & trial == 3]
# d = stimuli1[nr %in% ex$nr & trial == 1]
# d[,s := 0]
# d[,t := 1]
plotdata_adv = data.table(ss = 1:50)
advantage[, s := state]
advantage[,t := trial]
d = advantage

plotdata_adv[,c("subdiff", "optdiff") := computeAbserdiff(ss,xh = d$xh, d$yh, d$pxh ,d$xl ,d$yl ,d$pxl , d$b, d$t, d$s), by = ss]


#saveRDS(plotdata, "../stimuli/plotdata-t3-50.rds")

