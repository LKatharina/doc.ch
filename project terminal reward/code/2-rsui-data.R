#============================================================================
# Risk Seeking under Impossibility: Data sets
# 
#============================================================================

# Load Packages -------------------------------------------------------------
pacman::p_load(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R") 
source("1-rsui-function.R")

d1 <- fread("../data/rsft-gain-loss-processed.csv")
d2 <- fread("../data/rsft-dcpt-processed.csv")

# clean data 1
d1 = d1[,.(pid,nr,xh,pxh,yh,pyh,xl,pxl,yl,pyl,budget,start,trial,state,choice,difficulty)]
d1[,dataset := "Mangold et al. \n (2021)"]


# clean data 2
d2 = d2[phase == "five"]
pids = data.table(id = unique(d2$id),
      pid = 1:length(unique(d2$id)))

d2 = merge(d2,pids, by="id")
d2[, c("pxh","pyh","pxl","pyl","choice","start") := list(ph, 1-ph, pl, 1-pl,choice_1isHighVar,0)]
d2 = d2[,.(pid,xh,pxh,yh,pyh,xl,pxl,yl,pyl,budget,start,trial,state,choice)]
d2 = d2[ pid != 7]

stimuli_j = unique(d2[,.(xh,yh,pxh,pyh,xl,pxl,yl,pyl,budget,start)])
stimuli_j[,nr := 1:nrow(stimuli_j)]

# Impossible States ??? -------------------------------------------------------
stimuli2 = unique(d2[,.(xh,yh,pxh,pyh,xl,pxl,yl,pyl,budget,start,trial,state)])

d2diff = lapply(1:nrow(stimuli2), function(i){ # Returns warnings due to impossible states
  
  d = stimuli2[i,]
  
  m = rsftModel(d$xh, d$yh, d$xl, d$yl,
                d$pxh, d$pyh, d$pxl, d$pyl,
                d$budget,
                timeHorizon = 5,
                0,
                gtrials = d$trial,
                gstates = d$state
                )
  
  
  return(cbind(
    policyHV = m@compact$policyHV,
    policyLV = m@compact$policyLV,
    d))
}
)

d2diff = rbindlist(d2diff)


d2diff = d2diff[is.na(d2diff$policyHV) == F | is.na(d2diff$policyHV) == F]

d2 = merge(d2,d2diff, by = c("xh","yh","xl","yl","pxh","pyh","pxl","pyl","budget","start","trial","state"))
d2 = merge(d2,stimuli_j, by = c("xh","yh","xl","yl","pxh","pyh","pxl","pyl","budget","start"))
d2 = d2[order(pid,nr,trial,state)]


# Difficulty -----------------------------------------------------------------------------
difflevels = lapply(1:nrow(stimuli_j), function(i){
  
  d = stimuli_j[i,]
  
  m = rsftModel(d$xh, d$yh, d$xl, d$yl,
                d$pxh, d$pyh, d$pxl, d$pyl,
                d$budget,
                timeHorizon = 5,
                d$start)
  
  
  return(cbind(
    m@compact,
    nr = i))
}
)

difflevels = rbindlist(difflevels)
difflevels = difflevels[trial == 1][,.(max(c(policyHV,policyLV))), by =  c("nr")]


# Effect of difficulty
difflevels[,difficulty := ifelse(V1 < 0.55,"hard","easy")]
difflevels[,difficulty := ifelse(V1 < 0.72 & V1 >= 0.55,"medium",difficulty)]
difflevels[,V1 := NULL]
d2 = merge(d2,difflevels[,.(nr,difficulty)], by = "nr")


d2 = d2[,.(pid,nr,xh,pxh,yh,pyh,xl,pxl,yl,pyl,budget,start,trial,state,choice,difficulty)]
d2[,dataset := "Jarecki et al. \n (2020)"]

imp1 = RiskseekingUnderImpossibility(d1)
imp2 = RiskseekingUnderImpossibility(d2)

impossible = rbind(imp1,imp2)
impossible[,.(choice = mean(choice), prhv = mean(prhv)),by=c("parameter","dataset")]
