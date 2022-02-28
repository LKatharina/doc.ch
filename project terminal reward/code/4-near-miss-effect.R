# Load Packages -------------------------------------------------------------
pacman::p_load(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R") 

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

stimuli_j = unique(d2[,.(xh,yh,pxh,pyh,xl,pxl,yl,pyl,budget,start)])
stimuli_j[,nr := 1:nrow(stimuli_j)]

# Opt Strategy  -------------------------------------------------------
stimuli2 = unique(d2[,.(xh,yh,pxh,pyh,xl,pxl,yl,pyl,budget,start,trial,state)])

d2diff = lapply(1:nrow(stimuli2), function(i){
  
  d = stimuli2[i,]
  
  m = rsftModel(d$xh, d$yh, d$xl, d$yl,
                d$pxh, d$pyh, d$pxl, d$pyl,
                d$budget,
                timeHorizon = 5,
                0,
                gtrials = d$trial,
                gstates = d$state)
  
  
  return(cbind(
    policyHV = m@compact$policyHV,
    policyLV = m@compact$policyLV,
    d))
}
)

d2diff = rbindlist(d2diff)
d2diff[is.na(d2diff$policyHV) == T | is.na(d2diff$policyHV) == T]

d2 = d2[ pid != 7]
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


d2 = d2[,.(pid,nr,xh,pxh,yh,pyh,xl,pxl,yl,pyl,budget,start,trial,state,choice,difficulty,policyHV,policyLV)]
d2[,dataset := "Jarecki et al. \n (2020)"]


# Opt Strategy  -------------------------------------------------------
stimuli1 = unique(d1[,.(xh,yh,pxh,pyh,xl,pxl,yl,pyl,budget,start,trial,state)])

d1diff = lapply(1:nrow(stimuli1), function(i){
  
  d = stimuli1[i,]
  
  m = rsftModel(d$xh, d$yh, d$xl, d$yl,
                d$pxh, d$pyh, d$pxl, d$pyl,
                d$budget,
                timeHorizon = 5,
                d$start,
                gtrials = d$trial,
                gstates = d$state)
  
  
  return(cbind(
    policyHV = m@compact$policyHV,
    policyLV = m@compact$policyLV,
    d))
}
)

d1diff = rbindlist(d1diff)
d1diff = d1diff[is.na(d1diff$policyHV) == F | is.na(d1diff$policyHV) == F]

d1 = merge(d1,d1diff, by = c("xh","yh","xl","yl","pxh","pyh","pxl","pyl","budget","start","trial","state"))
d1 = d1[order(pid,nr,trial,state)]

d = rbind(d1,d2)

nme = function(x,s,g){
  val = ifelse((x + s) >= g,1,0)
  return(val)
}


d[, valxh := nme(xh,state,budget), by = 1:nrow(d)]
d[, valyh := nme(yh,state,budget), by = 1:nrow(d)]
d[, valxl := nme(xl,state,budget), by = 1:nrow(d)]
d[, valyl := nme(yl,state,budget), by = 1:nrow(d)]
d[, valh := valxh + valyh, by = 1:nrow(d)]
d[, vall := valxl + valyl, by = 1:nrow(d)]
sd = d[valh == 1 & vall == 1 & xh > 0]
sd[,mean(choice)]

