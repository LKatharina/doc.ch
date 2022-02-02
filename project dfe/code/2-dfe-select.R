# ==========================================================================
# Auswahl der Stimuli
# Auswahlkriterien:
# Schwierigkeit der Zielerreichung
# Prozentsatz an "schlechten" Vorhersagen (wenn RSFT keine Präferenz für risky oder safe vorhersagt)
# ==========================================================================

# Load Packages-------------------------------------------------------------
pacman::p_load(data.table)


# Load data --------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


stimuli <- read.table("../stimuli/dfe_stimuli-all-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
stimuli <- as.data.table(stimuli)


# bad rows ----------------------------------------------------------------------------------

stimuli[,badrows := ifelse(policyHV == policyLV,1,0)]
stimuli[,sumbr := round(sum(badrows)/sum(.N),2), by=c("nr","b")]
stimuli <- stimuli[sumbr < .6] # Exclusion if %bad rows > 60%


# Difficulty Categories ---------------------------------------------------------------------
stimuli[,difficulty := ifelse(dh >= dl,dh,dl)]
stimuli <- stimuli[difficulty <= .90 & difficulty >= .25] # Stimuli with difficulties > 0.85 and < 0.3 are excluded

int <- 0.1
boundaries <- seq(0,0.9, int)
stimuli[, dhbin := cut(difficulty, boundaries, include.lowest = T, labels = F)]
stimuli[, dhbin := cut(difficulty, c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9), include.lowest = T)]


stimuli <- stimuli[trial == 1,]


stimuli[,diffh := abs(pxh-pyh)]
stimuli[,diffl := abs(pxl-pyl)]
stimuli[,difftot := abs(diffh-diffl)]

stimuli =  stimuli[diffh > diffl][order(-difftot,sumbr)]
stimuli = stimuli[(xh > xl | yh > xl) & (xh > yl | yh > yl)]

#min bad rows by dnbin and difftot
select = function(x,y){
  sel = stimuli[dhbin == x & difftot == y][sumbr == min(sumbr),nr] 
  return(sel)
}

s = stimuli[,.N, by=c("difftot","dhbin")]

vsnr = NULL
for(i in 1:nrow(s)){
  snr = select(s[i,dhbin],s[i,difftot])
  vsnr = c(vsnr,snr)
}

stimuli = stimuli[ nr %in% vsnr]

# remove duplicated by dnbin and difftot
removedup = function(x,y){
  sel = stimuli[dhbin == x & difftot == y][!duplicated(idh,idl),nr]
  return(sel)
}

s = stimuli[,.N, by=c("difftot","dhbin")]

vsnr = NULL
for(i in 1:nrow(s)){
  snr = removedup(s[i,dhbin],s[i,difftot])
  vsnr = c(vsnr,snr)
}

stimuli = stimuli[ nr %in% vsnr]


# random draw by difftot and dhbin
set.seed(100)


drawstimuli = function(x,y){
  sel = sample(stimuli[dhbin == x & difftot == y]$nr,1)
  return(sel)
}

s = stimuli[,.N, by=c("difftot","dhbin")]

vsnr = NULL
for(i in 1:nrow(s)){
  snr = drawstimuli(s[i,dhbin],s[i,difftot])
  vsnr = c(vsnr,snr)
}
  
stimuli = stimuli[ nr %in% vsnr]

fwrite(stimuli, "../stimuli/dfe-stimuli-20-t5.csv")

