# ==========================================================================
# Auswahl der Stimuli
# Auswahlkriterien:
# Schwierigkeit der Zielerreichung
# Prozentsatz an "schlechten" Vorhersagen (wenn RSFT keine Präferenz für risky oder safe vorhersagt)
# ==========================================================================

# Load Packages-------------------------------------------------------------
pacman::p_load(data.table)
library(cognitivemodels)
library(cognitiveutils)


# Load data --------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read.table("../stimuli/dfe_stimuli.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
d <- as.data.table(d)

# bad rows ----------------------------------------------------------------------------------

d[,badrows := ifelse(hvalue == lvalue,1,0)]
d[,sumbr := round(sum(badrows)/sum(.N),2), by=c("nr","b")]
d <- d[sumbr < .6] # Exclusion if %bad rows > 60%



# Difficulty Categories ---------------------------------------------------------------------
d[,difficulty := ifelse(dh >= dl,dh,dl)]
d <- d[difficulty <= .85 & difficulty >= .3] # Stimuli with difficulties > 0.85 and < 0.3 are excluded

int <- 0.1
boundaries <- seq(0,0.9, int)
d[, dhbin := cut(difficulty, boundaries, include.lowest = T, labels = F)]
d[, dhbin := cut(difficulty, c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9), include.lowest = T)]


stimuli <- d[trial == 1,]


stimuli[,diffh := abs(pxh-pyh)]
stimuli[,diffl := abs(pxl-pyl)]
stimuli[,difftot := abs(diffh-diffl)]
stimuli[order(-difftot,badrows)]

stimuli = stimuli[nr == 1983 | nr == 1987]
