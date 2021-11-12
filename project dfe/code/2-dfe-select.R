# ==========================================================================
# Select
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
d <- d[sumbr < .6]
d[,difh := abs(pxh - pyh)]
d[,difl := abs(pxl - pyl)]
d[,dis := abs(difh-difl)]
#data <- data[xh != xl & xh != yl & yh != xl & yh != yl]


# Difficulty Categories ---------------------------------------------------------------------
d[,difficulty := ifelse(dh >= dl,dh,dl)]
d <- d[difficulty <= .85 & difficulty >= .3]
int <- 0.1
boundaries <- seq(0,0.9, int)
d[, dhbin := cut(difficulty, boundaries, include.lowest = T, labels = F)]
d[, dhbin := cut(difficulty, c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9), include.lowest = T)]

unique(d$id)

tab <- table(d$dhbin,d$id)
tab <- as.data.table(tab)
setnames(tab,"V1", "dhbin")
setnames(tab,"V2", "id")
tab[, vic := ifelse(N > 0,1,0)]
tab[, sumvic := sum(vic), by=c("id")]
tab <- tab[sumvic == max(sumvic)]

d <- d[id %in% tab$id,]
d[,unique(id)]
d[dis == max(dis)]
d[trial == 1]
#stimuli <- d[id == 120][dh == max(dh) | dh == min(dh)]
stimuli <- d[nr == 677 | nr == 680]





