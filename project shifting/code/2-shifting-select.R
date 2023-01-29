# ==========================================================================
# Select Stimuli
# ==========================================================================


# Load Packages-------------------------------------------------------------
pacman::p_load(data.table)
library(cognitivemodels)
library(cognitiveutils)


# Load data --------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


sim <- read.table("../stimuli/shifting_stimuli.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))

sim <- as.data.table(sim)
sim[, costs := abs(dl -dh)]
sim[order(-costs)]
sim[, costs := abs(policyHV -policyLV)]
sim[, sumcosts := cumsum(costs), by = c("trial","nr")]
sim[,sumcosts]
sim[nr == "555"]
# Criteria for stimuli selection ------------------------------------------------
# difficulty
# % bad rows
# Predictions: Difference in Proportion of Risky choices --> DIFF
# Direction of the Predictions

sim[,sumbr := round(sum(badrows)/sum(.N),2), by=c("nr","b")]

utility <- function(x,y){
  diff <- abs(x - y)
  dis <-  as.numeric(( x > 0.5 & y < 0.5 ) | (x < 0.5 & y > 0.5)) # 1 if true , 0 if false
  return( diff * dis)
}


# Utility of paired comparisons
# Berechnet die Utility f?r alle m?glichen Paarvergleiche zwischen Parameterkombinationen
utility_of_paired_comparisons <- function(x,y) {
  xcol <- 1:length(x)
  ycol <- 1:length(y)
  k <- 0
  allComp <- NULL
  for(i in xcol){
    for(j in ycol){
      ut <- utility(unlist(x[,..i]),unlist(y[,..j]))
      k <- k + 1
      allComp <- c(allComp, ut)
    }
  }
  return(unlist(allComp))
}

utility_of_design <- function(x, w, m1, m2){
  colm1 <- grep(m1,names(x)) # Sucht nach einem Muster in einem Vektor, bei uns nach dem Namen rsft
  colm2 <- grep(m2,names(x)) # Sucht nach einem Muster in einem Vektor, bei uns nach dem Namen dcpt
  utvec <- utility_of_paired_comparisons(x = x[,..colm1], y = x[,..colm2]) # .. --> Ausserhalb der data table suchen
  if(any(utvec > 0)){
    u1 <- 1- mean((1 - utvec[utvec > 0])^2) # Mittlere Differenz zwischen den Vorhersagen, welche nicht 0 sind
  } else {
    u1 <- 0
  }
  u2 <- mean(as.numeric(utvec > 0.0001))
  return(w * u1 + (1 - w) * u2)
}

# Utiltiy of design --> Berechnet einen Wert f?r jeden Stimuli (bestehend aus 2 Gambles)
# u1 = Differenzen (von den Paarvergleichen) aufsummieren und den mean berechnen (jedoch nur, wenn die Differenz nicht = 0)
# u2 = Prozentsatz an vorhersagen, wo die Vorhersagen nicht in die gleiche Richtung gehen
utility_of_design <- function(x, m1, m2,f){
  colm1 <- grep(m1,names(x)) # Sucht nach einem Muster in einem Vektor, bei uns nach dem Namen rsft
  colm2 <- grep(m2,names(x)) # Sucht nach einem Muster in einem Vektor, bei uns nach dem Namen dcpt
  utvec <- utility_of_paired_comparisons(x = x[,..colm1], y = x[,..colm2]) # .. --> Ausserhalb der data table suchen
  if(any(utvec > 0)){
    if(f == "mean"){
    u1 <- mean((utvec))
    } else {
    u1 <- sd(utvec)
    }
  } else {
    u1 <- 0
  }
  u2 <- mean(as.numeric(utvec > 0.0001))
  return(u1)
}

# Calculate Utility ---------------------------------------------------------------------------
sim[ , ut := utility_of_design(.SD, m1 = "prhv_rsft", m2 = "prhv_ph", f = "mean"), by= "nr"]
sim <- sim[ ut > 0]


library(ggplot2)
ggplot(sim[trial == 1], aes(ut)) + 
  geom_histogram(binwidth=0.001, colour = "black", fill = "grey") + 
  labs(x = "ut", y = "N") + theme(text = element_text(size=12))


# Difficulty Catgories ---------------------------------------------------------------------
s <- sim[dh <= .8 & dh >= .2 ]
int <- 0.1
boundaries <- seq(0,0.9, int)
s[, dhbin := cut(dh, boundaries, include.lowest = T, labels = F)]
s[, dhbin := cut(dh, c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9), include.lowest = T)]


ggplot(s[trial == 1], aes(x = ut, fill = dhbin)) + 
  geom_histogram(binwidth=0.01,colour = "black", position = 'identity') + 
  labs(x = "ut", y = "N") + theme(text = element_text(size=12))

# Best stimuli of each category -------------------------------------------------------------- 
mut <- s[trial == 1, .(max = max(ut)) ,by="dhbin"]
stimuli <- s[nr %in% s[trial == 1 & ut %in% mut$max, nr]]
s[nr %in% s[trial == 1 & ut %in% mut$max, nr]]


stimuli <- melt(
  data = stimuli,
  measure.vars = c("prhv_rsft","prhv_ph", "prhv_shift"),
  value.name = "prhv",
  variable.name = "model")


stimuli[ , model := factor(model, labels = c("rsft", "ph", "shift"))]
stimuli[ , trial := as.factor(trial)]
