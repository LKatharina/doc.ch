#===========================================================
# Power Analysis Shifting Experiment 2b
#
#==========================================================

# Packages ------------------------------------------------
library(designr)
library(lme4)
library(ggplot2)
library(interactions)
library(simglm)
library(tidyverse)
library(ggeffects)

# Simulations -----------------------------------------------
nsubj <- seq(100,100,1) # Sample Size per complexity Condition
nsim <- length(nsubj) # number of simulations per sample size
complexity <- data.frame()
warn <- c()

for (i in 1:nsim) { 
  
  # create experimental design
  design <-
    fixed.factor("difficulty", levels=c("easy", "hard")) + #within
    fixed.factor("complexity", levels=c("t5", "t7", "t10")) + #between 
    random.factor("Subj", groups = c("complexity"),instances=nsubj[i]) #Random
  
  # set contrasts
  dat <- dplyr::arrange(design.codes(design))
  (contrasts(dat$complexity) <- c(-1,0,1))
  (contrasts(dat$difficulty) <- c(-1,1))
  nsj <- length(unique(dat$Subj))
  
  
  # Task repetitions
  dat = rbind(dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,
              dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat)
  
  
  for (j in 1:1000) {
    fix      <- c(0.4, # Intercept
                  0.2, # Complexity condition 7
                  0.4, # Complexity condition 10
                  0.1) # Difficulty
    sd_Subj  <- c(0.34) # specify subject random effects (standard deviation) 
    dat$ysim <- simLMM(form=~ complexity + difficulty + (1|Subj), data=dat,
                       Fixef=fix, VC_sd=list(sd_Subj), CP=0.3,
                       empirical=FALSE, family="binomial",verbose = T)
    ww <- ""
    suppressMessages(suppressWarnings(
      fit <- withCallingHandlers({
        fit = glmer(ysim ~complexity + difficulty + (1|Subj),
                    data=dat, family = binomial, control=glmerControl(optimizer="bobyqa"))
        
      },
      warning = function(w) { ww <<- w$message }
      )
    ))
    
    complexity5cof = coef(summary(fit))[2,]
    complexity <- rbind(complexity,c(nsj,complexity5cof,isSingular(fit)))
    warn[i] <- ww
    
  }
}

# Results for LMMs
koeffizienten = function(COF){
  names(COF) <- c("nsj","Estimate","SE","z","p","singular")
  COF$warning <- warn
  COF$noWarning <- warn==""
  COF$sign   <- as.numeric(COF$p < .05 & COF$Estimate>0) # determine significant results
  return(COF)
}


complexity = koeffizienten(complexity)

# plot
p1 <- ggplot(data=complexity)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsj, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsj, y=sign))+
  geom_line(    stat="summary", aes(x=nsj, y=sign))
