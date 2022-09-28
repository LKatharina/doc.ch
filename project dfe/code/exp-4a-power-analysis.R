#===============================================
# Power Analysis DFE Experiment 4b
#
#===============================================
library(designr)
library(lme4)
library(ggplot2)
library(interactions)
library(ggeffects)
library(data.table)
library(tidyverse)


# Simulations----------------------------------------------------------------
nsubj <- seq(128,128,1) # number of participants in each format condition
nsim <- length(nsubj) # number of simulations
format <- data.frame()
warn <- c()

for (i in 1:nsim) { # i <- 1
  
  # create experimental design
  design <-
    fixed.factor("format", levels=c("experience","description")) + #between 
    random.factor("Subj", groups = c("format"),instances=nsubj[i]) #Random
  
  # contrasts
  dat <- dplyr::arrange(design.codes(design))
  (contrasts(dat$format) <- c(-1,1))
  nsj <- length(unique(dat$Subj))
  
  # Task repetition
  dat = rbind(dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat)
  
  for (j in 1:1000) {
    fix      <- c(0.1, # Intercept
                  0.2) # Format condition
    sd_Subj  <- c(0.34) # specify subject random effects (standard deviation) 
    dat$ysim <- simLMM(form=~ format + (1 | Subj), data=dat,
                       Fixef=fix, VC_sd=list(sd_Subj), CP=0,
                       empirical=FALSE, family="binomial")
    
    ww <- ""
    suppressMessages(suppressWarnings(
      fit <- withCallingHandlers({
          fit = glmer(ysim ~ format + (1 | Subj),
                     data=dat,
                     family = binomial,
                     control=glmerControl(optimizer="bobyqa"))
      },
      warning = function(w) { ww <<- w$message }
      )
    ))
    
    formatcof = coef(summary(fit))[2,]
    format <- rbind(format,c(nsj,formatcof,isSingular(fit)))
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

format = koeffizienten(format)

# Plot curve
p1 <- ggplot(data=format)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsj, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsj, y=sign))+
  geom_line(    stat="summary", aes(x=nsj, y=sign))


