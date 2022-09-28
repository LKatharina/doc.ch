#===============================================
# Power Analysis Near-Miss Experiment 2b
#
#===============================================

library(designr)
library(lme4)
library(ggplot2)
library(tidyverse)
library(ggeffects)


# Power Analysis --------------------------------------------------------------------------
nsubj <- seq(100,100,1) # sample size
nsim <- length(nsubj) # Number of simulations per condition
nearmiss <- data.frame()
warn <- c()

for (i in 1:nsim) {
  
  # create experimental design
  design <-
    fixed.factor("nearmiss", levels=c("low", "medium", "strong"),is_ordered = T) + #Within
    random.factor("Subj",instances=nsubj[i]) #Random
  
  # Contrasts
  dat <- dplyr::arrange(design.codes(design))
  (contrasts(dat$nearmiss) <- c(-1,0,1))
  nsj <- length(unique(dat$Subj))
  
  # continuous
  dat$nearmissn <- model.matrix(~nearmiss,dat)[,2]
  
  # Number of choice tasks
  dat = rbind(dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,
              dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,
              dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat)
  
  for (j in 1:1000) {
    fix      <- c(0.1, # Intercept
                  0.06) # Near miss potential
    sd_Subj  <- c(0.34) # specify subject random effects (standard deviation) 
    dat$ysim <- simLMM(form=~ nearmissn + (1|Subj), data=dat,
                       Fixef=fix, VC_sd=list(sd_Subj), CP=0.3,
                       empirical=FALSE, family="binomial",verbose = T)
    ww <- ""
    suppressMessages(suppressWarnings(
      fit <- withCallingHandlers({
        fit = glmer(ysim ~nearmissn + (1|Subj),
                    data=dat, family = binomial, control=glmerControl(optimizer="bobyqa"))
        
      },
      warning = function(w) { ww <<- w$message }
      )
    ))
    
    nearmisscof = coef(summary(fit))[2,]
    nearmiss <- rbind(nearmiss,c(nsj,nearmisscof,isSingular(fit)))
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


nearmiss = koeffizienten(nearmiss)

# plot
p1 <- ggplot(data=nearmiss)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsj, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsj, y=sign))+
  geom_line(    stat="summary", aes(x=nsj, y=sign))

