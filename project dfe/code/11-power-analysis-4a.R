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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Regression: Sample Size, Type, x Interaktion --> Powern auf Interaktion, Binomiales Outcome (Optimal Choice)
# 1 stelle Vektor = 1 im Random Effekt, 2 Stelle = 2 im Random Effekt

sample = 10 # Anzahl N pro Sampling-Stufe
design <-
  #fixed.factor("format", levels=c("description", "experience")) + # Within
  fixed.factor("sampling", levels=c("s1", "s2","s3","s4","s5", "s6","s7","s8","s9"), is_ordered = T) + #between 
  random.factor("Subj", groups = "sampling",instances=sample) #Random
# Jeder Proband in jeder Sampling Stufe sieht 1x ein Stimuli von jedem Typ

# Contraste setzen
dat <- dplyr::arrange(design.codes(design))
(contrasts(dat$sampling) <- c(-4/4,-3/4,-2/4,-1/4,0,1/4,2/4,3/4,4/4))

# In numeric
dat$samplingn <- model.matrix(~sampling,dat)[,2]
dat = dat %>% 
  mutate(samplingn = recode(samplingn, `-1` = 3, `-0.75` = 5, `-0.5` = 7,`-0.25` = 9,`0` = 11,
                            `0.25` = 13,`0.5` = 15, `0.75` = 17,`1` = 19))

# Wiederholungen der Aufgaben
dat = rbind(dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat)

# Effects
fix      <- c(0.1, # Intercept
              0.05) # Sampling
sd_Subj  <- c(0.34) # specify subject random effects (standard deviation) 
dat$ysim <- simLMM(form=~ samplingn + (1 | Subj), data=dat,
                   Fixef=fix, VC_sd=list(sd_Subj), CP=0.3,
                   empirical=FALSE, family="binomial")


fit = glmer(ysim ~ samplingn + (1 | Subj), data=dat, family = binomial, control=glmerControl(optimizer="bobyqa"))
ggpredict(fit, "samplingn [all]") %>% plot() +
  ylim(0,1)


summary(fit)
# options(scipen = 999)
# ss <- getME(fit,c("theta","fixef"))
# m2 <- update(fit,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
# 
# m3 <- update(fit,start=ss,control=glmerControl(optimizer="bobyqa",
#                                                  optCtrl=list(maxfun=2e5)))


# Simulations----------------------------------------------------------------
nsubj <- seq(10,10,1) # we vary the number of subjects from 10 to 100 in steps of 1
nsim <- length(nsubj) # number of simulations
sampling <- t1 <- t2 <- t1sampling <- t2sampling <- data.frame()
warn <- c()

for (i in 1:nsim) { # i <- 1
  
  # create experimental design
  design <-
    fixed.factor("sampling", levels=c("s1", "s2","s3","s4","s5", "s6","s7","s8","s9"), is_ordered = T) + #between 
    random.factor("Subj", groups = "sampling",instances=nsubj[i]) 
  
  # Contraste setzen
  dat <- dplyr::arrange(design.codes(design))
  (contrasts(dat$sampling) <- c(-4/4,-3/4,-2/4,-1/4,0,1/4,2/4,3/4,4/4))
  nsj <- length(unique(dat$Subj))
  
  # In numeric
  dat$samplingn <- model.matrix(~sampling,dat)[,2]
  dat = dat %>% 
    mutate(samplingn = recode(samplingn, `-1` = 3, `-0.75` = 5, `-0.5` = 7,`-0.25` = 9,`0` = 11,
                              `0.25` = 13,`0.5` = 15, `0.75` = 17,`1` = 19))
  
  # Wiederholungen der Aufgaben
  dat =  rbind(dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat)
  
  for (j in 1:1000) { # j <- 1
    fix      <- c(0.1, # Intercept
                  0.04) # Sampling
    sd_Subj  <- c(0.34) # specify subject random effects (standard deviation) 
    dat$ysim <- simLMM(form=~ samplingn + (1 | Subj), data=dat,
                       Fixef=fix, VC_sd=list(sd_Subj), CP=0.3,
                       empirical=FALSE, family="binomial")
    
    ww <- ""
    suppressMessages(suppressWarnings(
      fit <- withCallingHandlers({
        fit = glmer(ysim ~ samplingn + (1 | Subj),
                    data=dat,
                    family = binomial,
                    control=glmerControl(optimizer="bobyqa"))
      },
      warning = function(w) { ww <<- w$message }
      )
    ))
    
    samplingcof = coef(summary(fit))[2,]
    sampling <- rbind(sampling,c(nsj,samplingcof,isSingular(fit)))
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

#sampling = koeffizienten(sampling)
sampling = koeffizienten(sampling)
saveRDS(sampling, "../data/powerdata_4a_sampling.rds")

p1 <- ggplot(data=sampling)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsj, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsj, y=sign))+
  geom_line(    stat="summary", aes(x=nsj, y=sign))


# Stimulations 2 ---------------------------------------------------------------------------------

sample = 90 # Anzahl N pro Sampling-Stufe
design <-
  #fixed.factor("format", levels=c("description", "experience")) + # Within
  fixed.factor("format", levels=c("experience","description")) + #between 
  random.factor("Subj", groups = c("format"),instances=sample) #Random
# Jeder Proband in jeder Sampling Stufe sieht 1x ein Stimuli von jedem Typ

exp(0.1+0.2*1)/(1 + exp(0.1+0.2*1))

# Contraste setzen
dat <- dplyr::arrange(design.codes(design))
(contrasts(dat$format) <- c(-1,1))

# Wiederholungen der Aufgaben

dat = rbind(dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat)

#0.1
# Effects
fix      <- c(0.1, # Intercept
              0.1) # Format
sd_Subj  <- c(0.34) # specify subject random effects (standard deviation) 
dat$ysim <- simLMM(form=~ format + (1 | Subj), data=dat,
                   Fixef=fix, VC_sd=list(sd_Subj), CP=0.3,
                   empirical=FALSE, family="binomial")
?simLMM

fit = glm(ysim ~ format, data=dat, family = binomial)
ggpredict(fit, "format [all]") %>% plot() +
  ylim(0,1)


summary(fit)


exp(0.1+0.3*-1)/(1 + exp(0.1+0.3*-1))

exp(0.1+0.3*1)/(1 + exp(0.1+0.3*1))

nsubj <- seq(128,128,1) # we vary the number of subjects from 10 to 100 in steps of 1
nsim <- length(nsubj) # number of simulations
format <- t1 <- t2 <- t1sampling <- t2sampling <- data.frame()
warn <- c()

for (i in 1:nsim) { # i <- 1
  
  # create experimental design
  design <-
    #fixed.factor("format", levels=c("description", "experience")) + # Within
    fixed.factor("format", levels=c("experience","description")) + #between 
    random.factor("Subj", groups = c("format"),instances=nsubj[i]) #Random
  # Jeder Proband in jeder Sampling Stufe sieht 1x ein Stimuli von jedem Typ
  
  # Contraste setzen
  dat <- dplyr::arrange(design.codes(design))
  (contrasts(dat$format) <- c(-1,1))
  nsj <- length(unique(dat$Subj))
  
  
  # Wiederholungen der Aufgaben
  dat = rbind(dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat,dat)
  
  for (j in 1:1000) { # j <- 1
    fix      <- c(0.1, # Intercept
                  0.2) # Sampling
    sd_Subj  <- c(0.34) # specify subject random effects (standard deviation) 
    dat$ysim <- simLMM(form=~ format + (1 | Subj), data=dat,
                       Fixef=fix, VC_sd=list(sd_Subj), CP=0,
                       empirical=FALSE, family="binomial")
    
    ww <- ""
    suppressMessages(suppressWarnings(
      fit <- withCallingHandlers({
        #fit = glm(ysim ~ format, data=dat, family = binomial)
          fit = glmer(ysim ~ format + (1 | Subj),
                     data=dat,
                     family = binomial,
                     control=glmerControl(optimizer="bobyqa"))
      },
      warning = function(w) { ww <<- w$message }
      )
    ))
    
    formatcof = coef(summary(fit))[2,]
    #format <- rbind(format,c(nsj,formatcof))
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

#sampling = koeffizienten(sampling)
format = koeffizienten(format)

p1 <- ggplot(data=format)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsj, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsj, y=sign))+
  geom_line(    stat="summary", aes(x=nsj, y=sign))

data = as.data.table(dat)
data = data[,.(ysim = mean(ysim), sd_ysim = sd(ysim)), by=c("Subj","format")]

data[,.(mean(ysim),sd(ysim)), by = "format"]


# ANSCHAUEN =================================================================

sample = 1 # Anzahl N pro Sampling-Stufe
design <-
  #fixed.factor("format", levels=c("description", "experience")) + # Within
  fixed.factor("format", levels=c("description", "experience")) + #between 
  fixed.factor("sampling", levels=c("s1", "s2","s3","s4","s5", "s6","s7","s8","s9"), is_ordered = T) +
  random.factor("Subj", groups = c("format","sampling"),instances=sample) #Random
# Jeder Proband in jeder Sampling Stufe sieht 1x ein Stimuli von jedem Typ

exp(0.1+0.2*1)/(1 + exp(0.1+0.2*1))

# Contraste setzen
dat <- dplyr::arrange(design.codes(design))
(contrasts(dat$format) <- c(-1,1))
(contrasts(dat$sampling) <- c(-4/4,-3/4,-2/4,-1/4,0,1/4,2/4,3/4,4/4))

dat$samplingn <- model.matrix(~sampling,dat)[,2]
dat = dat %>% 
  mutate(samplingn = recode(samplingn, `-1` = 3, `-0.75` = 5, `-0.5` = 7,`-0.25` = 9,`0` = 11,
                            `0.25` = 13,`0.5` = 15, `0.75` = 17,`1` = 19))

dat$formatid = ifelse(dat$format == "experience", 1, 0)
# Wiederholungen der Aufgaben

dat = rbind(dat,dat,dat,dat,dat,dat)

#0.1
# Effects
fix      <- c(0.1, # Intercept
              0.1,
              0.2,
              0.4) # Format
sd_Subj  <- c(0.34) # specify subject random effects (standard deviation) 
sd_format <- c(0.5)
dat$ysim <- simLMM(form=~ format + samplingn + format:samplingn + (1 | Subj) + (1 | format/formatid), data=dat,
                   Fixef=fix, VC_sd=list(sd_Subj,sd_format), CP=0.3,
                   empirical=FALSE, family="binomial")


fit = glmer(ysim ~format + samplingn + format:samplingn + (1 | Subj) + (1 | format/formatid),
            data=dat, family = binomial, control=glmerControl(optimizer="bobyqa"))
ggpredict(fit, "format [all]") %>% plot() +
  ylim(0,1)


summary(fit)


nsubj <- seq(13,20,1) # we vary the number of subjects from 10 to 100 in steps of 1
nsim <- length(nsubj) # number of simulations
sampling <- t1 <- t2 <- t1sampling <- t2sampling <- data.frame()
warn <- c()

for (i in 1:nsim) { # i <- 1
  
  # create experimental design
  design <-
    fixed.factor("sampling", levels=c("s1", "s2","s3","s4","s5", "s6","s7","s8","s9"), is_ordered = T) + #between 
    random.factor("Subj", groups = "sampling",instances=nsubj[i]) 
  
  # Contraste setzen
  dat <- dplyr::arrange(design.codes(design))
  (contrasts(dat$sampling) <- c(-4/4,-3/4,-2/4,-1/4,0,1/4,2/4,3/4,4/4))
  nsj <- length(unique(dat$Subj))
  
  # In numeric
  dat$samplingn <- model.matrix(~sampling,dat)[,2]
  dat = dat %>% 
    mutate(samplingn = recode(samplingn, `-1` = 3, `-0.75` = 5, `-0.5` = 7,`-0.25` = 9,`0` = 11,
                              `0.25` = 13,`0.5` = 15, `0.75` = 17,`1` = 19))
  
  # Wiederholungen der Aufgaben
  dat = rbind(dat,dat,dat,dat,dat)
  
  for (j in 1:100) { # j <- 1
    fix      <- c(0.1, # Intercept
                  0.05) # Sampling
    sd_Subj  <- c(0.34) # specify subject random effects (standard deviation) 
    dat$ysim <- simLMM(form=~ samplingn + (1 | Subj), data=dat,
                       Fixef=fix, VC_sd=list(sd_Subj), CP=0.3,
                       empirical=FALSE, family="binomial")
    
    ww <- ""
    suppressMessages(suppressWarnings(
      fit <- withCallingHandlers({
        fit = glmer(ysim ~ samplingn + (1 | Subj),
                    data=dat,
                    family = binomial,
                    control=glmerControl(optimizer="bobyqa"))
      },
      warning = function(w) { ww <<- w$message }
      )
    ))
    
    samplingcof = coef(summary(fit))[2,]
    sampling <- rbind(sampling,c(nsj,samplingcof,isSingular(fit)))
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

#sampling = koeffizienten(sampling)
sampling = koeffizienten(sampling)

p1 <- ggplot(data=sampling)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsj, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsj, y=sign))+
  geom_line(    stat="summary", aes(x=nsj, y=sign))

