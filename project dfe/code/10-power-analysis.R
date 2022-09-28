#===============================================
# Power Analysis DFE Experiment 4b
#
#===============================================
library(designr)
library(lme4)
library(ggplot2)
library(interactions)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Regression: Sample Size, Type, x Interaktion --> Powern auf Interaktion, Binomiales Outcome (Optimal Choice)
# 1 stelle Vektor = 1 im Random Effekt, 2 Stelle = 2 im Random Effekt

sample = 80 # Anzahl N pro Sampling-Stufe
design <-
  fixed.factor("type", levels=c("t1", "t2","t3")) + # Within
  fixed.factor("sampling", levels=c("s1", "s2","s3"), is_ordered = T) + #between 
  random.factor("Subj", groups = "sampling",instances=sample) #Random
# Jeder Proband in jeder Sampling Stufe sieht 1x ein Stimuli von jedem Typ

# Contraste setzen
dat <- dplyr::arrange(design.codes(design))
(contrasts(dat$sampling) <- c(-1,0,1))
(contrasts(dat$type) <- c(-1,0,1))

# In numeric
dat$samplingn <- model.matrix(~sampling,dat)[,2]
# In 3,5,15 umwandeln
# dat$samplingn = ifelse(dat$samplingn == -1,3,dat$samplingn)
# dat$samplingn = ifelse(dat$samplingn == 0,5,dat$samplingn)
# dat$samplingn = ifelse(dat$samplingn == 1,15,dat$samplingn)

# Wiederholungen der Aufgaben
dat = rbind(dat,dat,dat,dat,dat,dat)
# c(-0.3,0.06,0.2,0.9,-0.01,-0.055)
#  c(0.2,0.5,2,4,1,1)
#fix      <- c(0.1,0.6,0.5,0.9,-0.2,-0.5)
fix      <- c(0.8, # Intercept
              2.1, # Sampling
              1, # Type2
              2, # Type3
              -0.4, # sampling:type2
              -0.6) # sampling:type3
#specify fixed-effects: Intercept, samplingn, typet2, typet3, sampling:typet2, samplingtypet3
sd_Subj  <- c(0.35,0.34,0.3) # specify subject random effects (standard deviation) 
dat$ysim <- simLMM(form=~ samplingn + type + samplingn:type + (1 + type | Subj), data=dat,
                   Fixef=fix, VC_sd=list(sd_Subj), CP=0.3,
                   empirical=FALSE, family="binomial")

# CP = Korrelation der Daten innerhalb eines Subjects (Rep)
# lmer
#PLot
#Simulation --> Ohne Anova

fit = glmer(ysim ~ samplingn + type + samplingn:type + (1 + type || Subj), data=dat, family = binomial, control=glmerControl(optimizer="bobyqa"))
interact_plot(fit, pred = samplingn, modx = type) +
  xlim(-1,1) +
  ylim(0,1)
summary(fit)
# options(scipen = 999)
# ss <- getME(fit,c("theta","fixef"))
# m2 <- update(fit,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
# 
# m3 <- update(fit,start=ss,control=glmerControl(optimizer="bobyqa",
#                                                  optCtrl=list(maxfun=2e5)))


 nsubj <- seq(100,160,20) # we vary the number of subjects from 10 to 100 in steps of 1
 nsim <- length(nsubj) # number of simulations
 sampling <- t1 <- t2 <- t1sampling <- t2sampling <- data.frame()
 warn <- c()
 
 for (i in 1:nsim) { # i <- 1
   #print(paste0(i,"/",nsim))
   # create experimental design
   design <-
     fixed.factor("type", levels=c("t1", "t2","t3")) + # Within
     fixed.factor("sampling", levels=c("1", "s2","s3"), is_ordered = T) + #between 
     random.factor("Subj", groups = "sampling",instances=nsubj[i]) #Random
   # Jeder Proband in jeder Sampling Stufe sieht 1x ein Stimuli von jedem Typ
   
   # Contraste setzen
   dat <- dplyr::arrange(design.codes(design))
   nsj <- length(unique(dat$Subj))
   (contrasts(dat$sampling) <- c(-1,0,1))
   (contrasts(dat$type) <- c(-1,0,1))
   
   # In numeric
   dat$samplingn <- model.matrix(~sampling,dat)[,2]
   # In 3,5,15 umwandeln
   # dat$samplingn = ifelse(dat$samplingn == -1,3,dat$samplingn)
   # dat$samplingn = ifelse(dat$samplingn == 0,5,dat$samplingn)
   # dat$samplingn = ifelse(dat$samplingn == 1,15,dat$samplingn)
   
   # Wiederholungen der Aufgaben
   dat = rbind(dat,dat,dat,dat,dat)
   
   for (j in 1:100) { # j <- 1
     # simulate data
     #  c(0.2,0.5,2,4,1,1) 
      fix      <- c(0.8, # Intercept
                    2.1, # Sampling
                    1, # Type2
                    2, # Type3
                    -0.4, # sampling:type2
                    -0.6) # sampling:type3
     sd_Subj  <- c(0.37,0.34,0.3) # specify subject random effects (standard deviation) 
     dat$ysim <- simLMM(form=~ samplingn + type + samplingn:type + (1 + type | Subj), data=dat,
                        Fixef=fix, VC_sd=list(sd_Subj), CP=0.3,
                        empirical=FALSE, family="binomial")
     # fit = glmer(ysim ~ samplingn + type + samplingn:type + (1 + type || Subj), data=dat, family = binomial)
     # 
     # samplingcof = coef(summary(fit))[2,]
     # t1cof = coef(summary(fit))[3,]
     # t2cof = coef(summary(fit))[4,]
     # int1cof = coef(summary(fit))[5,]
     # int2cof = coef(summary(fit))[6,]
     # 
     # sampling <- rbind(sampling,c(nsj,samplingcof))
     # t1 <- rbind(t1,c(nsj,t1cof))
     # t2 <- rbind(t2,c(nsj, t2cof))
     # t1sampling <- rbind(t1sampling,c(nsj,int1cof))
     # t2sampling <- rbind(t2sampling,c(nsj, int2cof))
     # 
     ww <- ""
     suppressMessages(suppressWarnings(
              fit <- withCallingHandlers({
                glmer(ysim ~ samplingn + type + samplingn:type + (1 + type || Subj), data=dat, family = binomial,
                      control=glmerControl(optimizer="bobyqa"))
                },
                warning = function(w) { ww <<- w$message }
                )
            ))
     
     samplingcof = coef(summary(fit))[2,]
     t1cof = coef(summary(fit))[3,]
     t2cof = coef(summary(fit))[4,]
     int1cof = coef(summary(fit))[5,]
     int2cof = coef(summary(fit))[6,]
     
     sampling <- rbind(sampling,c(nsj,samplingcof,isSingular(fit)))
     t1 <- rbind(t1,c(nsj,t1cof,isSingular(fit)))
     t2 <- rbind(t2,c(nsj, t2cof,isSingular(fit)))
     t1sampling <- rbind(t1sampling,c(nsj,int1cof,isSingular(fit)))
     t2sampling <- rbind(t2sampling,c(nsj, int2cof,isSingular(fit)))
     warn[i] <- ww

   }
 }
 # Results for LMMs
   
koeffizienten = function(COF){
  names(COF) <- c("nsj","Estimate","SE","z","p","singular")
  COF$warning <- warn
  COF$noWarning <- warn==""
  COF$sign   <- as.numeric(COF$p < .05 & COF$Estimate<0) # determine significant results
  COF$nsjF   <- gtools::quantcut(COF$nsj, q=seq(0,1,length=3))
  COF$nsjFL  <- plyr::ddply(COF,"nsjF",transform,nsjFL=mean(nsj))$nsjFL
  return(COF)
}

#sampling = koeffizienten(sampling)
t1sampling = koeffizienten(t1sampling)
t2sampling = koeffizienten(t2sampling)

saveRDS(t1sampling, "../data/powerdata_I1.rds")
saveRDS(t2sampling, "../data/powerdata_I2.rds")
t1sampling = readRDS( "../data/powerdata_I1.rds")
t2sampling = readRDS( "../data/powerdata_I2.rds")

p1 <- ggplot(data=t1sampling)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsj, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsj, y=sign))+
  geom_line(    stat="summary", aes(x=nsj, y=sign))+
   scale_x_continuous(breaks = c(unique(t1sampling$nsj)))

p2 <- ggplot(data=t2sampling)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsjFL, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsjFL, y=sign))+
  geom_line(    stat="summary", aes(x=nsjFL, y=sign))

# determine number of subjects needed for a power of 60%
m0 <- loess(sign ~ nsj, data=t1sampling)
t1sampling$pred <- predict(m0)
idx <- t1sampling$pred>0.8
min(t1sampling$nsj[idx])


m0 <- loess(sign ~ nsj, data=t2sampling)
t1sampling$pred <- predict(m0)
idx <- t2sampling$pred>0.8
min(t2sampling$nsj[idx])