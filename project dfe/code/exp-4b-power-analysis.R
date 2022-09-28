#===============================================
# Power Analysis DFE Experiment 4b
#
#===============================================
library(designr)
library(lme4)
library(ggplot2)
library(interactions)

 nsubj <- seq(100,160,20) # Increase sample sizes per sampling condition from 100 to 160 by 20
 nsim <- length(nsubj) # number of simulations per sample size
 sampling <- t1 <- t2 <- t1sampling <- t2sampling <- data.frame()
 warn <- c()
 
 for (i in 1:nsim) {
    
   # create experimental design
   design <-
     fixed.factor("type", levels=c("t1", "t2","t3")) + # Within
     fixed.factor("sampling", levels=c("1", "s2","s3"), is_ordered = T) + #between 
     random.factor("Subj", groups = "sampling",instances=nsubj[i]) #Random
   
   dat <- dplyr::arrange(design.codes(design))
   nsj <- length(unique(dat$Subj))
   (contrasts(dat$sampling) <- c(-1,0,1))
   (contrasts(dat$type) <- c(-1,0,1))
   
   # In numeric
   dat$samplingn <- model.matrix(~sampling,dat)[,2]
   
   # Task repetition
   dat = rbind(dat,dat,dat,dat,dat)
   
   for (j in 1:100) {
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

t1sampling = koeffizienten(t1sampling)

p1 <- ggplot(data=t1sampling)+
  geom_smooth(aes(x=nsj, y=sign))+
  geom_point(   stat="summary", aes(x=nsj, y=sign))+
  geom_errorbar(stat="summary", aes(x=nsj, y=sign))+
  geom_line(    stat="summary", aes(x=nsj, y=sign))+
   scale_x_continuous(breaks = c(unique(t1sampling$nsj)))

