# ==========================================================================
# Power Analysis: Shifting Model
# ==========================================================================

# Packages -----------------------------------------------------------------
pacman::p_load(data.table, brms, ggplot2, scales, purrr, extrafont)
library(cognitivemodels)
library(designr)
library(lme4)
library(ggplot2)
library(interactions)
library(simr)
windowsFonts(Arial=windowsFont("Arial"))

# Read data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data = fread("../data/power_data_shift.csv")


# Set Contrasts: Sum coding ------------------------------------------------
# (effect are in contrast to the overall mean)
contrasts(data$advantage) = car::contr.Sum(levels(data$advantage))
contrasts(data$dstate) = car::contr.Sum(levels(data$dstate))
contrasts(data$doutcome) = car::contr.Sum(levels(data$doutcome))

data

# Regression model  ----------------------------------
data[,abs_ad := abs(ad_risky)]
fit_freq1 <- glmer(optBehavior ~ trial + abs_ad  + (1 | pid),  data = data[ad_risky != 0], family = "binomial")
summary(fit_freq1)

interact_plot(fit_freq1, pred = trial, modx = abs_ad) +
  xlim(1,5) +
  ylim(0,1)

# Power analysis based on the effects of the pretest -----------------------
# Simulate new data based on the pretest data to assess the required sample size
# The powerSim function allows to estimate the power to detect a specific effect in the model.
# The model including the effect of interest will be compared with an alternative model that
# does not include the effect of interest.

# Observed Power 
#sim_advantage <- powerSim(fit_freq, nsim=100, test = fcompare(choice~dstate + doutcome))
#sim_dstate <- powerSim(fit_freq, nsim=100, test = fcompare(choice~advantage + doutcome))
#sim_doutcome <- powerSim(fit_freq, nsim=100, test = fcompare(choice~advantage + dstate))

#sim_trial <- powerSim(fit_freq1, nsim=10, test = fcompare(choice~advantage))

# Smallest effect of interest  ----------------------------------------------
model_SESOI <- fit_freq1
fixef(model_SESOI)['dstate[S.negative]'] <- -0.03

# Increase Sample Size ------------------------------------------------------
model_ext_pid <- extend(model_SESOI, along="pid", n=150)


#sim_advantage <- powerSim(model_ext_pid, nsim=50, test = fcompare(choice~advantage))

p_curve_doutcome <- powerCurve(model_ext_pid, test=fcompare(choice~advantage + dstate), along="pid", breaks=c(50,60,70,80))
p_curve_dstate <- powerCurve(model_ext_pid, test=fcompare(choice~advantage + doutcome), along="pid", breaks=c(60,70,80))
p_curve_advantage <- powerCurve(model_ext_pid, test=fcompare(choice~dstate + doutcome), along="pid", breaks=c(60,70,80))



p_curve_dstate <- readRDS("../pwrmodels/p_curve_dstate.rds")
plot(p_curve_dstate)
p_curve_doutcome <- readRDS("../pwrmodels/p_curve_doutcome.rds")
plot(p_curve_doutcome)


# Minimal Effect Size of Interest
dataAGG <- data[,.(meanR = mean(choice)),by=c("doutcome", "dstate", "advantage")]

dataOutcome <- data[,.(meanR = mean(choice)), by=c("doutcome","Version")]
dataAdvantage <- data[,.(meanR = mean(choice)), by=c("advantage","Version")]
dataState <- data[,.(meanR = mean(choice)), by=c("dstate","Version")]


Outcome <- dcast(data = dataOutcome,
                 ... ~ doutcome,
                 value.var = c("meanR"))

State <- dcast(data = dataState,
               ... ~ dstate,
               value.var = c("meanR"))

Advantage <- dcast(data = dataAdvantage,
                   ... ~ advantage,
                   value.var = c("meanR"))


AGG <- merge(Outcome,State, by="Version")
AGG <- merge(AGG, Advantage, by="Version")

AGG[,DiffLossGain := loss - gain, ]
AGG[,DiffRiskySafe := risky - safe,]
AGG[,DiffNegativePositive := negative - positive,]
