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
data = data[ad_risky != 0 & domain == "positive gain"]
data[,pid := as.factor(pid)]
data[,abs_ad := abs(ad_risky)]

# Regression model  ---------------------------------------

fit_freq1 <- glmer(optBehavior ~ trial + abs_ad  + (1 | pid),  data = data, family = "binomial")
summary(fit_freq1)

interact_plot(fit_freq1, pred = trial, modx = abs_ad) +
  xlim(1,5) +
  ylim(0,1)


# Power analysis based on the effects of the pretest -----------------------
# Simulate new data based on the pretest data to assess the required sample size
# The powerSim function allows to estimate the power to detect a specific effect in the model.
# The model including the effect of interest will be compared with an alternative model that
# does not include the effect of interest.


# Smallest effect of interest  ----------------------------------------------
model_SESOI <- fit_freq1
fixef(model_SESOI)['trial'] <- 0.07

# Increase Sample Size ------------------------------------------------------
model_ext_pid <- extend(model_SESOI, along="pid", n=150)


#sim_advantage <- powerSim(model_ext_pid, nsim=50, test = fcompare(choice~advantage))
p_curve_trial <- powerCurve(model_ext_pid, test=fcompare(optBehavior~abs_ad), along="pid", breaks=c(100), nsim =  1000)
plot(p_curve_trial)

saveRDS("../data/p_curve_trial.rds")


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



