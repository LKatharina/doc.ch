# ==========================================================================
# Power Analysis: Experiment 2a
# ==========================================================================

# Packages -----------------------------------------------------------------
pacman::p_load(data.table, brms)
library(cognitivemodels)
library(lme4)
library(simr)

# Read data ---------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data = fread("../data/power_data_shift.csv")
data = data[ad_risky != 0 & domain == "positive gain"]
data[,pid := as.factor(pid)]
data[,abs_ad := abs(ad_risky)]

# Regression model  --------------------------------------------------------------

fit_freq1 <- glmer(optBehavior ~ trial + abs_ad  + (1 | pid),  data = data, family = "binomial")
summary(fit_freq1)


# Power analysis based on the effects of the master's data -----------------------

# Smallest effect of interest  ---------------------------------------------------
model_SESOI <- fit_freq1
fixef(model_SESOI)['trial'] <- 0.07

# Increase Sample Size -----------------------------------------------------------
model_ext_pid <- extend(model_SESOI, along="pid", n=150)


p_curve_trial <- powerCurve(model_ext_pid, test=fcompare(optBehavior~abs_ad), along="pid", breaks=c(100), nsim =  1000)
plot(p_curve_trial)






