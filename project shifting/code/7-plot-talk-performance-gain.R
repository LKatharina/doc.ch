# ==============================================================================
# Plot Performance Gain
# data not shared (data from Marbacher et al. (2021)
# ==============================================================================

# Packages ---------------------------------------------------------------------
pacman::p_load(data.table, ggplot2)
library(cognitivemodels)


windowsFonts(Arial=windowsFont("Arial"))

# Read Data --------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("2-shifting-select.R")
source("3-shifting-plot.R")
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")

data = fread("../data/rsft-gain-loss-processed.csv")




tau = 0.21
x = "deterministic"

data[ad_risky != 0, .(mean = mean(optBehavior)), by = trial][order(trial)]
data[, .(mean = mean(optBehavior)), by = trial][order(trial)]


# energy budget rule
energyBudget = function(data, TimeHorizon, tau){
  data$EVneeded = data[, budget - state / (TimeHorizon+1 - trial)]
  data[, ':=' ("energyLV" = EVneeded - (xl*pxl + yl*pyl), "energyHV" = EVneeded - (xh*pxh + yh*pyh))]
  sim_energy = as.data.table(cr_softmax(x = data[,.(energyHV,energyLV)],tau))
  
  energy_sim <- data.table(
    energyHV = data$energyHV,
    energyLV = data$energyLV,
    prhv_ph = sim_energy$energyHV)
  
}


sim_energy = energyBudget(data,5,tau)



# Probability heuristic
Probheuristic <- function(data,tau){
  data$high = data[,max(xh,yh), by = 1:nrow(data)]$V1
  data$low = data[,max(xl,yl), by = 1:nrow(data)]$V1
  data$probh <- ifelse(data$xh == data$high, data$pxh, data$pyh)
  data$probl <- ifelse(data$xl == data$low, data$pxl, data$pyl)
  
  sim_heuristik = as.data.table(cr_softmax(x = data[,.(probh,probl)],tau))
  
  ph_sim <- data.table(
    probh_ph = data$probh,
    probl_ph = data$probl,
    prhv_ph = sim_heuristik$probh)
  
  return(ph_sim)
}

ph_data <- Probheuristic(data,tau)

data = cbind(data,ph_data)
stimuli = data[trial == 1]
stimuli = stimuli[duplicated(nr) == F]

# Optimal Model
rsft = lapply(1:nrow(stimuli), function(i){
  
  d = stimuli[i,]
  
  m = rsftModel(c(d$xh, d$yh), c(d$xl, d$yl),
                c(d$pxh, d$pyh),c(d$pxl, d$pyl),
                d$budget,
                timeHorizon = 5,
                d$start)
  
  choiceprob = as.data.table(cr_softmax(x = m@compact[, .(policyHV, policyLV)], tau))
  choiceprob = cbind(m@compact[, .(trial, state)], choiceprob)
  
  return(cbind(
    m@compact,
    prhv_rsft = choiceprob$policyHV))
}
)

rsft = rbindlist(rsft)
rsft[, nr := cumsum(trial == 1)] 
data = merge(data,rsft, by = c("trial","state","nr"))

rsft_model <- hm1988(
  ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
  trials = ~trial,        # NEW: ".ALL" will predict for *all possible* trials
  data = data,            # our data (as before)
  budget = ~budget,       # name of our budget column in our data
  initstate = ~start,     # name of our starting-state column in our data
  ntrials = 5,            # we always 5 trials therefore I hard-code this
  states = ~state,        # NEW: ".ALL" will predict for *all possible* states
  choicerule = "softmax",
  fix = list(tau = 0.3))


# Shifting
if(x == "deterministic"){
  data$opt_rsft = 1
  data$opt_ph = ifelse((data$ad_risky > 0 & data$prhv_ph > 0.5) | (data$ad_risky < 0 & data$prhv_ph < 0.5) | (data$ad_risky == 0.5),1,0)
  
  shifting <- shift_d( ~ opt_ph + opt_rsft,
                       time = ~trial,
                       data = data,
                       fix = list(c = 3.5))
  
  opt_shift <- predict(shifting)
  data <- cbind(data, opt_shift = round(opt_shift,0))
  
} else {
  shifting <- shift_d( ~ prhv_ph + prhv_rsft,
                       time = ~trial,
                       data = data,
                       fix = list(c = 3.5))
  prhv_shift <- predict(shifting)
  data <- cbind(data, prhv_shift)
  
  data[, ':=' (opt_rsft = ifelse(ad_risky > 0,prhv_rsft,(1-prhv_rsft)),
               opt_ph = ifelse(ad_risky > 0,prhv_ph,(1-prhv_ph)),
               opt_shift = ifelse(ad_risky > 0,prhv_shift,(1-prhv_shift)))]
  
  data$opt_rsft = ifelse(data$ad_risky == 0,1,data$opt_rsft)
  data$opt_ph = ifelse(data$ad_risky == 0,1,data$opt_ph)
  data$opt_shift = ifelse(data$ad_risky == 0,1,data$opt_shift)
}

data[,opt_det_rsft := 1]
data[,prhv_det_rsft := fcase(prhv_rsft > 0.5, 1,
                             prhv_rsft < 0.5, 0,
                             prhv_rsft == 0.5, 0.5)]

ldata_opt = melt(data,
            measure.vars = patterns("^opt_"),
            value.name = c("proptimal"),
            variable.name = "model")


ldata_prhv = melt(data,
             measure.vars = patterns("^prhv_"),
             value.name = c("prhv"),
             variable.name = "model")


predictions = ldata_opt[, .(mean_pred = mean(proptimal), mean_behav = mean(optBehavior), sd_behavior = sd(optBehavior)/sqrt(.N)), by = c("trial","model")]

predictions[, trial := as.factor(trial)]

ldata_prhv[,mean(prhv), by=c("model", "difficulty")]


# Plots ------------------------------------------------------------------------
# Plot performance gain
ggplot(predictions[model == "opt_shift" | model == "opt_det_rsft"], aes(x = trial, y=mean_pred*100, color = model))+
  #geom_hline(yintercept = 50,  linetype = 2, size = 0.2, colour = "grey45")+
  geom_bar(data = predictions[model == "opt_shift"],
           mapping = aes(x=trial, y = mean_behav*100),
           fill = "#FFFFFF",
           stat = "identity",
           inherit.aes = F,
           colour = "black",
           width = 0.5,
           position = position_dodge()) +
  geom_errorbar(data = predictions[model == "opt_shift"],
                mapping = aes(x=trial, ymin = mean_behav*100, ymax= mean_behav*100 +  sd_behavior*100),
                width=.25, color = "black",
                position=position_dodge(.5),
                inherit.aes = F) +
  theme_classic(base_family = "Arial") +
  geom_point(data = predictions[model == "opt_shift"| model == "opt_det_rsft"],
             aes(x = trial, y = mean_pred*100, color = model, fill = model, shape = model),
             size = 3, stroke = 2,  inherit.aes = F, alpha = 0)  +
  geom_line(data = predictions[model == "opt_shift" | model == "opt_det_rsft"],
            mapping = aes(x = trial, y = mean_pred*100, group = model, color = model),
            linetype = 2, inherit.aes = F, size = 0.8, alpha = 0, show.legend = F) +
  scale_y_continuous(breaks = c(0,100), limits = c(0,110), expand = c(0,0))+
  labs(x = 'Entscheidungen (1-5)', y = 'Performanz (%)')+
  scale_fill_manual(values = c(opt_det_rsft = "black",opt_shift = "blue"), name = "", labels = c(opt_det_rsft = "Optimal Strategy",opt_shift ="Shifting Model")) +
  scale_color_manual(values = c(opt_det_rsft = "black",opt_shift = "blue"), name = "", labels = c(opt_det_rsft = "Optimal Strategy",opt_shift ="Shifting Model")) +
  scale_shape_manual(values = c(opt_det_rsft = 4,opt_shift = 3), name = "", labels =c(opt_det_rsft = "Optimal Strategy",opt_shift ="Shifting Model")) +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "#FBFBFB", fill = "#FBFBFB"),
        legend.position = "bottom",legend.direction = "horizontal",
        legend.title = element_text(size=5,colour = "#FBFBFB"),legend.text = element_text(size = 12,colour = "#FBFBFB"),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 15),
        axis.title.x = element_text(family = "Arial", size = 15),
        #axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 12),
        axis.text.x.bottom =  element_blank(), #, element_text(family = "Arial", size = 12),
        axis.ticks.x.bottom=element_blank(),
        #axis.ticks.y.left=element_blank(),
        plot.background = element_rect(fill = "#FBFBFB", colour = NA),
        panel.background = element_rect(fill = "#FBFBFB", colour = NA),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))


ggsave("../figures/talk_performancegaineffect.png", width = 7, height = 6, scale = 0.7) 

# Plot performance gain -> Optimal Model
ggplot(predictions[model == "opt_det_rsft"], aes(x = trial, y=mean_pred*100, color = model))+
  #geom_hline(yintercept = 50,  linetype = 2, size = 0.2, colour = "grey45")+
  geom_bar(data = predictions[model == "opt_det_rsft"],
           mapping = aes(x=trial, y = mean_behav*100),
           fill = "#FFFFFF",
           stat = "identity",
           inherit.aes = F,
           colour = "black",
           width = 0.5,
           position = position_dodge()) +
  geom_errorbar(data = predictions[model == "opt_det_rsft"],
                mapping = aes(x=trial, ymin = mean_behav*100, ymax= mean_behav*100 +  sd_behavior*100),
                width=.25, color = "black",
                position=position_dodge(.5),
                inherit.aes = F) +
  theme_classic(base_family = "Arial") +
  geom_point(data = predictions[model == "opt_det_rsft"],
             aes(x = trial, y = mean_pred*100, color = model, fill = model, shape = model),
             size = 3, stroke = 2,  inherit.aes = F)  +
  geom_line(data = predictions[model == "opt_det_rsft"],
            mapping = aes(x = trial, y = mean_pred*100, group = model, color = model),
            linetype = 2, inherit.aes = F, size = 0.8, alpha = 0.7, show.legend = F) +
  scale_y_continuous(breaks = c(0,100), limits = c(0,110), expand = c(0,0))+
  labs(x = 'Entscheidungen (1-5)', y = 'Performanz (%)')+
  scale_fill_manual(values = c(opt_det_rsft = "black"), name = "", labels = c(opt_det_rsft = "Optimal Strategy")) +
  scale_color_manual(values = c(opt_det_rsft = "black"), name = "", labels = c(opt_det_rsft = "Optimal Strategy")) +
  scale_shape_manual(values = c(opt_det_rsft = 4), name = "", labels =c(opt_det_rsft = "Optimal Strategy")) +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "#FBFBFB", fill = "#FBFBFB"),
        legend.position = "bottom",legend.direction = "horizontal",
        legend.title = element_text(size=5),legend.text = element_text(size = 12),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 15),
        axis.title.x = element_text(family = "Arial", size = 15),
        #axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 12),
        axis.text.x.bottom =  element_blank(), #, element_text(family = "Arial", size = 12),
        axis.ticks.x.bottom=element_blank(),
        #axis.ticks.y.left=element_blank(),
        plot.background = element_rect(fill = "#FBFBFB", colour = NA),
        panel.background = element_rect(fill = "#FBFBFB", colour = NA),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))

ggsave("../figures/talk_performancegain_opt.png", width = 7, height = 6, scale = 0.7) 

# Plot with predictions
ggplot(predictions[model == "opt_shift" | model == "opt_det_rsft"], aes(x = trial, y=mean_pred*100, color = model))+
  #geom_hline(yintercept = 50,  linetype = 2, size = 0.2, colour = "grey45")+
  geom_bar(data = predictions[model == "opt_shift"],
           mapping = aes(x=trial, y = mean_behav*100),
           fill = "#FFFFFF",
           stat = "identity",
           inherit.aes = F,
           colour = "black",
           width = 0.5,
           position = position_dodge()) +
  geom_errorbar(data = predictions[model == "opt_shift"],
                mapping = aes(x=trial, ymin = mean_behav*100, ymax= mean_behav*100 +  sd_behavior*100),
                width=.25, color = "black",
                position=position_dodge(.5),
                inherit.aes = F) +
  theme_classic(base_family = "Arial") +
  geom_point(data = predictions[model == "opt_shift"| model == "opt_det_rsft"],
             aes(x = trial, y = mean_pred*100, color = model, fill = model, shape = model),
             size = 3, stroke = 2,  inherit.aes = F)  +
  geom_line(data = predictions[model == "opt_shift" | model == "opt_det_rsft"],
            mapping = aes(x = trial, y = mean_pred*100, group = model, color = model),
            linetype = 2, inherit.aes = F, size = 0.8, alpha = 0.7, show.legend = F) +
  scale_y_continuous(breaks = c(0,100), limits = c(0,110), expand = c(0,0))+
  labs(x = 'Entscheidungen (1-5)', y = 'Performanz (%)')+
  scale_fill_manual(values = c(opt_det_rsft = "black",opt_shift = "blue"), name = "", labels = c(opt_det_rsft = "Optimal Strategy",opt_shift ="Shifting Model")) +
  scale_color_manual(values = c(opt_det_rsft = "black",opt_shift = "blue"), name = "", labels = c(opt_det_rsft = "Optimal Strategy",opt_shift ="Shifting Model")) +
  scale_shape_manual(values = c(opt_det_rsft = 4,opt_shift = 3), name = "", labels =c(opt_det_rsft = "Optimal Strategy",opt_shift ="Shifting Model")) +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "#FBFBFB", fill = "#FBFBFB"),
        legend.position = "bottom",legend.direction = "horizontal",
        legend.title = element_text(size=5),legend.text = element_text(size = 12),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 15),
        axis.title.x = element_text(family = "Arial", size = 15),
        #axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 12),
        axis.text.x.bottom =  element_blank(), #, element_text(family = "Arial", size = 12),
        axis.ticks.x.bottom=element_blank(),
        #axis.ticks.y.left=element_blank(),
        plot.background = element_rect(fill = "#FBFBFB", colour = NA),
        panel.background = element_rect(fill = "#FBFBFB", colour = NA),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))



ggsave("../figures/talk_performancegain1.png", width = 7, height = 6, scale = 0.7) 
