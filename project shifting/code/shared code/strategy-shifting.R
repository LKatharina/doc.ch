# ==========================================================================
# Plot Performance Gain
# data not shared
# ==========================================================================

# Packages
pacman::p_load(data.table, ggplot2)
library(cognitivemodels)


windowsFonts(Arial=windowsFont("Arial"))

# Read Data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("2-shifting-select.R")
source("3-shifting-plot.R")
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")

data = fread("../data/power_data_shift.csv")
tau = 0.21
x = "probabilistic"

data[ad_risky != 0, .(mean = mean(optBehavior)), by = trial][order(trial)]
data[, .(mean = mean(optBehavior)), by = trial][order(trial)]

# Probability heuristic
Probheuristic <- function(data){
  data$high = data[,max(xh,yh), by = 1:nrow(data)]$V1
  data$low = data[,max(xl,yl), by = 1:nrow(data)]$V1
  data$probh <- ifelse(data$xh == data$high, data$pxh, data$pyh)
  data$probl <- ifelse(data$xl == data$low, data$pxl, data$pyl)
  
  sim_heuristik = as.data.table(cr_softmax(x = data[,.(probh,probl)],0.21))
  
  ph_sim <- data.table(
    probh_ph = data$probh,
    probl_ph = data$probl,
    prhv_ph = sim_heuristik$probh)
  
  return(ph_sim)
}

ph_data <- Probheuristic(data)

data = cbind(data,ph_data)
stimuli = data[trial == 1]
stimuli = stimuli[duplicated(nr) == F]

# Optimal Model
rsft = lapply(1:nrow(stimuli), function(i){
  
  d = stimuli[i,]
  
  m = rsftModel(d$xh, d$yh, d$xl, d$yl,
                d$pxh, d$pyh, d$pxl, d$pyl,
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

ldata = melt(data,
            measure.vars = c("opt_ph","opt_rsft","opt_shift", "opt_det_rsft"),
            value.name = "proptimal",
            variable.name = "model")


predictions = ldata[, .(mean_pred = mean(proptimal), mean_behav = mean(optBehavior), sd_behavior = sd(optBehavior)/sqrt(.N)), by = c("trial","model")]

predictions[, trial := as.factor(trial)]

# Plots
# Plot performance change effect
plot <- ggplot(predictions[model == "opt_shift"], aes(x = trial, y=mean_pred*100, color = model))+
  geom_hline(yintercept = 50,  linetype = 2, size = 0.2, colour = "grey45")+
  geom_bar(data = predictions[model == "opt_shift"],
           mapping = aes(x=trial, y = mean_behav*100),
           fill = "white",
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
  #geom_point(data = predictions[model == "opt_shift"],
            # aes(x = trial, y = mean_pred*100, color = model),
            # size = 3, fill = "blue", shape = 3, stroke = 2,  inherit.aes = F)  +
  #geom_line(data = predictions[model == "opt_shift"],
          #  mapping = aes(x = trial, y = mean_pred*100, group = model, color = model), 
          #  linetype = 2, inherit.aes = F, size = 0.8, alpha = 0.7) +
  scale_y_continuous(breaks = seq(0,100, 100), limits = c(0,100), expand = c(0,0))+
  labs(x = 'Choices (1-5)', y = 'Performance (%)')+
  scale_color_manual(values = c("blue"), name = "Model", labels = c("Shifting Model (c=3)")) +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white"),
        legend.position = c(0.45 , 0.85),
        legend.title = element_text(size=13),legend.text = element_text(size = 12),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 15),
        axis.title.x = element_text(family = "Arial", size = 15),
        axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 12),
        axis.text.x.bottom =  element_blank(), #, element_text(family = "Arial", size = 12),
        axis.ticks.x.bottom=element_blank(),
        axis.ticks.y.left=element_blank(),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))


ggsave("../figures/talk_performancegain.png", width = 5, height = 5, scale = 0.5) 

# With the optimal model
plot <- ggplot(predictions[model == "opt_shift"], aes(x = trial, y=mean_pred*100, color = model))+
  geom_hline(yintercept = 50,  linetype = 2, size = 0.2, colour = "grey45")+
  geom_bar(data = predictions[model == "opt_shift"],
           mapping = aes(x=trial, y = mean_behav*100),
           fill = "white",
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
  geom_point(data = predictions[model == "opt_shift"],
             aes(x = trial, y = mean_pred*100, color = model),
             size = 3, fill = "blue", shape = 3, stroke = 2,  inherit.aes = F)  +
  geom_line(data = predictions[model == "opt_shift"],
            mapping = aes(x = trial, y = mean_pred*100, group = model, color = model),
            linetype = 2, inherit.aes = F, size = 0.8, alpha = 0.7) +
  
  geom_point(data = predictions[model == "opt_det_rsft"],
             aes(x = trial, y = mean_pred*100, color = model), inherit.aes = F)+
  theme_classic(base_family = "Arial") +
  #geom_point(data = predictions[model == "opt_shift"],
  # aes(x = trial, y = mean_pred*100, color = model),
  # size = 3, fill = "blue", shape = 3, stroke = 2,  inherit.aes = F)  +
  #geom_line(data = predictions[model == "opt_shift"],
  #  mapping = aes(x = trial, y = mean_pred*100, group = model, color = model), 
  #  linetype = 2, inherit.aes = F, size = 0.8, alpha = 0.7) +
  scale_y_continuous(breaks = seq(0,100, 100), limits = c(0,100), expand = c(0,0))+
  labs(x = 'Choices (1-5)', y = 'Performance (%)')+
  scale_color_manual(values = c("blue"), name = "Model", labels = c("Shifting Model (c=3)")) +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white"),
        legend.position = c(0.45 , 0.85),
        legend.title = element_text(size=13),legend.text = element_text(size = 12),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 15),
        axis.title.x = element_text(family = "Arial", size = 15),
        axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 12),
        axis.text.x.bottom =  element_blank(), #, element_text(family = "Arial", size = 12),
        axis.ticks.x.bottom=element_blank(),
        axis.ticks.y.left=element_blank(),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))


ggsave("../figures/talk_performancegain.png", width = 5, height = 5, scale = 0.5) 

# Plot with predictions
plot <- ggplot(predictions[model == "opt_shift"], aes(x = trial, y=mean_pred*100, color = model))+
  geom_hline(yintercept = 50,  linetype = 2, size = 0.2, colour = "grey45")+
  geom_bar(data = predictions[model == "opt_shift"],
           mapping = aes(x=trial, y = mean_behav*100),
           fill = "white",
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
  geom_point(data = predictions[model == "opt_shift"],
  aes(x = trial, y = mean_pred*100, color = model),
  size = 3, fill = "blue", shape = 3, stroke = 2,  inherit.aes = F)  +
  geom_line(data = predictions[model == "opt_shift"],
   mapping = aes(x = trial, y = mean_pred*100, group = model, color = model),
   linetype = 2, inherit.aes = F, size = 0.8, alpha = 0.7) +
  scale_y_continuous(breaks = seq(0,100, 100), limits = c(0,100), expand = c(0,0))+
  labs(x = 'Choices (1-5)', y = 'Performance (%)')+
  scale_color_manual(values = c("blue"), name = "", labels = c("Shifting Model")) +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white"),
        legend.position = c(0.40 , 0.94),
        legend.title = element_text(size=5),legend.text = element_text(size = 12),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 15),
        axis.title.x = element_text(family = "Arial", size = 15),
        axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 12),
        axis.text.x.bottom =  element_blank(), #, element_text(family = "Arial", size = 12),
        axis.ticks.x.bottom=element_blank(),
        axis.ticks.y.left=element_blank(),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))


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
  labs(x = 'Choices (1-5)', y = 'Performance (%)')+
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
