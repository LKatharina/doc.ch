#============================================================================
# Risk Seeking under Impossibility
# 
#============================================================================

# Load Packages -------------------------------------------------------------
pacman::p_load(data.table,patchwork)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")
source("1-rsui-function.R")
source("2-rsui-data.R")
source("2-plot-reward-functions.R")

# restructure data
cdata = impossible[parameter == "-99",][,':='(prhv = choice, rewardfunction = "Choice")]
pdata = impossible[parameter %in% c("0.5","-99")][,rewardfunction := ifelse(parameter == 0.5, "S-shaped","Step")]
data = rbind(cdata,pdata)

# aggregation
data_agg = data[ , .(mprhv = mean(prhv), sd = sd(prhv)/sqrt(.N), n = .N), by=c("rewardfunction","dataset")]
data_agg[rewardfunction == "S-shaped", sd := 0]

# Only effect
choiceplot = ggplot(data_agg, aes(x = dataset, y = mprhv, fill = rewardfunction, colour = rewardfunction))+
  geom_hline(yintercept = 0.5, linetype = 2, size = 0.2, colour = "grey45") +
  geom_bar(data = data_agg[rewardfunction == "Choice"],
           mapping = aes(x=dataset, y = mprhv),
           stat = "identity",
           inherit.aes = F,
           fill = "white", colour = "black", width = 0.5) +
  geom_errorbar(data = data_agg[rewardfunction == "Choice"],
                mapping = aes(x=dataset, ymin = mprhv, ymax= mprhv + sd),
                width=.25, color = "black", position=position_dodge(.9),
                inherit.aes = F) +
 
  # geom_point(data = data_agg[rewardfunction != "choice"],
  #            mapping = aes(x=dataset, y = mprhv, fill = rewardfunction),
  #            shape = 21 ,size = 4,
  #            inherit.aes = F)+
  # geom_line(data = data_agg[rewardfunction != "choice"],
  #           mapping = aes(group = interaction(rewardfunction), color = rewardfunction, linetype = rewardfunction),
  #           size = 0.5) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  # scale_fill_manual(values=c("blue","grey28"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
  # scale_linetype_manual(values=c("solid","dashed"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
  # scale_color_manual(values=c("blue","black"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
  theme_classic(base_family = "Arial") + #base_size = 18
  labs(x = 'Dataset', y = '% Risky Choices') +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),
        legend.title = element_text(size=11),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 13),
        axis.title.x =  element_blank(), #element_text(family = "Arial", size = 13),
        axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 9, colour="black"),
        axis.ticks.y.left=element_blank(),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))

ggsave("../figure/pres_goodeffortplot.png", width = 5, height = 5, scale = 0.5) 

# Effect and Predictions
choiceplot = ggplot(data_agg[rewardfunction == "S-shaped"], aes(x = dataset, y = mprhv, fill = rewardfunction, colour = rewardfunction))+
  geom_hline(yintercept = 0.5, linetype = 2, size = 0.2, colour = "grey45") +
  geom_bar(data = data_agg[rewardfunction == "Choice"],
           mapping = aes(x=dataset, y = mprhv),
           stat = "identity",
           inherit.aes = F,
           fill = "white", colour = "black", width = 0.5) +
  geom_errorbar(data = data_agg[rewardfunction == "Choice"],
                mapping = aes(x=dataset, ymin = mprhv, ymax= mprhv + sd),
                width=.25, color = "black", position=position_dodge(.9),
                inherit.aes = F) +
  geom_point(data = data_agg[rewardfunction == "S-shaped"],
             mapping = aes(x=dataset, y = mprhv, colour = rewardfunction),
             size = 3, shape = 3, stroke = 2,
             inherit.aes = F)+
  geom_line(data = data_agg[rewardfunction == "S-shaped"],
            mapping = aes(x = dataset, y = mprhv, colour = rewardfunction, group = interaction(rewardfunction)),
                          linetype = 2, inherit.aes = F, size = 0.8, alpha = 0.7) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  scale_color_manual(values = c("blue"), name = "", labels = c("Subjective Rewards")) +
  #scale_fill_manual(values=c("blue"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)"))+
  # scale_linetype_manual(values=c("solid","dashed"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
  # scale_color_manual(values=c("blue","black"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
  theme_classic(base_family = "Arial") + #base_size = 18
  labs(x = 'Dataset', y = '% Risky Choices') +
  theme(legend.background = element_blank(),#element_rect(linetype = 1, size = 0.01, colour = "white"),
        legend.position = c(0.40 , 0.94),
        legend.title = element_text(size=5),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 13),
        axis.title.x =  element_blank(), #element_text(family = "Arial", size = 13),
        axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 9, colour="black"),
        axis.ticks.y.left=element_blank(),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))


choiceplot = ggplot(data_agg, aes(x = dataset, y = mprhv, fill = rewardfunction, colour = rewardfunction))+
  #geom_hline(yintercept = 0.5, linetype = 2, size = 0.2, colour = "grey45") +
  geom_bar(data = data_agg[rewardfunction == "Choice"],
           mapping = aes(x=dataset, y = mprhv*100),
           stat = "identity",
           inherit.aes = F,
           fill = "white", colour = "black", width = 0.5) +
  geom_errorbar(data = data_agg[rewardfunction == "Choice"],
                mapping = aes(x=dataset, ymin = mprhv*100, ymax= mprhv*100 + sd*100),
                width=.25, color = "black", position=position_dodge(.9),
                inherit.aes = F) +
  geom_point(data = data_agg[rewardfunction == "S-shaped" | rewardfunction == "Step"],
             mapping = aes(x=dataset, y = mprhv*100, colour = rewardfunction, fill = rewardfunction, shape = rewardfunction),
             size = 3, stroke = 2,
             inherit.aes = F)+
  geom_line(data = data_agg[rewardfunction == "S-shaped" | rewardfunction == "Step"],
            mapping = aes(x = dataset, y = mprhv*100, colour = rewardfunction, group = interaction(rewardfunction)),
            linetype = 2, inherit.aes = F, size = 0.8, alpha = 0.7, show.legend = F) +
  scale_y_continuous(breaks = c(0,100), limits = c(0,110), expand = c(0,0))+
  scale_color_manual(values = c("blue", "black"), name = "", labels = c("Subjective Rewards", "Optimal Strategy")) +
  scale_fill_manual(values = c("blue", "black"), name = "", labels = c("Subjective Rewards", "Optimal Strategy")) +
  scale_shape_manual(values = c(3,4), name = "", labels = c("Subjective Rewards","Optimal Strategy")) +
  #scale_fill_manual(values=c("blue"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)"))+
  # scale_linetype_manual(values=c("solid","dashed"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
  # scale_color_manual(values=c("blue","black"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
  theme_classic(base_family = "Arial") + #base_size = 18
  labs(x = 'Dataset', y = 'Risky Choices (%)') +
  theme(
        legend.background = element_rect(linetype = 1, size = 0.01, colour = "#FBFBFB", fill = "#FBFBFB"),
        legend.position = "bottom",legend.direction = "horizontal",
        legend.title = element_text(size=5),legend.text = element_text(size = 12),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 15),
        axis.title.x =  element_blank(), #element_text(family = "Arial", size = 13),
        #axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 12, colour="black"),
        #axis.ticks.y.left=element_blank(),
        plot.background = element_rect(fill = "#FBFBFB", colour = NA),
        panel.background = element_rect(fill = "#FBFBFB", colour = NA),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))


ggsave("../figure/pres_goodeffortplot1.png", width = 7, height = 6, scale = 0.7) 


picto = data.table(x = rep(1:5,2), y = c(c(0.2,0.6,0.8,0.3,0.7),c(0.6,0.1,0.4,0.9,0.2)), z = c(rep("d", 5),rep("p", 5)))
ggplot(picto, aes(x = x, y = y, fill = z, colour = z))+
  #geom_hline(yintercept = 0.5, linetype = 2, size = 0.2, colour = "grey45") +
  geom_bar(data = picto[z == "d"],
           mapping = aes(x=x, y = y),
           stat = "identity",
           inherit.aes = F,
           fill = "white",
           colour = "black",
           width = 0.5) +
  geom_point(data = picto[z == "p"],
             mapping = aes(x=x, y = y),
             colour = "black",
             fill = "black",
             shape = 4,
             size = 3, stroke = 2,
             inherit.aes = F,
             show.legend = F) +
  geom_line(data = picto[z == "p"],
            mapping = aes(x = x, y = y, group = interaction(z)),
            linetype = 1, inherit.aes = F, size = 0.8, show.legend = F) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  theme_classic(base_family = "Arial") + #base_size = 18
  theme(
    text =element_blank(),
    axis.title.y = element_blank(),
    axis.title.x =  element_blank(), #element_text(family = "Arial", size = 13),
    axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 11),
    axis.text.x.bottom = element_blank(),
    axis.ticks.y.left=element_blank(),
    axis.ticks.x.bottom=element_blank(),
    plot.background = element_rect(fill = "#FBFBFB", colour = NA),
    panel.background = element_rect(fill = "#FBFBFB", colour = NA),
    strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))

ggsave("../figure/picto.png", width = 2, height = 2, scale = 1) 
