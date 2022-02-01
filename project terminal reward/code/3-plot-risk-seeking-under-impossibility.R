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
source("1-reward-functions.R")
source("2-plot-reward-functions.R")
data <- read.csv("../data/rsft-gain-loss-processed.csv", header=T, as.is=T, na.strings=c("-77","-99","-66", "NA"))

# restructure data
cdata = impossible[parameter == "-99",][,':='(prhv = choice, rewardfunction = "choice")]
pdata = impossible[parameter %in% c("0.5","-99")][,rewardfunction := ifelse(parameter == 0.5, "S-shaped","Step")]
data = rbind(cdata,pdata)

# aggregation
data
data_agg = data[ , .(mprhv = mean(prhv), sd = sd(prhv)/sqrt(.N), n = .N), by=c("rewardfunction","domain")]

choiceplot = ggplot(data_agg, aes(x = domain, y = mprhv, fill = rewardfunction, colour = rewardfunction))+
  geom_bar(data = data_agg[rewardfunction == "choice"],
           mapping = aes(x=domain, y = mprhv),
           stat = "identity",
           inherit.aes = F,
           fill = "grey93", colour = "black") +
  geom_errorbar(data = data_agg[rewardfunction == "choice"],
                mapping = aes(x=domain, ymin = mprhv, ymax= mprhv + sd),
                width=.5, color = "black", position=position_dodge(.9),
                inherit.aes = F) +
  geom_point(data = data_agg[rewardfunction != "choice"],
             mapping = aes(x=domain, y = mprhv, fill = rewardfunction),
             shape = 21 ,size = 4,
             inherit.aes = F)+ 
  geom_line(data = data_agg[rewardfunction != "choice"],
            mapping = aes(group = interaction(rewardfunction), color = rewardfunction, linetype = rewardfunction),
            size = 0.5) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))+
  scale_fill_manual(values=c("blue","grey28"), name = "Reward Function", labels = c("S-shaped (k = 0.5)", "Step"))+
  scale_linetype_manual(values=c("solid","dashed"), name = "Reward Function", labels = c("S-shaped (k = 0.5)", "Step"))+
  scale_color_manual(values=c("blue","black"), name = "Reward Function", labels = c("S-shaped (k = 0.5)", "Step"))+
  theme_classic(base_size = 18, base_family = "Arial") +
  labs(x = 'Dataset', y = '% Risky Choices', tag = "b") +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),#legend.text = element_text(size = 10),
        text = element_text(family = "Arial"), axis.title.y = element_text(family = "Arial"),
        axis.title.x = element_text(family = "Arial"), 
        axis.text.y.left = element_text(family = "Arial"),
        axis.text.x.bottom = element_text(family = "Arial"),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))


comp <- RFplot  + plot_spacer() + choiceplot +
  plot_layout(guides = "collect", widths = c(.4,.05,.4))

  