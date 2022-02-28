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
data
data_agg = data[ , .(mprhv = mean(prhv), sd = sd(prhv)/sqrt(.N), n = .N), by=c("rewardfunction","dataset")]
data_agg[rewardfunction == "S-shaped", sd := 0]
# choiceplot = ggplot(data_agg, aes(x = dataset, y = mprhv, fill = rewardfunction, colour = rewardfunction))+
#   geom_bar(data = data_agg[rewardfunction == "choice"],
#            mapping = aes(x=dataset, y = mprhv),
#            stat = "identity",
#            inherit.aes = F,
#            fill = "grey93", colour = "black", width = 0.5) +
#   geom_errorbar(data = data_agg[rewardfunction == "choice"],
#                 mapping = aes(x=dataset, ymin = mprhv, ymax= mprhv + sd),
#                 width=.25, color = "black", position=position_dodge(.9),
#                 inherit.aes = F) +
#   geom_point(data = data_agg[rewardfunction != "choice"],
#              mapping = aes(x=dataset, y = mprhv, fill = rewardfunction),
#              shape = 21 ,size = 4,
#              inherit.aes = F)+ 
#   geom_line(data = data_agg[rewardfunction != "choice"],
#             mapping = aes(group = interaction(rewardfunction), color = rewardfunction, linetype = rewardfunction),
#             size = 0.5) +
#   scale_y_continuous(limits = c(0,1), expand = c(0,0))+
#   scale_fill_manual(values=c("blue","grey28"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
#   scale_linetype_manual(values=c("solid","dashed"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
#   scale_color_manual(values=c("blue","black"), name = "Reward \nFunction", labels = c("S-shaped \n(k = 0.5)", "Step"))+
#   theme_classic(base_family = "Arial") + #base_size = 18
#   labs(x = 'Dataset', y = '% Risky Choices', tag = "b") +
#   theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),
#         legend.title = element_text(size=11),legend.text = element_text(size = 10),
#         text = element_text(family = "Arial", size = 11),
#         axis.title.y = element_text(family = "Arial", size = 13),
#         axis.title.x = element_text(family = "Arial", size = 13), 
#         axis.text.y.left = element_text(family = "Arial", size = 11),
#         axis.text.x.bottom = element_text(family = "Arial", size = 11),
#         strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))

choiceplot = ggplot(data_agg, aes(x = dataset, y = mprhv, fill = rewardfunction, colour = rewardfunction))+
  geom_bar(data = data_agg,
                      mapping = aes(x=dataset, y = mprhv, fill = rewardfunction),
                      stat = "identity",
                      inherit.aes = F,
           colour = "black",
           width = 0.5,
           position = position_dodge()) +
  geom_errorbar(data = data_agg,
                mapping = aes(x=dataset, ymin = mprhv, ymax= mprhv + sd),
                width=.25, color = "black",
                position=position_dodge(.5),
                inherit.aes = T) +
  scale_y_continuous(breaks = seq(0,1, 0.25), limits = c(0,1.1), expand = c(0,0))+
  scale_fill_manual(values=c("white","blue","grey28"), name = "", labels = c("Data","Logistic Reward\nFunction (k = 0.5)", "Step Reward\nFunction"))+
  theme_classic(base_family = "Arial") + #base_size = 18
  labs(x = 'Dataset', y = '% Risky Choices', tag = "b") +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white"),
        legend.position = c(0.50 , 0.93),
        legend.direction = "horizontal",
        legend.title = element_text(size=11),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 13),
        axis.title.x = element_text(family = "Arial", size = 13),
        axis.text.y.left = element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 11),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))


comp <- RFplot / choiceplot +
  plot_layout(widths = c(.3,.01,.3)) #& theme(legend.position = 'bottom') + plot_spacer() + 
ggsave("../figure/rewardfunction_plot.png", width = 5, height = 7, scale = 0.95) 
  