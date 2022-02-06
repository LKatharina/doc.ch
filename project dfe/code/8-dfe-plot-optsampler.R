# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)
windowsFonts(Arial=windowsFont("Arial"))

# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#plotdata <- read.table("../stimuli/plotdata-420-30-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
#stimuli <- read.table("../stimuli/dfe-stimuli-20-t5.csv", header=T, sep=",", as.is=T, na.strings=c("NA"))
plotdata_adv = readRDS("../stimuli/plotdata-t3-50_3.RDS")
plotdata_opt = readRDS("../stimuli/plotdata-t3-50_1.RDS")
ntrials = 3


# Figure: Proportion chooing the better Option
small = 3
large = 15
plot_opt = ggplot(plotdata_opt, aes(x = ss, y = pr))+
  geom_line(size = 0.6,colour = "blue", show.legend = F) +
  geom_segment(aes(x = small, xend = small, y = 0, yend = plotdata_opt[ss == small,pr]),linetype = 2) +
  geom_segment(aes(x = 1, xend = small, y = plotdata_opt[ss == small,pr], yend = plotdata_opt[ss == small,pr]),linetype = 2) +
  geom_segment(aes(x = large, xend = large, y = 0, yend = plotdata_opt[ss == large,pr]),linetype = 2) +
  geom_segment(aes(x = 1, xend = large, y = plotdata_opt[ss == large,pr], yend = plotdata_opt[ss == large,pr]), linetype = 2) +
  scale_x_continuous(breaks = c(1,small,seq(5,max(plotdata_opt$ss),5)), expand = c(0,0), limits = c(1,max(plotdata_opt))) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), expand = c(0,0)) +
  #geom_point(pointdata, mapping = aes(x=x, y=y),inherit.aes = F, show.legend = F, size = 3) +
  theme_classic(base_family = "Arial") +
  labs(x = 'Sample size per Option', y = 'Proportion choosing\nthe better Option', tag = "b")+
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),
        legend.position = c(0.80 , 0.25),
        legend.title = element_text(size=11),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 13),
        axis.title.x = element_text(family = "Arial", size = 13), 
        axis.text.y.left = element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 11),
        strip.background = element_rect(colour="black", size = 0.01))


# Figure: Perceived Advantage of the better option
ladv = melt(plotdata_adv,
             id = "ss")
plot_adv = ggplot(ladv, aes(x = ss, y = value))+
  geom_line(aes(linetype = factor(variable,levels = c("optdiff","subdiff")), colour = factor(variable,levels = c("optdiff","subdiff")))) +
  scale_x_continuous(breaks = c(1,seq(5,max(ladv$ss),5)), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), expand = c(0,0)) +
  #geom_point(pointdata, mapping = aes(x=x, y=y),inherit.aes = F, show.legend = F, size = 3) +
  theme_classic(base_family = "Arial") +
  scale_color_manual(values = c("black","blue"),name = "Condition", labels = c("Description","Experience"))+
  scale_linetype_manual(values = c(1,1),name = "Condition", labels = c("Description","Experience"))+
  labs(x = 'Sample Size per Option', y = 'Perceived Advantage\nof the better Option',tag = "a")+
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),
        legend.position = c(0.80 , 0.25),
        legend.title = element_text(size=11),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 13),
        axis.title.x = element_text(family = "Arial", size = 13), 
        axis.text.y.left = element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 11),
        strip.background = element_rect(colour="black", size = 0.01))



comp <- plot_adv  + plot_spacer() + plot_opt +
  plot_layout(widths = c(.4,.01,.4)) #& theme(legend.position = 'bottom')
ggsave("../figures/dfe_performance_plot.png", width = 17, height = 6, scale = 0.5) 

