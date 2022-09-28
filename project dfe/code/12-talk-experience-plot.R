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
plotdata_opt = readRDS("../stimuli/plotdata_opt-t1-30.RDS")
ntrials = 3

# order
outcomesorder = function(data){

  temp = data
  data$xh = ifelse(temp$xh > temp$yh, temp$xh, temp$yh)
  data$pxh = ifelse(temp$xh > temp$yh, temp$pxh, temp$pyh)
  data$yh = ifelse(temp$xh > temp$yh, temp$yh, temp$xh)
  data$pyh = ifelse(temp$xh > temp$yh, temp$pyh, temp$pxh)
  data$xl = ifelse(temp$xl > temp$yl, temp$xl, temp$yl)
  data$pxl = ifelse(temp$xl > temp$yl, temp$pxl, temp$pyl)
  data$yl = ifelse(temp$xl > temp$yl, temp$yl, temp$xl)
  data$pyl = ifelse(temp$xl > temp$yl, temp$pyl, temp$pxl)
  return(data)
}


plotdata_opt = outcomesorder(plotdata_opt)


plotdata_opt[,stimuli := paste0(xh," (",100*pxh,"%) or ",yh, "   vs.  ", xl," (",100*pxl,"%) or ",yl, ";  goal = ",b)]

# Figure: Perceived Advantage of the better option
ladv = melt(plotdata_adv,
             id = "ss")
plot_adv = ggplot(ladv[ss <= 30], aes(x = ss, y = value))+
  geom_line(aes(linetype = factor(variable,levels = c("optdiff","subdiff")), colour = factor(variable,levels = c("optdiff","subdiff")))) +
  scale_x_continuous(breaks = c(seq(5,max(ladv$ss),5)), limits=c(1,31) ,expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1), expand = c(0,0)) +
  #geom_point(pointdata, mapping = aes(x=x, y=y),inherit.aes = F, show.legend = F, size = 3) +
  #ggtitle(expression(paste(bold("Figure 4. "), italic("Simulation: Perceived Difference"))))+
  theme_classic(base_family = "Arial") +
  scale_color_manual(values = c("blue","blue"),name = "", labels = c("Description","Experience"))+
  scale_linetype_manual(values = c(2,1),name = "", labels = c("Description","Experience"))+
  labs(x = 'Experience', y = "Performance") +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white"),
        legend.position = c(0.70 , 0.25),
        legend.title = element_text(size=11),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 15),
        axis.title.x = element_text(family = "Arial", size = 15),
        axis.text.y.left = element_blank(),#element_text(family = "Arial", size = 12),
        axis.text.x.bottom =  element_blank(), #, element_text(family = "Arial", size = 12),
        axis.ticks.x.bottom=element_blank(),
        axis.ticks.y.left=element_blank(),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"))

  # theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white"),
  #       #legend.position = c(0.80 , 0.25),
  #       legend.position = c(0.70 , 0.25),
  #       title = element_text(size=11, family = "Arial"),
  #       legend.title = element_text(size=11),legend.text = element_text(size = 10),
  #       text = element_text(family = "Arial", size = 11),
  #       axis.title.y = element_text(family = "Arial", size = 11),
  #       axis.title.x = element_text(family = "Arial", size = 11), 
  #       axis.text.y.left = element_text(family = "Arial", size = 11),
  #       axis.text.x.bottom = element_text(family = "Arial", size = 11),
  #       strip.background = element_rect(colour="black", size = 0.01))
ggsave("../figures/talk-dfe-plot1.png", width = 5, height = 5, scale = 0.5) 

comp <- plot_adv  + plot_spacer() + plot_opt +
  plot_layout(widths = c(.4,.01,.4)) #& theme(legend.position = 'bottom')


plot_adv = ggplot(ladv[ss <= 30], aes(x = ss, y = value))+
  # geom_line(aes(linetype = factor(variable,levels = c("optdiff","subdiff")), colour = factor(variable,levels = c("optdiff","subdiff")))) +
  scale_x_continuous(breaks = c(seq(5,max(ladv$ss),5)), limits=c(1,31) ,expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1), expand = c(0,0)) +
  # #geom_point(pointdata, mapping = aes(x=x, y=y),inherit.aes = F, show.legend = F, size = 3) +
  # #ggtitle(expression(paste(bold("Figure 4. "), italic("Simulation: Perceived Difference"))))+
  theme_classic(base_family = "Arial") +
  # scale_color_manual(values = c("black","blue"),name = "Format", labels = c("Description","Experience"))+
  # scale_linetype_manual(values = c(2,1),name = "Format", labels = c("Description","Experience"))+
  labs(x = 'Experience', y = "Performance") +
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

ggsave("../figures/talk-dfe-plot.png", width = 5, height = 5, scale = 0.5) 

