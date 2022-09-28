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
# Figure: Proportion chooing the better Option
small = 3
large = 15
plot_opt = ggplot(plotdata_opt[ss != 1], aes(x = ss, y = pr, linetype = stimuli))+
  geom_line(aes(linetype = stimuli),size = 0.9) +
  # geom_segment(aes(x = small, xend = small, y = 0, yend = plotdata_opt[ss == small,pr]),linetype = 2) +
  # geom_segment(aes(x = 1, xend = small, y = plotdata_opt[ss == small,pr], yend = plotdata_opt[ss == small,pr]),linetype = 2) +
  # geom_segment(aes(x = large, xend = large, y = 0, yend = plotdata_opt[ss == large,pr]),linetype = 2) +
  # geom_segment(aes(x = 1, xend = large, y = plotdata_opt[ss == large,pr], yend = plotdata_opt[ss == large,pr]), linetype = 2) +
  scale_x_continuous(breaks = c(seq(5,max(plotdata_opt$ss),5)), expand = c(0,0), limits = c(2,max(plotdata_opt$ss))) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1.1), expand = c(0,0)) +
 # scale_colour_manual(values = c("gray63","blue","black"),name = "Choice Problem") +
  scale_linetype_manual(values = c(3,2,1),name = "Choice Task") +
  #geom_point(pointdata, mapping = aes(x=x, y=y),inherit.aes = F, show.legend = F, size = 3) +
  theme_classic(base_family = "Arial") +
  labs(x = 'Draws per Option', y = 'Proportion of Choosing\nthe Better Option')+
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white", fill = alpha("white",0)),
        legend.position = c(0.50 , 0.25),
        legend.title = element_text(size=11),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 13),
        axis.title.x = element_text(family = "Arial", size = 13), 
        axis.text.y.left = element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 11),
        strip.background = element_rect(colour="black", size = 0.01))
ggsave("../figures/dfe_performance_plot_optimality.png", width = 6, height = 4, scale = 0.8) 


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
  scale_color_manual(values = c("black","blue"),name = "Format", labels = c("Description","Experience"))+
  scale_linetype_manual(values = c(2,1),name = "Format", labels = c("Description","Experience"))+
  labs(x = 'Draws per Option', y = "Perceived Difference") +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white"),
        #legend.position = c(0.80 , 0.25),
        legend.position = c(0.70 , 0.25),
        title = element_text(size=11, family = "Arial"),
        legend.title = element_text(size=11),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 11),
        axis.title.x = element_text(family = "Arial", size = 11), 
        axis.text.y.left = element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 11),
        strip.background = element_rect(colour="black", size = 0.01))
ggsave("../figures/dfe_perceived_diff_plot.png", width = 6, height = 6, scale = 0.5) 

comp <- plot_adv  + plot_spacer() + plot_opt +
  plot_layout(widths = c(.4,.01,.4)) #& theme(legend.position = 'bottom')

