
# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)

# Load data ----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")
source("7-dfe-optsampler.R")

dopt = data.table(xh = 7, pxh = 0.5, yh = 1, pyh = 0.5, xl = 8, pxl = 0.4, yl = 1, pyl = 0.6, b = 11, s = 0)
ntrials = 3

rsft = rsftModel(dopt$xh, dopt$yh, dopt$xl, dopt$yl, dopt$pxh, dopt$pyh, dopt$pxl, dopt$pyl,dopt$b, ntrials ,dopt$s)
dopt = cbind(rsft@compact,dopt)

plotdata_opt = lapply(1:nrow(dopt), function(i){
  d = dopt[i]
  d[, s := state]
  d[, t := trial]
  plotdata_opt = data.table(ss = 1:10)
  plotdata_opt[,pr := computePrOptimal(ss,xh = d$xh, d$yh, d$pxh ,d$xl ,d$yl ,d$pxl , d$b, d$t, d$s), by = c("ss")]
  return(cbind(
    plotdata_opt,
    trial = d$t,
    state = d$s))
}
)

plotdata_opt = rbindlist(plotdata_opt)
dopt = merge(dopt,plotdata_opt, by = c("trial","state"))
View(dopt)
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


dopt = outcomesorder(dopt)


# Figure: Proportion chooing the better Option
small = 3
large = 15
plot_opt = ggplot(dopt[ss != 1], aes(x = ss, y = pr))+
  geom_line(size = 0.9) +
  scale_x_continuous(breaks = seq(1,10,1), expand = c(0,0), limits = c(1,10)) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1.1), expand = c(0,0)) +
  # scale_colour_manual(values = c("gray63","blue","black"),name = "Choice Problem") +
  #geom_point(pointdata, mapping = aes(x=x, y=y),inherit.aes = F, show.legend = F, size = 3) +
  theme_classic(base_family = "Arial") +
  labs(x = 'Number of Draws per Option', y = 'Proportion of Choosing\nthe Better Option')+
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white", fill = alpha("white",0)),
        legend.position = c(0.50 , 0.25),
        legend.title = element_text(size=11),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 13),
        axis.title.x = element_text(family = "Arial", size = 13), 
        axis.text.y.left = element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 11),
        strip.background = element_rect(colour="black", size = 0.01)) +
  facet_wrap(trial~state)
ggsave("../figures/enviroment.png",width = 20, height = 25)


plotdata_opt[,stimuli := paste0(xh," (",100*pxh,"%) or ",yh, " vs. ", xl," (",100*pxl,"%) or ",yl)]

saveRDS(plotdata_opt, "../stimuli/plotdata_opt-t1-30.rds")

# Advantage
# 3108: 16 / 3110: 18 / 3057: 16
sel = stimuli[nr %in% c(3108,3057,3110)][trial == 3]

advantage = stimuli[nr == 3057 & state == 16 & trial == 3]
# d = stimuli1[nr %in% ex$nr & trial == 1]
# d[,s := 0]
# d[,t := 1]
plotdata_adv = data.table(ss = 1:50)
advantage[, s := state]
advantage[,t := trial]
d = advantage

plotdata_adv[,c("subdiff", "optdiff") := computeAbserdiff(ss,xh = d$xh, d$yh, d$pxh ,d$xl ,d$yl ,d$pxl , d$b, d$t, d$s), by = ss]

