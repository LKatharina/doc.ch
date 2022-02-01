#============================================================================
# Terminal Reward Functions: Plot
# 
#============================================================================


# Load Packages -------------------------------------------------------------
pacman::p_load(data.table)
windowsFonts(Arial=windowsFont("Arial"))
# Read data -----------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")
data <- read.csv("../data/rsft-gain-loss-processed.csv", header=T, as.is=T, na.strings=c("-77","-99","-66", "NA"))

# Parmeters k for s-haped reward function --------------------------------------
par = c(0.1,0.2,0.5,1,10)

# Example ----------------------------------------------------------------------
data = as.data.table(data)
stimuli = data[trial == 1]
stimuli = stimuli[duplicated(nr) == F]
d = stimuli[1,]

# Calculates final states ------------------------------------------------------
# rsftStates(xh, yh, xl, yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start,choiceprob, final)
stateFinal = rsftStates(d$xh, d$yh, d$xl, d$yl,
                        d$pxh, d$pyh, d$pxl, d$pyl,
                        d$budget,
                        timeHorizon = 5,
                        d$start,
                        choiceprob = 0.5,
                        final = T)

fs =  as.numeric(c(stateFinal$state,24.9999))

# Step Function ----------------------------------------------------------------
Rstep = NULL
for(i in fs){
  x = step(d$budget, i)
  Rstep = c(Rstep, x)
}

terminalreward_step = as.data.table(cbind(values = as.numeric(Rstep), s = fs, parameter = -99))
terminalreward_step[,RF := "Step"]

# logistic Function --------------------------------------------------------------
LogisticReward = function(k){
  Rlogistic = NULL
  for(i in fs){
    x = logistic(d$budget, i, k)
    Rlogistic = c(Rlogistic, x)
  }
  return(as.numeric(Rlogistic))
}

terminalreward_logistic = lapply(par, function(i) {
  datalogistic = LogisticReward(i)
  
  return(as.data.table(cbind(
    values = datalogistic,
    s =  fs,
    parameter = i
    )))
  }
)

terminalreward_logistic = rbindlist(terminalreward_logistic)
terminalreward_logistic[,RF := "S-shaped"]
plotdata = rbind(terminalreward_step,terminalreward_logistic)
plotdata[,parameter := as.factor(parameter)]
par = as.factor(par)

# Plot reward function ------------------------------------------------------------------------
p = ggplot(plotdata[parameter %in% par[which(par != "0.5")]],
           aes(x = s, y = values, group = parameter, colour = parameter))+
  theme_classic(base_size = 18, base_family = "Arial") +
  geom_line(show.legend = F, colour = "grey50", linetype = 2, size = 0.8, alpha = 0.5) +
  labs(x = 'States', y = 'Terminal Reward', tag = "a")+
  scale_x_continuous(breaks = c(12.5,24.999,25,37.5), labels = c("state < goal"," ","state = goal","state > goal")) +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),#legend.text = element_text(size = 10),
        text = element_text(family = "Arial"), axis.title.y = element_text(family = "Arial"),
        axis.title.x = element_text(family = "Arial"), 
        axis.text.y.left = element_text(family = "Arial"),
        axis.text.x.bottom = element_text(family = "Arial"),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"),
        axis.ticks.x = element_blank())

RFplot = p + 
  theme_classic(base_size = 18, base_family = "Arial") +
  geom_line(plotdata[parameter %in% c("0.5","-99")], mapping = aes(x = s, y = values, group = parameter, colour = parameter), linetype = 1, size = 0.8, inherit.aes = F, show.legend = F) +
  labs(x = 'States', y = 'Terminal Reward', tag = "a")+
  scale_color_manual(values=c("black","blue"),name = "Reward Function",labels = c("Step","S-shaped (k = 0.5)"))+
  scale_x_continuous(breaks = c(12.5,24.9999,25,37.5), labels = c("state < goal"," ","state = goal","state > goal")) +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),#legend.text = element_text(size = 10),
        text = element_text(family = "Arial"), axis.title.y = element_text(family = "Arial"),
        axis.title.x = element_text(family = "Arial"), 
        axis.text.y.left = element_text(family = "Arial"),
        axis.text.x.bottom = element_text(family = "Arial"),
        strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(family = "Arial"),
        axis.ticks.x = element_blank())


