#===========================================================================
# Simulations: experience based risky choices with goals
#
#===========================================================================


# Load Packages-------------------------------------------------------------
pacman::p_load(data.table, patchwork)
library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)

# Load Choice Enviroment -----------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ntrials = 3
difference <- fread("../stimuli/dfe-simulations-4a.csv")
dopt <- fread("../stimuli/dfe-simulations-4b.csv")


# Simulation Functions -------------------------------------------------------
computeExpectedReward = function(ss,xh,yh,pxh,xl,yl,pxl,b,t,s){
  
  ob = 0:ss
  belx = (ob + 1)/ (ob + 1 + ss-ob + 1)
  bely = 1 - belx
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2)))
  
  data = data.table(xh = xh, yh = yh, pxh = pxh, pyh = 1 - pxh,
                    xl = xl, yl = yl, pxl = pxl, pyl = 1 - pxl,
                    bxh = belx[com[,1]], byh = bely[com[,1]],
                    bxl = belx[com[,2]], byl = bely[com[,2]],
                    t = t, s = s, b = b)
  
  rsft_opt <- hm1988(
    ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
    trials = ~t,        # NEW: ".ALL" will predict for *all possible* trials
    data = data,            # our data (as before)
    budget = ~b,       # name of our budget column in our data
    initstate = 0,     # name of our starting-state column in our data
    ntrials = ntrials,            # we always 5 trials therefore I hard-code this
    states = ~s,        # NEW: ".ALL" will predict for *all possible* states
    choicerule = "softmax",
    fix = list(tau = 0.2))
  
  opt = predict(rsft_opt, type="values")
  
  rsft_sub <- hm1988(
    ~ xh + bxh + yh + byh | xl + bxl  + yl + byl,  # our formula (as before)
    trials = ~t,        # NEW: ".ALL" will predict for *all possible* trials
    data = data,            # our data (as before)
    budget = ~b,       # name of our budget column in our data
    initstate = 0,     # name of our starting-state column in our data
    ntrials = ntrials,            # we always 5 trials therefore I hard-code this
    states = ~s,        # NEW: ".ALL" will predict for *all possible* states
    choicerule = "softmax",
    fix = list(tau = 0.2))
  
  ER =  predict(rsft_sub, type="values")
  
  return(list(ER,opt))
}

computeAbserdiff = function(ss,xh,yh,pxh,xl,yl,pxl,b,t,s){
  binomh = dbinom(0:ss,ss,pxh)
  binoml = dbinom(0:ss,ss,pxl)
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2))) # N! / (N-k)! * k! = combinations without repetitions
  ER_opt = computeExpectedReward(ss,xh,yh,pxh,xl,yl,pxl,b,t,s)
  ER = ER_opt[[1]]
  opt = ER_opt[[2]]
  subdiff = abs(ER[,1] - ER[,2]) %*% (binomh[com[,1]] * binoml[com[,2]])
  optdiff = abs(opt[1,1] - opt[1,2])
  return(list(subdiff = subdiff[1,1], optdiff = unname(optdiff)))
}

computePrOptimal = function(ss,xh,yh,pxh,xl,yl,pxl,b,t,s){
  binomh = dbinom(0:ss,ss,pxh)
  binoml = dbinom(0:ss,ss,pxl)
  com = as.matrix(expand.grid(rep(list(1:(ss+1)),2)))
  ER_opt = computeExpectedReward(ss,xh,yh,pxh,xl,yl,pxl,b,t,s)
  ER = cr_argmax(ER_opt[[1]])
  opt = cr_argmax(ER_opt[[2]])
  
  propt = as.numeric((opt[,1] - opt[,2]) == (ER[,1] - ER[,2])) %*% (binomh[com[,1]] * binoml[com[,2]])
  return(propt[,1])
}



# Run functions for choice environments ---------------------------------------------------------------------

# Simulations H2/H3
plotdata_diff = data.table(ss = 1:10)
difference[, s := state]
difference[,t := trial]
d = difference
plotdata_diff[,c("subdiff", "optdiff") := computeAbserdiff(ss,xh = d$xh, d$yh, d$pxh ,d$xl ,d$yl ,d$pxl , d$b, d$t, d$s), by = ss]

# Simulation H4
plotdata_opt = lapply(1:nrow(dopt), function(i){
  d = dopt[i]
  d[, s := state]
  d[, t := trial]
  plotdata_opt = data.table(ss = 1:10)
  plotdata_opt[,pr := computePrOptimal(ss,xh = d$xh, d$yh, d$pxh ,d$xl ,d$yl ,d$pxl , d$b, d$t, d$s), by = c("ss")]
  return(cbind(
    plotdata_opt,
    nr = d$nr))
}
)

plotdata_opt = rbindlist(plotdata_opt)
plotdata_opt = merge(plotdata_opt,dopt[,.(xh,yh,pxh,pyh,xl,yl,pxl,pyl,b,trial,nr)], by="nr")

# Plot Simulation Results ---------------------------------------------------------------------------------------

# Figure: Perceived Difference between the options
ladv = melt(plotdata_diff,
            id = "ss")
plot_adv = ggplot(ladv[ss <= 30], aes(x = ss, y = value))+
  geom_line(aes(linetype = factor(variable,levels = c("optdiff","subdiff")), colour = factor(variable,levels = c("optdiff","subdiff")))) +
  scale_x_continuous(breaks = c(1,seq(5,max(ladv$ss),5)), limits=c(1,31) ,expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), expand = c(0,0)) +
  theme_classic(base_family = "Arial") +
  scale_color_manual(values = c("black","blue"),name = "Condition", labels = c("Description","Experience"))+
  scale_linetype_manual(values = c(2,1),name = "Condition", labels = c("Description","Experience"))+
  labs(x = 'Number of Draws per Option', y = "Perceived Difference") +
  theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "white"),
        legend.position = c(0.80 , 0.25),
        title = element_text(size=11, family = "Arial"),
        legend.title = element_text(size=11),legend.text = element_text(size = 10),
        text = element_text(family = "Arial", size = 11),
        axis.title.y = element_text(family = "Arial", size = 11),
        axis.title.x = element_text(family = "Arial", size = 11), 
        axis.text.y.left = element_text(family = "Arial", size = 11),
        axis.text.x.bottom = element_text(family = "Arial", size = 11),
        strip.background = element_rect(colour="black", size = 0.01))





# Figure: Proportion choosing the better Option

# Order Outcomes
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

small = 3
large = 15

plot_opt = ggplot(plotdata_opt[ss != 1], aes(x = ss, y = pr, linetype = stimuli))+
  geom_line(aes(linetype = stimuli),size = 0.9) +
  scale_x_continuous(breaks = c(2,small,seq(5,max(plotdata_opt$ss),5)), expand = c(0,0), limits = c(2,max(plotdata_opt$ss))) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1.1), expand = c(0,0)) +
  scale_linetype_manual(values = c(3,2,1),name = "Choice Task") +
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
        strip.background = element_rect(colour="black", size = 0.01))


