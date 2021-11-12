# ==========================================================================
# Plot Stimuli
# ==========================================================================

# Load Packages --------------------------------------------------------------------------------
pacman::p_load(data.table, ggplot2)
library(cognitivemodels)
windowsFonts(Arial=windowsFont("Arial"))

# Source ---------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("2-shifting-select.R")
#source("2-shifting-select1.R")

# Plot -----------------------------------------------------------------------------------------
jitter <- position_jitter(width = 0.1)


plotfunc <- function(i){
  plot <- ggplot(stimuli[nr == unique(nr)[i] & model != "shift"] , 
                 aes(x = trial, y=prhv, fill = model))+
    theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 2, size = 0.3)+
    geom_point(size = 1.4, position = jitter, shape = 21) +
    scale_fill_manual(values=c("blue","red"), name = "Model",labels = c("OPT softmax", "PH softmax"))+
    ylim(0,1) +
    labs(x = 'Trial', y = 'Proportion of Risky Choices', fill = "Model")+
    theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),legend.text = element_text(size = 10),
          text = element_text(family = "Arial", size = 11), axis.title.y = element_text(family = "Arial", size = 13),
          axis.title.x = element_text(family = "Arial", size = 13), 
          axis.text.y.left = element_text(family = "Arial", size = 11),
          axis.text.x.bottom = element_text(family = "Arial", size = 11),
          strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(size = 11, family = "Arial"))+
    ggtitle(paste("Stimuli", unique(stimuli$nr)[i]))
  return(plot)
}

s1_opt_ph <- plotfunc(1)
s2_opt_ph <- plotfunc(2)
s3_opt_ph <- plotfunc(3)



# Shifting -------------------------------------------------------------------------
plotfunc1 <- function(i){
  plot <- ggplot(stimuli[nr == unique(nr)[i]] , 
                 aes(x = trial, y=prhv, fill = model))+
    theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 2, size = 0.3)+
    geom_point(size = 1.4, position = jitter, shape = 21) +
    scale_fill_manual(values=c("blue","red", "green"), name = "Model",labels = c("OPT softmax", "PH softmax", "Shifting"))+
    ylim(0,1) +
    labs(x = 'Trial', y = 'Proportion of Risky Choices', fill = "Model")+
    theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),legend.text = element_text(size = 10),
          text = element_text(family = "Arial", size = 11), axis.title.y = element_text(family = "Arial", size = 13),
          axis.title.x = element_text(family = "Arial", size = 13), 
          axis.text.y.left = element_text(family = "Arial", size = 11),
          axis.text.x.bottom = element_text(family = "Arial", size = 11),
          strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(size = 11, family = "Arial"),
          legend.position = c(0.2 , 0.83))+
    ggtitle(paste("Stimuli", unique(stimuli$nr)[i]))
  return(plot)
}


s1 <- plotfunc1(1)
s2 <- plotfunc1(2)
s3 <- plotfunc1(3)


# Nur Shifting ------------------------------------------------------------------------
plotfunc2 <- function(i){
  plot <- ggplot(stimuli[nr == unique(nr)[i] & model == "shift"] , 
                 aes(x = trial, y=prhv, fill = model))+
    theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 2, size = 0.3)+
    geom_point(size = 1.4, position = jitter, shape = 21) +
    scale_fill_manual(values=c("blue"), name = "Model",labels = c("Shifting"))+
    ylim(0,1) +
    labs(x = 'Trial', y = 'Proportion of Risky Choices', fill = "Model")+
    theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),legend.text = element_text(size = 10),
          text = element_text(family = "Arial", size = 11), axis.title.y = element_text(family = "Arial", size = 13),
          axis.title.x = element_text(family = "Arial", size = 13), 
          axis.text.y.left = element_text(family = "Arial", size = 11),
          axis.text.x.bottom = element_text(family = "Arial", size = 11),
          strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(size = 11, family = "Arial"))+
    ggtitle(paste("Stimuli", unique(stimuli$nr)[i]))
  return(plot)
}


shift1 <- plotfunc2(1)
shift2 <- plotfunc2(2)
shift3 <- plotfunc2(3)
