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

# In bearbeitung

plotfunc <- function(i){
  plot <- ggplot(stimuli[nr == unique(nr)[i] & model != "shift"] , 
                 aes(x = trial, y=prhv, fill = model))+
    theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 2, size = 0.5, alpha = 0.3)+
    geom_point(size = 3, position = jitter, shape = 21, alpha = 0.3, colour = "black") +
    scale_fill_manual(values=c("black","white"), name = "Model",labels = c("OPT softmax", "PH softmax"))+
    ylim(0,1) +
    labs(x = 'Trial', y = 'Proportion of Risky Choices', fill = "Model")+
    theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),legend.text = element_text(size = 10),
          text = element_text(family = "Arial", size = 11), axis.title.y = element_text(family = "Arial", size = 13),
          axis.title.x = element_text(family = "Arial", size = 13), 
          axis.text.y.left = element_text(family = "Arial", size = 11),
          axis.text.x.bottom = element_text(family = "Arial", size = 11),
          strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(size = 11, family = "Arial"))+
    ggtitle(
      paste0("Stimuli ", unique(stimuli$nr)[i], ". Risky: ", stimuli[nr == unique(stimuli$nr)[i], xh[1]],
             " (", stimuli[nr == unique(stimuli$nr)[i], pxh[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yh[1]],
             " /  Safe: ", stimuli[nr == unique(stimuli$nr)[i], xl[1]],
             " (", stimuli[nr == unique(stimuli$nr)[i], pxl[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yl[1]],". Budget: ",
             stimuli[nr == unique(stimuli$nr)[i], b[1]], ". Difficulty: ", stimuli[nr == unique(stimuli$nr)[i], dhbin[1]]))
  return(plot)
}

s2 <- stimuli[nr == 4207]
s2 <- s2[, median(prhv), by = c("model","trial")][model == "shift"]
s1_opt_ph <- plotfunc(1)
ggsave("../figures/temp_shifting_a.png",width = 8, height = 6)
s2_opt_ph <- plotfunc(2)
ggsave("../figures/temp_shifting_b.png",width = 8, height = 6)
s3_opt_ph <- plotfunc(3)
ggsave("../figures/temp_shifting_c.png",width = 8, height = 6)



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
          strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(size = 11, family = "Arial"))+
    ggtitle( paste0("Stimuli ", unique(stimuli$nr)[i], ". Risky: ", stimuli[nr == unique(stimuli$nr)[i], xh[1]],
                    " (", stimuli[nr == unique(stimuli$nr)[i], pxh[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yh[1]],
                    " /  Safe: ", stimuli[nr == unique(stimuli$nr)[i], xl[1]],
                    " (", stimuli[nr == unique(stimuli$nr)[i], pxl[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yl[1]],". Budget: ",
                    stimuli[nr == unique(stimuli$nr)[i], b[1]], ". Difficulty: ", stimuli[nr == unique(stimuli$nr)[i], dhbin[1]]))
  return(plot)
}


s1 <- plotfunc1(1)
ggsave("../figures/temp_shifting_a1.png",width = 8, height = 6)
s2 <- plotfunc1(2)
ggsave("../figures/temp_shifting_b1.png",width = 8, height = 6)
s3 <- plotfunc1(3)
ggsave("../figures/temp_shifting_c1.png",width = 8, height = 6)


# Nur Shifting ------------------------------------------------------------------------
plotfunc2 <- function(i){
  plot <- ggplot(stimuli[nr == unique(nr)[i] & model == "shift"] , 
                 aes(x = trial, y=prhv, fill = model))+
    theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 2, size = 0.3)+
    geom_point(size = 1.4, position = jitter, shape = 21) +
    scale_fill_manual(values=c("green"), name = "Model",labels = c("Shifting"))+
    ylim(0,1) +
    labs(x = 'Trial', y = 'Proportion of Risky Choices', fill = "Model")+
    theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),legend.text = element_text(size = 10),
          text = element_text(family = "Arial", size = 11), axis.title.y = element_text(family = "Arial", size = 13),
          axis.title.x = element_text(family = "Arial", size = 13), 
          axis.text.y.left = element_text(family = "Arial", size = 11),
          axis.text.x.bottom = element_text(family = "Arial", size = 11),
          strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(size = 11, family = "Arial"))+
    ggtitle(paste0("Stimuli ", unique(stimuli$nr)[i], ". Risky: ", stimuli[nr == unique(stimuli$nr)[i], xh[1]],
                   " (", stimuli[nr == unique(stimuli$nr)[i], pxh[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yh[1]],
                   " /  Safe: ", stimuli[nr == unique(stimuli$nr)[i], xl[1]],
                   " (", stimuli[nr == unique(stimuli$nr)[i], pxl[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yl[1]],". Budget: ",
                   stimuli[nr == unique(stimuli$nr)[i], b[1]], ". Difficulty: ", stimuli[nr == unique(stimuli$nr)[i], dhbin[1]]))
  return(plot)
}


shift1 <- plotfunc2(1)
ggsave("../figures/temp_shifting_a2.png",width = 8, height = 6)
shift2 <- plotfunc2(2)
ggsave("../figures/temp_shifting_b2.png",width = 8, height = 6)
shift3 <- plotfunc2(3)
ggsave("../figures/temp_shifting_c2.png",width = 8, height = 6)