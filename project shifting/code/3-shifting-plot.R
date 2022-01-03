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
stimuli[, model := as.factor(model)]


# # Median Shifting + PH & OPT Vorhersagen ----------------------------------------------------
plotfunc <- function(i){
  
  agg <- stimuli[nr == unique(nr)[i], .(median = median(prhv)), by = c("model","trial")]
  
  plot <- ggplot(stimuli[nr == unique(nr)[i] & model != "shift"] , 
                 aes(x = trial, y=prhv, shape = model))+
    theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 2, size = 0.5, alpha = 0.1)+
    geom_point(size = 3, position = jitter, alpha = 0.5, show.legend = FALSE) +
    scale_shape_manual(values=c(1,2,3),name = "Model",labels = c("Probability Heuristic", "Optimal Model", "Shifting Model"))+ # Irgendwie verdreht (????)
    scale_colour_manual(values=c("black","green")) +
    #scale_fill_manual(values=c("grey","black"), name = "Model",labels = c("OPT softmax", "PH softmax"))+
    ylim(0,1) +
    labs(x = 'Trial', y = 'Proportion of Risky Choices')+
    geom_point(agg[model == "shift"], mapping = aes(x = trial, y = median, shape = model), colour = "blue", inherit.aes = F, size = 4, stroke = 2)+
    geom_line(agg[model == "shift"], mapping = aes(x = trial, y = median, group = 1), linetype = 2, inherit.aes = F, colour = "blue", size = 0.8, alpha = 0.5)+
    theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),legend.text = element_text(size = 10),
          text = element_text(family = "Arial", size = 11), axis.title.y = element_text(family = "Arial", size = 13),
          axis.title.x = element_text(family = "Arial", size = 13), 
          axis.text.y.left = element_text(family = "Arial", size = 11),
          axis.text.x.bottom = element_text(family = "Arial", size = 11),
          strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(size = 11, family = "Arial"))+
    guides(shape = guide_legend(override.aes = list(alpha = 1, colour = "black", stroke = 1))
           #fill = guide_legend(override.aes = list(alpha = 1)),
           ) +
     ggtitle(
      paste0("Stimuli ", unique(stimuli$nr)[i], ". Risky: ", stimuli[nr == unique(stimuli$nr)[i], xh[1]],
             " (", stimuli[nr == unique(stimuli$nr)[i], pxh[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yh[1]],
             " /  Safe: ", stimuli[nr == unique(stimuli$nr)[i], xl[1]],
             " (", stimuli[nr == unique(stimuli$nr)[i], pxl[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yl[1]],". Budget: ",
             stimuli[nr == unique(stimuli$nr)[i], b[1]], ". Difficulty: ", stimuli[nr == unique(stimuli$nr)[i], dhbin[1]]))
  print(plot)
  return(plot)
}

#

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
  
  agg <- stimuli[nr == unique(nr)[i], .(median = median(prhv)), by = c("model","trial")]

  plot <- ggplot(stimuli[nr == unique(nr)[i] & model != "shift"] , 
                 aes(x = trial, y=prhv, shape = model))+
    theme_classic() +
    geom_point(size = 3, position = jitter, alpha = 0.5, show.legend = FALSE, colour="black")  +
    geom_point(agg[model == "shift"], mapping = aes(x = trial, y = median, shape = model), colour = "blue", inherit.aes = F, size = 4, stroke = 2)+
    geom_line(agg[model == "shift"], mapping = aes(x = trial, y = median, group = interaction(model), colour = model), 
              linetype = 2, inherit.aes = F, size = 0.8, alpha = 0.5, colour = "blue") +
    geom_point(agg[model != "shift"], mapping = aes(x = trial, y = median, shape = model), colour = "black", inherit.aes = F)+
    geom_line(agg[model != "shift"], mapping = aes(x = trial, y = median, group = interaction(model)), 
              linetype = 2, inherit.aes = F, size = 0.1, alpha = 0.8, colour = "grey") +
    scale_shape_manual(values=c(1,2,3,4,8),name = "Model",labels = c("Optimal Model", "Probability Heuristic", "Shifting Model")) +
    scale_colour_manual(values=c("black","black", "blue"),name = "Model",labels = c("Optimal Model", "Probability Heuristic", "Shifting Model")) +
    #scale_fill_manual(values=c("grey","black"), name = "Model",labels = c("OPT softmax", "PH softmax"))+
    ylim(0,1) +
    labs(x = 'Trial', y = 'Proportion of Risky Choices')+
    theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),legend.text = element_text(size = 10),
          text = element_text(family = "Arial", size = 11), axis.title.y = element_text(family = "Arial", size = 13),
          axis.title.x = element_text(family = "Arial", size = 13), 
          axis.text.y.left = element_text(family = "Arial", size = 11),
          axis.text.x.bottom = element_text(family = "Arial", size = 11),
          strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(size = 11, family = "Arial"))+
    guides(shape = guide_legend(override.aes = list(alpha = 1, colour = "black", stroke = 1)),
                                colour = guide_legend(override.aes = list(alpha = 1, colour = "black", stroke = 1))
           #fill = guide_legend(override.aes = list(alpha = 1)),
    ) +
    ggtitle(
      paste0("Stimuli ", unique(stimuli$nr)[i], ". Risky: ", stimuli[nr == unique(stimuli$nr)[i], xh[1]],
             " (", stimuli[nr == unique(stimuli$nr)[i], pxh[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yh[1]],
             " /  Safe: ", stimuli[nr == unique(stimuli$nr)[i], xl[1]],
             " (", stimuli[nr == unique(stimuli$nr)[i], pxl[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yl[1]],". Budget: ",
             stimuli[nr == unique(stimuli$nr)[i], b[1]], ". Difficulty: ", stimuli[nr == unique(stimuli$nr)[i], dhbin[1]]))
  print(plot)
  return(plot)
}

#

s1_opt_ph <- plotfunc2(1)

ggsave("../figures/temp_shifting_a.png",width = 8, height = 6)
s2_opt_ph <- plotfunc(2)
ggsave("../figures/temp_shifting_b.png",width = 8, height = 6)
s3_opt_ph <- plotfunc(3)
ggsave("../figures/temp_shifting_c.png",width = 8, height = 6)

s4_opt_ph <- plotfunc(4)
s4_opt_ph <- plotfunc(5)


# plotfunc2 <- function(i){
#   plot <- ggplot(stimuli[nr == unique(nr)[i] & model == "shift"] , 
#                  aes(x = trial, y=prhv, fill = model))+
#     theme_classic() +
#     geom_hline(yintercept = 0.5, linetype = 2, size = 0.3)+
#     geom_point(size = 1.4, position = jitter, shape = 21) +
#     scale_fill_manual(values=c("green"), name = "Model",labels = c("Shifting"))+
#     ylim(0,1) +
#     labs(x = 'Trial', y = 'Proportion of Risky Choices', fill = "Model")+
#     theme(legend.background = element_rect(linetype = 1, size = 0.01, colour = "black"),legend.text = element_text(size = 10),
#           text = element_text(family = "Arial", size = 11), axis.title.y = element_text(family = "Arial", size = 13),
#           axis.title.x = element_text(family = "Arial", size = 13), 
#           axis.text.y.left = element_text(family = "Arial", size = 11),
#           axis.text.x.bottom = element_text(family = "Arial", size = 11),
#           strip.background = element_rect(colour="black", size = 0.01),strip.text = element_text(size = 11, family = "Arial"))+
#     ggtitle(paste0("Stimuli ", unique(stimuli$nr)[i], ". Risky: ", stimuli[nr == unique(stimuli$nr)[i], xh[1]],
#                    " (", stimuli[nr == unique(stimuli$nr)[i], pxh[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yh[1]],
#                    " /  Safe: ", stimuli[nr == unique(stimuli$nr)[i], xl[1]],
#                    " (", stimuli[nr == unique(stimuli$nr)[i], pxl[1]], "); ", stimuli[nr == unique(stimuli$nr)[i], yl[1]],". Budget: ",
#                    stimuli[nr == unique(stimuli$nr)[i], b[1]], ". Difficulty: ", stimuli[nr == unique(stimuli$nr)[i], dhbin[1]]))
#   return(plot)
# }
# 
# 
# shift1 <- plotfunc2(1)
# ggsave("../figures/temp_shifting_a2.png",width = 8, height = 6)
# shift2 <- plotfunc2(2)
# ggsave("../figures/temp_shifting_b2.png",width = 8, height = 6)
# shift3 <- plotfunc2(3)
# ggsave("../figures/temp_shifting_c2.png",width = 8, height = 6)