# Packages
pacman::p_load(data.table, ggplot2)
library(cognitivemodels)


windowsFonts(Arial=windowsFont("Arial"))

# Read Data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("2-shifting-select.R")
source("3-shifting-plot.R")
source("../../models/rsft1988.R") # rsft model
source("../../models/softmax.R")
source("../../models/rsft1988-probstates.R")
