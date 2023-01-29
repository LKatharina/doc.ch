rm(list=ls(all=TRUE))
library(data.table)
library(extrafont)
#library(showtext)
library(stringr)
# font_import(pattern = "unicode")
# loadfonts(device = "win")
# fonttable() # lists the fonts that r knows currently

# THE BELOW IS JUST IF THE FONT READGING DOES NOT WORK
#extrafont::fonttable()
# fntbl <- fread(system.file("fontmap", "fonttable.csv", package="extrafontdb"))
# fntbl[FullName == 'Roboto Slab Thin', FamilyName := 'Roboto Slab Thin']
#fwrite(fntbl, system.file("fontmap", "fonttable.csv", package="extrafontdb"))
library(ggplot2)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#How to label the options ("special" for one letter or one special character, "two" for two letters)
labelmethode = "special"
# Load stimuli ----------------------------------------------------------------
# Names of columns we would like to drop
drop_these_columns <- c("budget", "state","trial","start")

# Load stimuli drop cols_to_drop
d <- rbindlist(lapply(list.files('stimuli', full = TRUE), fread, drop = drop_these_columns, fill = TRUE))


# Label the options

#two: each option gets a label consisting of two letters
if(labelmethode == "two"){
  let = as.data.table(t(combn(letters,2)))
  lbl = let[,lbl := paste0(V1,V2)]$lbl
  o2_label = o1_label = NULL
  
  for(i in 1:nrow(d)){
    o1 = sample(lbl,1)
    lbl = lbl[lbl != as.character(o1)]
    templbl = lbl[str_detect(lbl,substr(o1,1,1), negate = T)]
    templbl = templbl[str_detect(templbl,substr(o1,2,2), negate = T)]
    o2 = sample(templbl,1)
    lbl = lbl[lbl != as.character(o2)]
    o1_label = c(o1_label,o1)
    o2_label = c(o2_label,o2)
  }
  
  d[,':='(labelHV = o1_label, labelLV = o2_label)]
  d = d[,.(x1HV, x2HV, p1HV, p2HV, labelHV, x1LV, x2LV, p1LV, p2LV, labelLV)]
}


# Labels consist of one letter or one special character (@, #, *, &)
sp_car = c("@","#","*", "&")
if(labelmethode == "special"){
  let = c(letters)
  opt_labels = sample(let,(2*nrow(d)-length(sp_car)))
  opt_labels = c(opt_labels,sp_car)
}


make_sprites <- function(d = d) {
  d1 <- d[,1:(1/2*ncol(d)),]
  d2 <- d[, (1/2*ncol(d)+1):ncol(d),]
  setnames(d2, names(d1))
  d <- rbind(d1,d2, fill = T)
  if(labelmethode == "special"){d[,opt_label := opt_labels]}
  d <- unique(d)

  for (i in 1:nrow(d)) {
    dd <- d[i]
    dd[, id := 1:.N]
    dd <- melt(dd, id = 'id', measure = list(1:2, 3:4, 5), value = c('x','p',"opt_label"))
    dd[ ,opt_label := opt_label[1]]
    dd[, variablef := factor(x, levels = x,  abs(x))] #labels = paste0(ifelse(sign(x) == 1, "+", "-"), include this in factor(x,levels = x ...)labels = paste0(ifelse(sign(x) == 1, "+", "-"), to get "-" and "+" signs
    dd[, variablef2 := reorder(variablef, 2:1)]
    plot_and_save(dd, 'variablef2', 1:2)
    plot_and_save(dd, 'variablef2', 2:1)
  }
}

plot_and_save <- function(dd, v, colorder) {
  font_name <- "Arial"
  cols <- c('white', 'white')
  cols <- cols[colorder]
  leg.l.margin <- -0.1 # margin from the left side of the plot to the point
  leg.txt.r.margin <- -2 # margin at the right side of the text (outcome)
  leg.txt.l.margin <- -3.5 # margin at the left side of the text (between point and outcome)
  v <- "variablef2"
  theplot <- ggplot() +
    theme_void(base_family = font_name, base_size = 20) +
    annotate("text", x=50, y=50, label= dd$opt_label[1], size = 3)
    
  fn <- dd[order(variablef), paste(x[1], sprintf("%.0f", p[1] * 100), x[2], collapse='_', sep='_')]
  fn <- paste0('sprite_', fn, '_featurecolor', paste0(colorder-1, collapse = ''), '.png')
  print(fn)
  ggsave(plot = theplot, filename = file.path('sprites_dfe', fn), width = 0.5, height = 0.5/1.6, units='in', dpi = 600)
}

make_sprites(d = d)
warnings()

##
# Combine the individual images to one sprite.css image
# which is one img that contains all imgs on a grid
# and one associated .css file that refers to the locations on the grid
# --------------------------------------------------------------------------
# 1. Downloade imagemagic for your OS: https://imagemagick.org/script/download.php
# 2. Run the code in "installation" at https://github.com/krzysztof-o/spritesheet.js/
# Download Texture packer https://www.codeandweb.com/texturepacker/tutorials/how-to-create-a-sprite-sheet
system("rm sprites.png sprites.css")
system("glue sprites . --sprite-namespace= --namespace=")

