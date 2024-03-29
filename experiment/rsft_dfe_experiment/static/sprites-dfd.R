rm(list=ls(all=TRUE))
library(data.table)
library(extrafont)
#font_import(pattern = "arial")
#loadfonts(device = "win")
#fonttable() # lists the fonts that r knows currently

# THE BELOW IS JUST IF THE FONT READGING DOES NOT WORK
#extrafont::fonttable()
# fntbl <- fread(system.file("fontmap", "fonttable.csv", package="extrafontdb"))
# fntbl[FullName == 'Roboto Slab Thin', FamilyName := 'Roboto Slab Thin']
#fwrite(fntbl, system.file("fontmap", "fonttable.csv", package="extrafontdb"))
library(ggplot2)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load stimuli ----------------------------------------------------------------

# Names of columns we would like to drop
drop_these_columns <- c("budget", "state", "start", "trial")
# Load stimuli drop cols_to_drop
d <- rbindlist(lapply(list.files('stimuli', full = TRUE), fread, drop = drop_these_columns, fill = TRUE))



make_sprites <- function(d = d) {
  d1 <- d[,1:(1/2*ncol(d)),]
  d2 <- d[, (1/2*ncol(d)+1):ncol(d),]
  setnames(d2, names(d1))
  d <- rbind(d1,d2, fill = T)
  d <- unique(d)

  for (i in 1:nrow(d)) {
    dd <- d[i]
    dd[, id := 1:.N]
    dd <- melt(dd, id = 'id', measure = list(1:2, 3:4), value = c('x','p'))
    dd[, variablef := factor(x, levels = x,  abs(x))] #labels = paste0(ifelse(sign(x) == 1, "+", "-"), include this in factor(x,levels = x ...)labels = paste0(ifelse(sign(x) == 1, "+", "-"), to get "-" and "+" signs
    dd[, variablef2 := reorder(variablef, 2:1)]
    plot_and_save(dd, 'variablef2', 1:2)
    plot_and_save(dd, 'variablef2', 2:1)
  }
}


plot_and_save <- function(dd, v, colorder) {
  font_name <- "Arial"
  cols <- c('grey89', 'grey68')
  cols <- cols[colorder]
  leg.l.margin <- -0.1 # margin from the left side of the plot to the point
  leg.txt.r.margin <- -2 # margin at the right side of the text (outcome)
  leg.txt.l.margin <- -3.5 # margin at the left side of the text (betwen point and outcome)
  v <- "variablef2"
    theplot <- ggplot(dd, aes_string(x = 0, fill = v, color = v, group = v)) +
      geom_bar(aes(y = p), stat = 'identity', width = .2, color = NA) +
      theme_void(base_family = font_name, base_size = 20)+
      coord_flip() +
      xlim(-.2,1) +
      geom_point(aes(x = -100, y = p), size = .5) +
      theme(
        legend.direction = 'horizontal',
        legend.position = c(0,0.5),
        legend.justification = c(0, 0),
        legend.key.width = unit(.2, 'lines'),
        legend.key.height = unit(.01, 'lines'),
        legend.title = element_blank(),
        legend.margin = margin(l = leg.l.margin, r = .01, unit = 'lines'),
        # the point and the outcome (legend of the bar)
        legend.text = element_text(
          size = 3,
          margin = margin(r = leg.txt.r.margin, l = leg.txt.l.margin, unit = "lines")),
        aspect.ratio = 1/1.6,
        plot.margin = margin(0)) +
      scale_fill_manual(values = cols) +
      scale_color_manual(values = cols) +
      guides(fill = 'none', color = guide_legend(reverse = TRUE,
        override.aes = list(shape = 16))) +
      # the text in the bar
      geom_text(
        aes(y = p, label = percent(p, accuracy = 1)),
        position = position_stack(vjust = .5),
        size = .65,
        family = font_name,
        color = 'black')
  
  fn <- dd[order(variablef), paste(x[1], sprintf("%.0f", p[1] * 100), x[2], collapse='_', sep='_')]
  fn <- paste0('sprite_', fn, '_featurecolor', paste0(colorder-1, collapse = ''), '.png')
  print(fn)
  ggsave(plot = theplot, filename = file.path('sprites_dfd', fn), width = 0.5, height = 0.5/1.6, units='in', dpi = 600)
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

paste('sprites', format, sep = "_")
