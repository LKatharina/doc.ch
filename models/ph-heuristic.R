#================================================
# Probability Heuristic (Korn & Bach 2018)
#
#================================================


Probheuristic <- function(xh,yh,xl,yl, pxh, pyh, pxl, pyl){
  probh <- ifelse(xh > yh, pxh, pyh)
  probl <- ifelse(xl > yl, pxl, pyl)
  return(data.table(probh,probl))
}
  

