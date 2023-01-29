#================================================
# Probability Heuristic (Korn & Bach 2018)
#
#================================================


Probheuristic <- function(xh,yh,xl,yl, pxh, pyh, pxl, pyl, choicerule, tau){
  probh <- ifelse(xh > yh, pxh, pyh)
  probl <- ifelse(xl > yl, pxl, pyl)
  
  if(choicerule == "softmax"){
    predictions = as.data.table(cr_softmax(probh, probl ,tau))
  } else {
    
  }
  return(data.table(probh,probl))
}
  

