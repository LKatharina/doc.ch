# ============================================
# Shifting Model
# 
# ============================================


shift = function(xh,yh,xl,yl, pxh, pyh, pxl, pyl, goal, timeHorizon, start, gstates, gtrials, model1, model2, t, c){
  if(t <= c){
    choice = model1
  } else {
    choice = model2
  }
  return(choice)
}