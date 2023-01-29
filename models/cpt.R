#=========================================================================
# Cumulative Prospect Theory (Kahnemann & Tversky, 1992)
#
#=========================================================================

TK1992_value <- function(alpha, beta,lambda, x){
  v1a <- ifelse(x > 0, x^alpha, -lambda*abs(x)^beta)
  return(v1a)
}

# Weighting function ----------------------------------------------------

TK1992_weighting = function( x1, x2, px1, px2, gammag, gammal ){
  x1_original = x1
  x2_original = x2
  px1_original = px1
  px2_original = px2
  switched = FALSE
  
  # Order Outcomes
  if(x1 > x2){
    temp = x1
    x1 = x2
    x2 = temp
    temp = px1
    px1 = px2
    px2 = temp
    switched = TRUE
  }
  
  
  if( x1 >= 0 & x2 >= 0 ){
    # Probability Weighting gains
    w2 = (px2^gammag) / (px2^gammag + (1 - px2)^gammag)^(1 / gammag)
    w1 = ((px2 + px1)^gammag) / ((px2 + px1)^gammag + (1 - (px2 + px1))^gammag )^(1 / gammag) - w2
    
  } else if(x1 <= 0 & x2 <= 0){
    # Probability Weighting losses
    w1 = (px1^gammal) / (px1^gammal + (1 - px1)^gammal)^(1 / gammal)
    w2 = ((px2 + px1)^gammal)/ ((px2 + px1)^gammal + (1 - (px2 + px1))^gammal )^(1 / gammal) - w1
    
  } else if(x2 < 0 & x1 > 0 ){
    # mixed
    w1 = (px1^gammag) / (px1^gammag + (1 - px1)^gammag)^(1 / gammag)
    w2 = (px2^gammal) / (px2^gammal + (1 - px2)^gammal)^(1 / gammal)
    
  } else {
    w1 = (px2^gammal) / (px2^gammal + (1 - px2)^gammal)^(1 / gammal)
    w2 = (px1^gammag) / (px1^gammag + (1 - px1)^gammag)^(1 / gammag)
  }
  
  # original order
  if(switched == TRUE){
    temp = w2
    w2 = w1
    w1 = temp
  }
  
  return(data.frame(px1_original, px2_original, w1, w2, x1 = x1_original, x2 = x2_original))
} 

cpt <- function(xh,yh,xl,yl, pxh, pyh, pxl, pyl,
                ref,
                fix = list(),
                fweighting,
                fvalue,
                choicerule){
  
  alpha = fix$alpha
  beta = fix$beta
  lambda = fix$lambda
  gammag = fix$gammag
  gammal = fix$gammal
  tau = fix$tau
  
  
  # reference point
  xh = xh - ref
  yh = yh - ref
  xl = xl - ref
  yl = yl - ref
  
  # Values
  v_xh <- fvalue(alpha, beta, lambda,  xh)
  v_yh <- fvalue(alpha, beta, lambda,  yh)
  v_xl <- fvalue(alpha, beta, lambda,  xl)
  v_yl <- fvalue(alpha, beta, lambda,  yl)

  # Weighting
  weights_hv <- fweighting(xh, yh, pxh, pyh, gammag, gammal)
  weights_lv <- fweighting(xl, yl, pxl, pyl, gammag, gammal)


# Utilities
  uhv = v_xh *  weights_hv$w1 + v_yh * weights_hv$w2
  ulv = v_xl *  weights_lv$w1 + v_yl * weights_lv$w2
  
  
# choice rule
  predictions = as.data.table(choicerule(data.table(prhv = uhv, prlv = ulv),tau))
  
  return(data.table(prhv = predictions$prhv,uhv,ulv))
}

cpt(2,0,1,0,0.5,0.5,1,0,0,fix = list(alpha = 1, beta = 1, lambda = 1, gammag = 1, gammal = 1, tau = 0.2),TK1992_weighting, TK1992_value, cr_softmax)
