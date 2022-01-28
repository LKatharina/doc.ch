###########################################################
# Softmax Choice Rule
#
#
############################################################


softmax = function(v1,values,tau){
  sum = 0
  for(i in values){
    tv = exp(tau*i)
    sum = sum + tv
  }
  p = exp(tau*v1) / sum
  return(p)
}


cr_softmax <-function(x, tau) {
  x <- as.matrix(x)
  if ( ncol(x) == 1 ) {
    return(cr_softmax(cbind(x, 1-x), tau)[,1])
  }
  x[x==0] <- .0000001 # ensure no NAs
  # For numerical stability, normalize to avoid instabilities due to large exponentials, see http://cs231n.github.io/linear-classify/#softmax
  s <- apply(x, 1, max) # shift (exp is shift invariant)
  y <- exp(((x - s)/tau))
  return( y / rowSums(y) )
}