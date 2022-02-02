########################################
#
# EV and Variances
#
#########################################

ExpectedValue = function(v, p){
  vp = v * p
  return(sum(vp))
}

Variances = function(v, p){
  vp = v * p
  ev = sum(vp)
  ev = rep(ev,length(v))
  tv = (v-ev)^2 * p
  return(sum(tv))
}

# ExpectedValue(c(3,0),c(0.25,0.75))
# ExpectedValue(c(32,0),c(0.025,0.975))
# Variances(c(3,0),c(0.25,0.75))
# Variances(c(32,0),c(0.025,0.975))
