#============================================================================
# Houston and McNamara 1988: RISK SENSITIVE FORAGING MODEL
# Reward Functions
#============================================================================

# Step reward function ------------------------------------------------------
step = function(goal,state) { ifelse(state >= goal, 1, 0) }

# EV maximizing reward function ---------------------------------------------
evmax = function(goal,state){
  state
}

# smooth reward function ----------------------------------------------------
logistic = function(goal,state,k){
  y = 1/(1+exp(-k*(state-goal)))
  return(y)
}