# ==========================================================================
# Create Stimuli
# Model Predictions
# ==========================================================================

# Load Packages-------------------------------------------------------------
pacman::p_load(data.table)
library(cognitivemodels)
library(cognitiveutils)


# Source--------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Rsft Variables ------------------------------------------------------------
t = 5 # Trails
f <- function(state, budget) as.numeric(state >= budget) #Reward function


# Gambles --------------------------------------------------------------------

#define smallest acceptable variance difference between the risky and the safe option
mindiff_var <- 5 #minimal variance difference


# Table with all possible combinations of x and y and px
gamble_set <- expand.grid(x = 1:8, px = c(.1 ,.2, .25, .3, .4, .5, .6, .7, .75, .8, .9), y = 1:9)
gamble_set <- as.data.table(gamble_set)

# calculate py, EV and Var
gamble_set[,py := 1 - px]
gamble_set[,ev := x * px +y * py]
gamble_set[,var := round((x - ev)^2 * px + (y - ev)^2 * py,4)]

# remove duplicates
tab <- matrix(ncol = 2, nrow = nrow(gamble_set))
tab <- rbind(gamble_set$ev, gamble_set$var)
tab <- t(tab)
dup <- duplicated(tab)

gamble_set[,dup := dup]
gamble_set <- gamble_set[dup == FALSE]

#remove gambles with equal outcomes
gamble_set <- gamble_set[x != y]

# ID
gamble_set[,id := 1:nrow(gamble_set)]
gamble_set[,dup := NULL]



# Stimuli --------------------------------------------------------------------
# Pairs of gambles (risky Option vs. Safe Option) = stimuli
# create stimuli (gamble combinations), 
# where the two gambles have equal EVs and different variances (min difference defined above)

# all possible combinations
pairs <- matrix(ncol=2, nrow = nrow(t(combn(max(gamble_set$id),2))))
pairs[,1:2] <- t(combn(max(gamble_set$id), 2))
colnames(pairs) <- c("id1","id2")
pairs <- data.table(pairs)

rset1 <- gamble_set[id %in% pairs[,id1],.(id1 = id ,ev1 = ev, var1 = var, x1 = x, y1 = y,
                                          px1 = px, py1 = py)]
rset2 <- gamble_set[id %in% pairs[,id2],.(id2 = id ,ev2 = ev, var2 = var, x2 = x, y2 = y,
                                          px2 = px, py2 = py)]
pairs <- merge(pairs,rset1, by = "id1")
pairs <- merge(pairs, rset2, by = "id2")


# Delete combinations with different EVs and differences in variances < mindiff_var
pairs <- pairs[ev1 == ev2,]
pairs <- pairs[abs(var1 - var2) > mindiff_var,]

# ID
pairs[, id := 1:.N]

# Budget -------------------------------------------------------------
# Budget --> Amount of points you have to reach within t trials.

# Range budget
#b min = minimal outcome of the two options * t (trials)
#b max = maximal outcome of the two options * t (trials)
#t = 5 --> defined at the beginning
pairs[,bmin := min(x1,x2,y1,y2)*t,by=id]
pairs[,bmax := max(x1,x2,y1,y2)*t,by=id]


# Create table with rows = number of stimuli * Sum of all possible budgets
# number of rows
nor <- pairs[,.(l = length(bmin:bmax)), by = id][, sum(l)]
rps <- pairs[,.(l = length(bmin:bmax)), by = id]

budget_diff <- matrix(ncol = ncol(pairs)+1)

colnames(budget_diff) <- c(names(pairs),"b")
budget_diff <- as.data.frame(budget_diff)

z = 1
for(i in 1:nrow(rps)){
  j = 1
  p = 0
  while(j <= rps$l[i]){
    budget_diff[z,] <- pairs[i,]
    budget_diff[z,"b"] <- pairs[i,bmin + p]
    
    z = z + 1
    j = j + 1
    p = p + 1
  }
}

budget_diff <- as.data.table(budget_diff)
budget_diff[,]

budget_diff[,start := 0]


# order by variance -----------------------------------------------------------
budget_diff[, o <- ifelse(var1 > var2,1, 0)]
for(i in 1:nrow(budget_diff)){
  if(budget_diff$var1[i] > budget_diff$var2[i]){
    budget_diff[i, ':=' (varh = var1, varl = var2, xh = x1, yh = y1 , xl = x2,
                         yl = y2, pxh = px1, pyh = py1,
                         pxl = px2, pyl = py2, idh = id1, idl = id2)]
  } else {
    budget_diff[i,':=' (varh = var2, varl = var1, 
                        xh = x2, yh = y2, xl = x1,
                        yl = y1, pxh = px2, pyh = py2,
                        pxl = px1, pyl = py1, idh = id2, idl = id1)]
  }
}

budget_diff[,nr := 1:nrow(budget_diff)]

orderOutcomes <- function(budget_diff){
  for(i in 1:nrow(budget_diff)){
    if(budget_diff$xh[i] < budget_diff$yh[i]){
      v <- budget_diff$xh[i]
      budget_diff$xh[i] = budget_diff$yh[i]
      budget_diff$yh[i] = v
      p <- budget_diff$pxh[i]
      budget_diff$pxh[i] = budget_diff$pyh[i]
      budget_diff$pyh[i] = p
    } else { 
      budget_diff$xh[i] = budget_diff$xh[i]
      budget_diff$yh[i] =  budget_diff$yh[i] 
      budget_diff$pxh[i] = budget_diff$pxh[i] 
      budget_diff$pyh[i] = budget_diff$pyh[i]
    }
    if(budget_diff$xl[i] < budget_diff$yl[i]){
      v <- budget_diff$xl[i]
      budget_diff$xl[i] = budget_diff$yl[i]
      budget_diff$yl[i] = v
      p <- budget_diff$pxl[i]
      budget_diff$pxl[i] = budget_diff$pyl[i]
      budget_diff$pyl[i] = p
    } else { 
      budget_diff$xl[i] = budget_diff$xl[i]
      budget_diff$yl[i] =  budget_diff$yl[i] 
      budget_diff$pxl[i] = budget_diff$pxl[i] 
      budget_diff$pyl[i] = budget_diff$pyl[i]
    }
  }
  
  cols = c("xh", "yh", "xl", "yl")
  budget_diff[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  return(budget_diff)
}

sim = orderOutcomes(sim)



## Simulations =======================================================
choicerule = "softmax"
tau = 0.2

#Reward Function for rsft
R <- function(a, x_c) {
  ifelse(a >= x_c, 1, 0)
}

# 1. RSFT model -----------------------------------------
rsft_model <- hm1988(
  ~ xh + pxh + yh + pyh | xl + pxl  + yl + pyl,  # our formula (as before)
  trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
  data = budget_diff,         # our data (as before)
  budget = ~b,      # name of our budget column in our data
  initstate = ~start,    # name of our starting-state column in our data
  ntrials = 5,            # we always 5 trials therefore I hard-code this
  states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
  choicerule = choicerule,
  fix = list(tau = tau))

predict(rsft_model)

# From this rsft_model object I will not only simulate the predictions, but ...
#  I let the model tell me the trials and states that are possible in these trials
#  I also predict the probability of ending up in one state (prstate)

rsft_sim <- data.table(
  trial = 6 - rsft_model$get_timehorizons(), # get trials that are *remaining*
  state =   rsft_model$get_states(),         # get possible states
  prhv_rsft =    rsft_model$predict("response"),  # get pr(hv) prediction
  prstate = rsft_model$predict("pstate"), # get pr(state)
  hvalue = predict(rsft_model, type="values")[,1],
  lvalue = predict(rsft_model, type="values")[,2]
)

# Stimuli nr
rsft_sim[, nr := cumsum(trial == 1)]


sim <- merge(rsft_sim,budget_diff, by="nr")


## 2 Ph Heuristic ------------------------------------------------------------------

refpoint <- function(data){
  data[, r := (b - s)/ (..t - trial)]
  data[,':=' (rxh = (xh - r), ryh = (yh - r), rxl = (xl - r),
              ryl = (yl - r))]
  data[, ':=' (sxh = sign(rxh), syh = sign(ryh), sxl = sign(rxl),
               syl = sign(ryl))]
  return(data)
}

RefandProp <- function(sxh, syh, sxl, syl){
  if(sxh >= r & syh >= r & sxl >= r & syl >= r){
    valh = 0.5
    vall = 0.5
  } else if(sxh < r & sxl < r){
    valh = 0.5
    vall = 0.5
  } else if( sxl >= r & syl >= r & sxh >= r & syh < r){
    vall = 1
    valh = 0.5
  } else if( sxl >= r & syl < r & sxh >= r & syh < r){
    vall = pxl
    valh = pxh
  } else ( sxl < r & sxh >= r & syh < r){
    vall = 0
    valh = 0.5
  } else ( sxl >= r & syl < r & sxh < r){
    vall = 0.5
    valh = 0
  } else if( sxl >= r & syl < r & sxh >= r & syh >= r){
    vall = 0.5
    valh = 1
  }
  cbind( valh = valh, vall = vall)
}

sim[, lapply(SD,)]

Probheuristic <- function(data){
  high = max(data$xh, data$yh)
  low = max(data$xl, data$yl)
  data$probh <- ifelse(data$xh == high, data$pxh, data$pyh)
  data$probl <- ifelse(data$xl == low, data$pxl, data$pyl)
  
  sim_heurisik <- softmax(~ probh | probl,
                          d = data,
                          c(tau = 0.2))
  
  ph_sim <- data.table(
    probh_ph = data$probh,
    probl_ph = data$probl,
    prhv_ph = predict(sim_heurisik))
  
  return(ph_sim)
}

ph_sim <- Probheuristic(sim)

# Combine data
sim <- cbind(sim, ph_sim)


# 3. Shifting -----------------------------------------------------------------------------------
shifting <- shift_d( ~ prhv_ph + prhv_rsft,
                     time = ~trial,
                     data = sim,
                     fix = list(c = 3.5))

prhv_shift <- predict(shifting)
sim <- cbind(sim, prhv_shift)



# Difficulty -------------------------------------------------------------------------------
d <- sim[trial == 1, .(dh = hvalue, dl=lvalue), by = c("nr","b")]
sim <- merge(sim,d, by=c("nr","b"))


# bad rows ----------------------------------------------------------------------------------
sim[,badrows := ifelse(hvalue == lvalue,1,0)]




# delete colums -----------------------------------------------------------------------------
drops <- c("ev2","x1", "x2", "y1", "y2", "px1", "px2", "py1", "py2", "bmin", "bmax", "var1", "var2",
           "id1", "id2", "cdiff", "cdir", "c1", "prdir","prc1", "mdiff", "sumbr", "diff", "sumc1", "sumdir", "dir")
sim[, c(drops) := NULL]

sim <- sim[,list(nr,id,idh,idl,ev1,varh,varl,xh,yh,pxh,pyh,xl,yl,pxl,pyl,dh,dl,b,start,trial,state,hvalue,lvalue,prhv_rsft,prstate,
                 probh_ph, probl_ph,prhv_ph,prhv_shift, badrows)]

# Save --------------------------------------------------------------------------------------
fwrite(sim, "../stimuli/shifting_stimuli.csv")

