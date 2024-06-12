# ==============================================================================
# Function to create data set with all possible combinations of design stimuli
# ==============================================================================

Get_Stimuli <- function(rewards_x, rewards_y, probs_x, min_var, ntrials) {
  
  # Rsft Variables ------------------------------------------------------------
  t = ntrials # Trails
  
  # Gambles --------------------------------------------------------------------
  
  #define smallest acceptable variance difference between the risky and the safe option
  mindiff_var <- min_var #minimal variance difference
  
  # Table with all possible combinations of x and y and px
  gamble_set <- as.data.table(expand.grid(x = rewards_x, px = probs_x, y = rewards_y))

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
  pairs <- merge(pairs, rset1, by = "id1")
  pairs <- merge(pairs, rset2, by = "id2")
  
  
  # Delete combinations with different EVs and differences in variances < mindiff_var
  pairs <- pairs[ev1 == ev2,]
  pairs <- pairs[abs(var1 - var2) > mindiff_var,]
  
  # ID
  pairs[, id := .I]
  
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
  
  
  # order by variance-----------------------------------------------------------
  for(i in 1:nrow(budget_diff)){
    if(budget_diff$var1[i] > budget_diff$var2[i]){
      budget_diff[i, `:=` (varh = var1, varl = var2, 
                           r1 = x1, r2 = y1, s1 = x2, s2 = y2, 
                           pr1 = px1, pr2 = py1, ps1 = px2, ps2 = py2, 
                           idh = id1, idl = id2)]
    } else {
      budget_diff[i, `:=` (varh = var2, varl = var1, 
                           r1 = x2, r2 = y2, s1 = x1, s2 = y1, 
                           pr1 = px2, pr2 = py2, ps1 = px1, ps2 = py1, 
                           idh = id2, idl = id1)]
    }
  }
  
  # Design ID
  budget_diff[, design_id := .I]
  
  stimuli <- budget_diff[, .(ps1, s1, s2, pr1, r1, r2, budget = b, design_id)]
  
  return(stimuli)
}

