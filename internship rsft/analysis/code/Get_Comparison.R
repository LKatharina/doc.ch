
Get_Comparison <- function(df) {
  
  mean_prhv <- d2[, .(prhv_rsft = mean(prhv_rsft)), 
                  by = .(dfe_id, trial)]
  
  dfe <- unique(mean_prhv$dfe_id)
  dfe_comp <- mean_prhv[, .(trial = c("1", "2", "3"),
                            absolute_dif = c(prhv_rsft[trial == 1 & dfe_id == dfe[2]] - prhv_rsft[trial == 1 & dfe_id == dfe[1]],
                                             prhv_rsft[trial == 2 & dfe_id == dfe[2]] - prhv_rsft[trial == 2 & dfe_id == dfe[1]],
                                             prhv_rsft[trial == 3 & dfe_id == dfe[2]] - prhv_rsft[trial == 3 & dfe_id == dfe[1]]),
                            percentage_dif = c((1 / prhv_rsft[trial == 1 & dfe_id == dfe[1]] 
                                                * prhv_rsft[trial == 1 & dfe_id == dfe[2]] - 1) * 100,
                                               (1 / prhv_rsft[trial == 2 & dfe_id == dfe[1]] 
                                                * prhv_rsft[trial == 2 & dfe_id == dfe[2]] - 1) * 100,
                                               (1 / prhv_rsft[trial == 3 & dfe_id == dfe[1]] 
                                                * prhv_rsft[trial == 3 & dfe_id == dfe[2]] - 1) * 100),
                            significant_dif = c(ifelse(test = sign(prhv_rsft[trial == 1 & dfe_id == dfe[1]] - 0.5) != 
                                                         sign(prhv_rsft[trial == 1 & dfe_id == dfe[2]] - 0.5),
                                                       yes = "YES",
                                                       no = "NO"),
                                                ifelse(test = sign(prhv_rsft[trial == 2 & dfe_id == dfe[1]] - 0.5) != 
                                                         sign(prhv_rsft[trial == 2 & dfe_id == dfe[2]] - 0.5),
                                                       yes = "YES",
                                                       no = "NO"),
                                                ifelse(test = sign(prhv_rsft[trial == 3 & dfe_id == dfe[1]] - 0.5) != 
                                                         sign(prhv_rsft[trial == 3 & dfe_id == dfe[2]] - 0.5),
                                                       yes = "YES",
                                                       no = "NO")))]
  return(dfe_comp)
}
