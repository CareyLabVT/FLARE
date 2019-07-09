update_qt <- function(resid30day, modeled_depths, qt, include_wq, num_wq_vars){
  
  old_qt <- qt
  
  temperature_resid <- resid30day[,1:length(modeled_depths)]
  index <- which(!is.na(temperature_resid[1,]))
  resid <- temperature_resid[, index]
  
  Qt <- cov(resid)
  
  full_Qt <- array(NA,dim=c(length(modeled_depths),length(modeled_depths)))
  for(i in 1:length(index)){
    full_Qt[index[i],index] <- Qt[i,]
  }
  
  full_Qt_diag <- diag(full_Qt)
  inter <- approxfun(modeled_depths,full_Qt_diag,rule = 2)
  full_Qt_diag <- inter(modeled_depths)
  full_Qt_temp <- full_Qt
  #diag(full_Qt) <- rep(NA,length(full_Qt_diag))
  
  for(i in 1:length(modeled_depths)){
    if(!is.na(full_Qt_temp[i,i])){
      inter <- approxfun(modeled_depths,full_Qt[i,],rule = 2)
      full_Qt[i,] <- inter(modeled_depths)
      full_Qt[,i] <- inter(modeled_depths)
    }
  }
  for(i in 1:length(modeled_depths)){
    inter <- approxfun(modeled_depths,full_Qt[,i],rule = 2)
    full_Qt[,i] <- inter(modeled_depths)
  }
  
  diag(full_Qt) <- full_Qt_diag
  
  qt <- full_Qt
  
  if(include_wq){
    for(i in 1:num_wq_vars){
      for(j in 1:length(modeled_depths)){
        qt <- rbind(qt, rep(0.0, ncol(qt)))
        qt <- cbind(qt, rep(0.0, nrow(qt)))
        qt[ncol(qt),nrow(qt)] <- old_qt[i*j,i*j]
      }
    }
  }
  
  return(qt)
}