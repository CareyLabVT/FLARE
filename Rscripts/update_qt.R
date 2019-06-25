update_qt <- function(resid30day, modeled_depths, qt, include_wq){
  
  old_qt <- as.matrix(qt[1:wq_end[2],1:wq_end[2]])
  index <- which(!is.na(resid30day[1, ]))
  resid <- resid30day[, index]

  Qt <- cov(resid)
  
  full_Qt <- array(NA,dim=c(nrow(old_qt),nrow(old_qt)))
  #full_Qt <- array(NA,dim=c(length(modeled_depths),length(modeled_depths)))
  for(i in 1:length(index)){
    full_Qt[index[i],index] <- Qt[i,]
  }
  
  full_Qt_diag <- diag(full_Qt)
  inter <- approxfun(modeled_depths,full_Qt_diag[1:length(modeled_depths)],rule = 2)
  full_Qt_diag[1:length(modeled_depths)] <- inter(modeled_depths)
  for(i in 1:(num_wq_vars)){
    if(length(which(!is.na(full_Qt_diag[wq_start[i]:wq_end[i]]))) > 0){
    inter <- approxfun(modeled_depths,full_Qt_diag[wq_start[i]:wq_end[i]],rule = 2)
    full_Qt_diag[wq_start[i]:wq_end[i]] <- inter(modeled_depths)
    }else{
      full_Qt_diag[wq_start[i]:wq_end[i]] <- diag(old_qt)[wq_start[i]:wq_end[i]]
    }
  }
    
    
  full_Qt_temp <- full_Qt
  #diag(full_Qt) <- rep(NA,length(full_Qt_diag))
  
  for(i in 1:nrow(old_qt)){
    if(!is.na(full_Qt_temp[i,i])){
      inter <- approxfun(modeled_depths,full_Qt[i,1:length(modeled_depths)],rule = 2)
      full_Qt[i,1:length(modeled_depths)] <- inter(modeled_depths)
      full_Qt[1:length(modeled_depths),i] <- inter(modeled_depths)
    }
      for(wq in 1:(num_wq_vars)){
        if(length(which(!is.na(full_Qt[i, wq_start[wq]:wq_end[wq]]))) > 0){
          inter <- approxfun(modeled_depths,full_Qt[i, wq_start[wq]:wq_end[wq]],rule = 2)
          full_Qt[i, wq_start[wq]:wq_end[wq]]  <- inter(modeled_depths)
          full_Qt[wq_start[wq]:wq_end[wq], i] <- inter(modeled_depths)
        }else{
          full_Qt[i, wq_start[wq]:wq_end[wq]] <- old_qt[i, wq_start[wq]:wq_end[wq]]
          full_Qt[wq_start[wq]:wq_end[wq], i] <- old_qt[wq_start[wq]:wq_end[wq],i]
        }
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
      for(j in 1:ndepths_modeled){
        qt <- rbind(qt, rep(0.0, ncol(qt)))
        qt <- cbind(qt, rep(0.0, nrow(qt)))
        qt[ncol(qt),nrow(qt)] <- old_qt[i*j,i*j]
      }
    }
  }
  
  return(qt)
}