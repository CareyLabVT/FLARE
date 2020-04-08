update_sigma <- function(qt, p_t, h, x_star, pars_star, x_corr, pars_corr, psi_t, zt, npars, qt_pars, include_pars_in_qt_update, nstates, qt_alpha){
  
  #From:
  #Rastetter, E. B., M. Williams, K. L. Griffin, B. L. Kwiatkowski, 
  #G. Tomasky, M. J. Potosnak, P. C. Stoy, G. R. Shaver, M. Stieglitz, 
  #J. E. Hobbie, and G. W. Kling. 2010. Processing arctic eddy-flux 
  #data using a simple carbon-exchange model embedded in the ensemble 
  #Kalman filter. Ecological Applications 20:1285-1301.
  
  old_qt <- qt
  if(npars > 0){
    x_par_star <- cbind(x_star,pars_star)
    x_par_corr <- cbind(x_corr,pars_corr)
    if(!include_pars_in_qt_update){
      x_par_star <- x_star
      x_par_corr <- x_corr
      p_t <- p_t[1:nstates,1:nstates]
      h <- h[, 1:nstates]
      old_qt <- old_qt[1:nstates,1:nstates]
    }
  }else{
    x_par_star <- x_star
    x_par_corr <- x_corr
  }
  
  gamma <- t((1 - qt_beta) *  solve(h %*% p_t %*% t(h)) %*% h %*% p_t %*% (diag(1 , nrow = dim(h)[2], ncol = dim(h)[2]) - t(h) %*% h) + qt_beta * h)
  
  ens_mean_star <- colMeans(x_par_star)
  
  nmembers <- nrow(x_par_star)
  nstates <- ncol(x_par_star)
  
  dit_star <- matrix(NA, nrow = nmembers, ncol = nstates)
  #Loop through ensemble members
  for(m in 1:nmembers){  
    #Ensemble specific deviation
    dit_star[m, ] <- x_par_star[m, ] - ens_mean_star
    
    #if the first ensemble then create the matrix that is then averaged
    if(m == 1){
      p_it_star <- dit_star[m, ] %*% t(dit_star[m, ]) 
    }else{
      #if not the first ensemble then add the matrix to the previous matrix
      p_it_star <- dit_star[m, ] %*% t(dit_star[m, ]) +  p_it_star 
    }
  }
  
  p_t_star <- p_it_star / (nmembers - 1)
  
  y_corr <- matrix(NA, nrow =  nmembers, ncol = length(zt))
  for(m in 1:nmembers){
    if(length(psi_t) > 1){
      y_corr[m, ] <- zt + t(rmvnorm(n = 1, mean = rep(0,dim(psi_t)[1]), sigma = psi_t))
    }else{
      y_corr[m, ] <- zt + t(rnorm(n = 1, mean = 0, sd = sqrt(psi_t)))
    }
    y_tmp <- y_corr[m,] - h %*% x_par_corr[m, ]
    if(m == 1){
      y_it <- y_tmp %*% t(y_tmp)
    }else{
      y_it <- y_tmp %*% t(y_tmp) + y_it
    }
  }
  
  St <-  y_it / (nmembers - 1)
  
  q_star <- gamma %*% (St - h %*% p_t_star %*% t(h) - psi_t) %*% t(gamma)
  
  
  qt <- (qt_alpha * old_qt) + ((1 - qt_alpha) *  q_star)
  if(npars > 0 & !include_pars_in_qt_update){
    for(pars in 1:npars){
      qt <- rbind(qt, rep(0.0, ncol(qt)))
      qt <- cbind(qt, rep(0.0, nrow(qt)))
      qt[ncol(qt),nrow(qt)] <- diag(qt_pars)[pars]
    }
  }
  
  return(qt)
}

update_qt <- function(resid30day, modeled_depths, qt, include_wq, npars, nstates, wq_start, wq_end, num_wq_vars){
  
  old_qt <- qt
  
  ndepths_modeled <- length(modeled_depths)
  
  if(use_cov){
    tmp_resid <- resid30day[, 1:ndepths_modeled]
    index <- NULL
    for(i in 1:ndepths_modeled){
      if(length(which(!is.na(tmp_resid[, i]))) > 3){
      index <- c(index, i)
      }
    }
    resid <- tmp_resid[, index]
    Qt <- cov(resid, use = "pairwise.complete.obs")
    full_Qt <- array(NA,dim=c(ndepths_modeled,ndepths_modeled))
    for(i in 1:length(index)){
      full_Qt[index[i],index] <- Qt[i,]
    }
    
    full_Qt_diag <- diag(full_Qt)
    inter <- approxfun(modeled_depths,full_Qt_diag,rule = 2)
    full_Qt_diag <- inter(modeled_depths)
    full_Qt_temp <- full_Qt
    #diag(full_Qt) <- rep(NA,length(full_Qt_diag))
    
    for(i in 1:ndepths_modeled){
      if(!is.na(full_Qt_temp[i,i])){
        inter <- approxfun(modeled_depths,full_Qt[i,],rule = 2)
        full_Qt[i,] <- inter(modeled_depths)
        full_Qt[,i] <- inter(modeled_depths)
      }
    }
    for(i in 1:ndepths_modeled){
      inter <- approxfun(modeled_depths,full_Qt[,i],rule = 2)
      full_Qt[,i] <- inter(modeled_depths)
    }
    
    diag(full_Qt) <- full_Qt_diag
    
    qt <- full_Qt
    
  }else{
    Qt <- array(0.0, dim = c(ndepths_modeled, ndepths_modeled))
    for(i in 1:ndepths_modeled){
      Qt[i,i] <- var(resid30day[, i], na.rm = TRUE)
    }
    tmp_diag <- diag(Qt)
    inter <- approxfun(modeled_depths[which(!is.na(tmp_diag))],tmp_diag[which(!is.na(tmp_diag))],rule = 2)
    new_diag <- inter(modeled_depths)
    diag(Qt) <- new_diag
    qt <- Qt
  }
  
  if(include_wq){
    for(wq in 1:num_wq_vars){
      #IF THERE IS SOME DATA FOR THE VARIABLE
      if(length(which(!is.na(c(resid30day[, wq_start[wq]:wq_end[wq]])))) > 0){
        for(j in 1:ndepths_modeled){
          qt <- rbind(qt, rep(0.0, ncol(qt)))
          qt <- cbind(qt, rep(0.0, nrow(qt)))
          if(length(!is.na(resid30day[, wq_start[wq] + j - 1])) > 3){
            qt[ncol(qt),nrow(qt)] <- var(resid30day[ ,wq_start[wq] + j - 1], na.rm = TRUE)
          }else{
            qt[ncol(qt),nrow(qt)] <- NA
          }
        }
        tmp_diag <- diag(qt)[wq_start[wq]:wq_end[wq]]
        if(length(which(!is.na(tmp_diag))) > 1){
          modeled_depths[which(!is.na(tmp_diag))]
          inter <- approxfun(modeled_depths[which(!is.na(tmp_diag))],tmp_diag[which(!is.na(tmp_diag))],rule = 2)
          new_diag <- inter(modeled_depths)
          diag(qt)[wq_start[wq]:wq_end[wq]] <- new_diag
        }else if(length(which(!is.na(tmp_diag))) == 1){
          diag(qt)[wq_start[wq]:wq_end[wq]] <- tmp_diag[which(!is.na(tmp_diag))]
        }else{
          diag(qt)[wq_start[wq]:wq_end[wq]] <- old_qt[wq_start[wq] + j - 1, wq_start[wq] + j - 1]
        }
      }else{ #IF THERE IS NO DATA FOR THE VARIABLE
        for(j in 1:ndepths_modeled){
          qt <- rbind(qt, rep(0.0, ncol(qt)))
          qt <- cbind(qt, rep(0.0, nrow(qt)))
          qt[ncol(qt),nrow(qt)] <- old_qt[wq_start[wq] + j - 1, wq_start[wq] + j - 1]
        }
      } 
    }
  }
  
  if(npars > 0){
    for(p in 1:npars){
      qt <- rbind(qt, rep(0.0, ncol(qt)))
      qt <- cbind(qt, rep(0.0, nrow(qt)))
      qt[ncol(qt),nrow(qt)] <- old_qt[nstates + p,nstates + p]
    }
  }
  
  return(qt)
}