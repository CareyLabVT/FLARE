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
    for(i in (length(modeled_depths)+1):nrows(old_qt)){
        qt <- rbind(qt, rep(0.0, ncol(qt)))
        qt <- cbind(qt, rep(0.0, nrow(qt)))
        qt[ncol(qt),nrow(qt)] <- old_qt[i,i]
    }
  }
  
  return(qt)
}

update_sigma <- function(qt, p_t, h, x_star, pars_star, x_corr, pars_corr, psi_t, zt, npars, qt_pars, include_pars_in_qt_update, nstates, curr_qt_alpha){
  
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
    y_corr[m, ] <- zt + t(rmvnorm(n = 1, mean = rep(0,dim(psi_t)[1]), sigma = psi_t))
    y_tmp <- y_corr[m,] - h %*% x_par_corr[m, ]
    if(m == 1){
      y_it <- y_tmp %*% t(y_tmp)
    }else{
      y_it <- y_tmp %*% t(y_tmp) + y_it
    }
  }
  
  St <-  y_it / (nmembers - 1)
  
  q_star <- gamma %*% (St - h %*% p_t_star %*% t(h) - psi_t) %*% t(gamma)


    qt <- (curr_qt_alpha * old_qt) + ((1 - curr_qt_alpha) *  q_star)
    if(npars > 0 & !include_pars_in_qt_update){
      for(pars in 1:npars){
        qt <- rbind(qt, rep(0.0, ncol(qt)))
        qt <- cbind(qt, rep(0.0, nrow(qt)))
        qt[ncol(qt),nrow(qt)] <- diag(qt_pars)[pars]
      }
    }
    
  return(qt)
}