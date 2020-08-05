run_EnKF <- function(x,
                     z,
                     qt,
                     qt_pars,
                     psi_slope,
                     psi_intercept,
                     full_time_local,
                     working_directory,
                     pars_config,
                     modeled_depths,
                     surface_height,
                     wq_start,
                     wq_end,
                     met_file_names,
                     include_wq,
                     spin_up_days,
                     glm_output_vars,
                     process_uncertainty,
                     initial_condition_uncertainty,
                     parameter_uncertainty,
                     machine,
                     hist_days,
                     inflow_file_names,
                     outflow_file_names,
                     management_input,
                     forecast_sss_on,
                     snow_ice_thickness,
                     avg_surf_temp,
                     running_residuals,
                     the_sals_init,
                     mixing_vars,
                     glm_depths,
                     diagnostics_names,
                     diagnostics,
                     combined_error,
                     code_folder, 
                     base_GLM_nml, 
                     base_AED_nml,
                     base_AED_phyto_pars_nml,
                     base_AED_zoop_pars_nml,
                     obs_config,
                     states_config
){
  
  npars <- nrow(pars_config)
  nsteps <- length(full_time_local)
  nmembers <- dim(x)[2]
  n_met_members <- length(met_file_names) - 1
  nstates <- dim(x)[3] - npars
  ndepths_modeled <- length(modeled_depths)

  
  q_v <- rep(NA,ndepths_modeled)
  w <- rep(NA,ndepths_modeled)
  
  alpha_v <- 1 - exp(-vert_decorr_length)
  
  
  if(include_wq){
    num_wq_vars <- length(wq_start)
  }
  
  num_phytos <- length(which(str_detect(states_config$state_names,"PHY_") & !str_detect(states_config$state_names,"_IP") & !str_detect(states_config$state_names,"_IN")))
  
  full_time_local_char <- strftime(full_time_local,
                                   format="%Y-%m-%d %H:%M",
                                   tz = local_tzone)
  
  x_prior <- array(NA, dim = c(nsteps, nmembers, nstates + npars))
  
  glm_salt <- array(NA, dim = c(nmembers, 500))
  
  set_up_model(code_folder, 
               working_directory, 
               base_GLM_nml, 
               num_wq_vars, 
               base_AED_nml,
               base_AED_phyto_pars_nml,
               base_AED_zoop_pars_nml,
               ndepths_modeled,
               modeled_depths,
               the_sals_init,
               machine,
               include_wq)
  
  ###START EnKF
  for(i in 2:nsteps){
    
    print(paste0("Running time step ", i-1, " : ", 
                 full_time_local_char[i - 1], " - ",
                 full_time_local_char[i]))
    
    met_index <- 1
    inflow_outflow_index <- 1
    
    curr_start <- (full_time_local_char[i - 1])
    curr_stop <- (full_time_local_char[i])
    
    setwd(working_directory)
    
    #Create array to hold GLM predictions for each ensemble
    x_star <- array(NA, dim = c(nmembers, nstates))
    x_corr <- array(NA, dim = c(nmembers, nstates))
    
    #Matrix to store calculated ensemble specific deviations and innovations
    dit <- array(NA, dim = c(nmembers, nstates))
    dit_combined <- array(NA, dim = c(nmembers, nstates + npars))
    
    if(npars > 0){
      pars_corr <-  array(NA, dim = c(nmembers, npars))
      dit_pars<- array(NA, dim = c(nmembers, npars))
    }
    
    # Start loop through ensemble members
    for(m in 1:nmembers){
      
      curr_met_file <- met_file_names[met_index]
      
      
      if(npars > 0){
        curr_pars <- x[i - 1, m , (nstates+1):(nstates+npars)] 
      }
      
      out <- run_model(i,
                       m,
                       mixing_vars_start = mixing_vars[m, ],
                       curr_start,
                       curr_stop,
                       par_names = pars_config$par_names,
                       curr_pars,
                       working_directory,
                       par_nml = pars_config$par_nml,
                       num_phytos,
                       glm_depths_start = glm_depths[i-1, m, ],
                       surface_height_start = surface_height[i-1, m],
                       simulate_SSS,
                       x_start = x[i-1, m, ],
                       full_time_local, 
                       wq_start, 
                       wq_end,
                       management_input, 
                       hist_days, 
                       forecast_sss_on,
                       sss_depth,
                       use_specified_sss,
                       modeled_depths,
                       ndepths_modeled,
                       curr_met_file,
                       inflow_file_names,
                       inflow_outflow_index,
                       outflow_file_names,
                       glm_output_vars,
                       diagnostics_names,
                       machine,
                       npars,
                       num_wq_vars,
                       snow_ice_thickness_start = snow_ice_thickness[i-1, m, ],
                       avg_surf_temp_start = avg_surf_temp[i-1, m],
                       nstates,
                       states_config,
                       include_wq)
      
      x_star[m, ] <- out$x_star_end
      surface_height[i ,m ] <- out$surface_height_end
      snow_ice_thickness[i,m ,] <- out$snow_ice_thickness_end
      avg_surf_temp[i , m] <- out$avg_surf_temp_end
      mixing_vars[m, ] <- out$mixing_vars_end
      diagnostics[i, m, , ] <- out$diagnostics_end
      glm_depths[i, m,] <- out$glm_depths_end
      
      ########################################
      #END GLM SPECIFIC PART
      ########################################
      
      #INCREMENT ThE MET_INDEX TO MOVE TO ThE NEXT NOAA ENSEMBLE
      met_index <- met_index + 1
      if(met_index > length(met_file_names)){
        met_index <- 1
      }
      
      inflow_outflow_index <- inflow_outflow_index + 1
      if(inflow_outflow_index > nrow(inflow_file_names)){
        inflow_outflow_index <- 1
      }
      
      
      #Add process noise
      
      q_v[] <- NA
      w[] <- NA
      for(jj in 1:length(combined_error)){
        w[] <- rnorm(ndepths_modeled, 0, 1)
        q_v[1] <- combined_error[jj] * w[1]
        for(kk in 2:ndepths_modeled){
          q_v[kk] <- alpha_v * q_v[kk-1] + sqrt(1 - alpha_v^2) * combined_error[jj] * w[kk]
        }
        
        x_corr[m, (((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] <- 
          x_star[m, (((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] + q_v
      }
      
    } # END ENSEMBLE LOOP
    
    
    #Correct any negative water quality states
    if(include_wq & no_negative_states){
      for(m in 1:nmembers){
        index <- which(x_corr[m,] < 0.0)
        x_corr[m, index[which(index <= wq_end[num_wq_vars] & index >= wq_start[1])]] <- 0.0
      }
    }
    
    if(npars > 0){
      pars_corr <- x[i - 1, , (nstates+1):(nstates+npars)]
      if(npars == 1){
        pars_corr <- matrix(pars_corr,nrow = length(pars_corr),ncol = 1)
      }
      pars_star <- pars_corr
    }
    
    
    
    if(npars > 0){
      x_prior[i, , ] <- cbind(x_corr, pars_corr)
    }else{
      x_prior[i, , ] <- x_corr
    }
    
    z_index <- which(!is.na(c(z[i, , ])))
    
    #if no observations at a time step then just propogate model uncertainity
    
    if(length(z_index) == 0 | 
       i > (hist_days + 1) | 
       i < (spin_up_days+1) | 
       hist_days == 0){
      
      if(npars > 0){
        
        if(i > (hist_days + 1)){
          #don't add the noise to parameters in future forecast mode (pars_star doesn't have noise)
          x[i, , ] <- cbind(x_corr, pars_star)
        }else{
          #add the noise to parameters if in data assimilation mode
          x[i, , ] <- cbind(x_corr, pars_corr)
        }
        
        if(process_uncertainty == FALSE & i > (hist_days + 1)){
          #don't add process noise if process uncertainty is false (x_star doesn't have noise)
          #don't add the noise to parameters in future forecast mode ()
          x[i, , ] <- cbind(x_star, pars_star)
        }
        
        if(i == (hist_days + 1) & initial_condition_uncertainty == FALSE){
          for(m in 1:nmembers){
            x[i, m, ] <- c(colMeans(x_star), pars_star[m, ]) 
          }
        }
        
        if(i < (spin_up_days+1)){
          x[i, , ] <- cbind(x_star, pars_star)
        }
        
      }else{
        if(i > (hist_days + 1)){
          x[i, , ] <- cbind(x_corr)
        }else{
          x[i, , ] <- x_prior[i, , ]
        }
        
        if(process_uncertainty == FALSE & i > (hist_days + 1)){
          x[i, , ] <- x_star
        }
        
        if(i < (spin_up_days+1)){
          x[i, , ] <- x_star
        }
      }
    }else{
      
      #if observation then calucate Kalman adjustment
      zt <- c(z[i, ,])
      zt <- zt[which(!is.na(zt))]
      
      #Assign which states have obs in the time step
      h <- matrix(0, nrow = length(obs_config$state_names_obs) * ndepths_modeled, ncol = nstates)
      
      index <- 0
      for(k in 1:(nstates/ndepths_modeled)){
        for(j in 1:ndepths_modeled){
          index <- index + 1
          if(!is.na(first(states_config$states_to_obs[[k]]))){
            for(jj in 1:length(states_config$states_to_obs[[k]])){
              if(!is.na((z[i, j, states_config$states_to_obs[[k]][jj]]))){
                states_to_obs_index <- states_config$states_to_obs[[k]][jj]
                index2 <- (states_to_obs_index - 1) * ndepths_modeled + j
                h[index2,index] <- states_config$states_to_obs_mapping[k]
              }
            }
          }
        }
      }
      z_index <- c()
      for(j in 1:nrow(h)){
        if(sum(h[j, ]) > 0){
          z_index <- c(z_index, j)
        }
      }
      
      h <- h[z_index, ]
      
      #Extract the data uncertainity for the data 
      #types present during the time-step 
      
      curr_psi <- psi_intercept[z_index] + psi_slope[z_index] * zt
      
      curr_psi <- curr_psi ^ 2
      
      if(length(z_index) > 1){
        psi_t <- diag(curr_psi)
      }else{
        #Special case where there is only one data 
        #type during the time-step
        psi_t <- curr_psi
      }
      
      d_mat <- t(rmvnorm(n = nmembers, mean = zt, sigma=as.matrix(psi_t)))
      
      #Set any negative observations of water quality variables to zero
      d_mat[which(z_index > length(modeled_depths) & d_mat < 0.0)] <- 0.0 
      
      #Ensemble mean
      ens_mean <- apply(x_corr[,], 2, mean)
      
      if(npars > 0){
        par_mean <- apply(pars_corr, 2, mean)
        for(m in 1:nmembers){
          pars_corr[m, ] <- pars_config$inflat_pars * (pars_corr[m,] - par_mean) + par_mean
        }
        par_mean <- apply(pars_corr, 2, mean)
      }
      
      
      #Loop through ensemble members
      for(m in 1:nmembers){  
        #  #Ensemble specific deviation
        dit[m, ] <- x_corr[m, ] - ens_mean
        if(npars > 0){
          dit_pars[m, ] <- pars_corr[m, ] - par_mean
        }
        if(m == 1){
          p_it <- dit[m, ] %*% t(dit[m, ]) 
          if(npars > 0){
            p_it_pars <- dit_pars[m, ] %*% t(dit[m, ])
          }
        }else{
          p_it <- dit[m, ] %*% t(dit[m, ]) +  p_it 
          if(npars > 0){
            p_it_pars <- dit_pars[m, ] %*% t(dit[m, ]) + p_it_pars
          }
        }
      }
      
      #estimate covariance
      p_t <- p_it / (nmembers - 1)
      if(npars > 0){
        p_t_pars <- p_it_pars / (nmembers - 1)
      }
      
      if(!is.na(localization_distance)){
        p_t <- localization(p_t,nstates,modeled_depths,num_wq_vars,wq_start,wq_end)
      }
      #Kalman gain
      k_t <- p_t %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
      if(npars > 0){
        k_t_pars <- p_t_pars %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
      }
      
      #Update states array (transposes are necessary to convert 
      #between the dims here and the dims in the EnKF formulations)
      update_increment <-  k_t %*% (d_mat - h %*% t(x_corr))
      x[i, , 1:nstates] <- t(t(x_corr) + update_increment)
      if(npars > 0){
        x[i, , (nstates+1):(nstates+npars)] <- t(t(pars_corr) + 
                                                   k_t_pars %*% (d_mat - h %*% t(x_corr)))
      }
      
      
      
      # if(adapt_qt_method == 1){
      #   #does previous day have an observation
      #   previous_day_obs <- FALSE
      #   if(length(which(!is.na(z[i-1, ]))) > 0){
      #     previous_day_obs <- TRUE
      #   }
      #   
      #   if(previous_day_obs){
      #     running_residuals[1:(num_adapt_days - 1), ] <- running_residuals[2:num_adapt_days, ]
      #     running_residuals[num_adapt_days, ] <- NA
      #     running_residuals[num_adapt_days, z_index] <- zt - ens_mean[z_index]
      #     if(!is.na(running_residuals[1, z_index[1]])){
      #       qt <- update_qt(running_residuals, 
      #                       modeled_depths, 
      #                       qt, 
      #                       include_wq, 
      #                       npars, nstates, 
      #                       wq_start, 
      #                       wq_end, 
      #                       num_wq_vars)
      #     }
      #   }
      # }
    }
    
    #IF NO INITIAL CONDITION UNCERTAINITY THEN SET EACH ENSEMBLE MEMBER TO THE MEAN
    #AT THE INITIATION OF ThE FUTURE FORECAST
    if(i == (hist_days + 1)){
      
      if(initial_condition_uncertainty == FALSE){
        state_means <- colMeans(x[i, ,1:nstates])
        for(m in 1:nmembers){
          x[i, m, 1:nstates]  <- state_means
        }
      }
      if(npars > 0){
        if(parameter_uncertainty == FALSE){
          par_means <- colMeans(x[i, ,(nstates + 1):(nstates + npars)])
          for(m in 1:nmembers){
            x[i, m, (nstates + 1):(nstates + npars)] <- par_means
          }
        }
      } 
    }
    
    ###################
    ## Quality Control Step 
    ##################
    
    #Correct any negative water quality states
    if(include_wq & no_negative_states){
      for(m in 1:nmembers){
        index <- which(x[i,m,] < 0.0)
        x[i, m, index[which(index <= wq_end[num_wq_vars] & index >= wq_start[1])]] <- 0.0
      }
    }
    
    #Correct any parameter values outside bounds
    if(npars > 0){
      for(par in 1:npars){
        low_index <- which(x[i, ,nstates + par] < pars_config$par_lowerbound[par])
        high_index <- which(x[i, ,nstates + par] > pars_config$par_upperbound[par]) 
        x[i,low_index ,nstates + par] <- pars_config$par_lowerbound[par]
        x[i,high_index ,nstates + par] <- pars_config$par_upperbound[par]
      }
    }
    
    ###############
    
    #Print parameters to screen
    if(npars > 0){
      for(par in 1:npars){
        print(paste0(pars_config$par_names_save[par],": mean ", 
                     round(mean(pars_corr[,par]),4)," sd ", 
                     round(sd(pars_corr[,par]),4)))
      }
    }
    
    # Save the states after the last historical (data assimilation) 
    # time step (before forecasting)
    if(i == (hist_days + 1)){
      x_restart <- x[i, , ]
      qt_restart <- qt
      surface_height_restart <- surface_height[i, ]
      snow_ice_restart <- snow_ice_thickness[i, , ]
      avg_surf_temp_restart <- avg_surf_temp[i, ]
      mixing_restart <- mixing_vars
      glm_depths_restart <- glm_depths[i, , ]
      
    }else if(hist_days == 0 & i == 2){
      x_restart <- x[1, , ]
      qt_restart <- qt
      surface_height_restart <- surface_height[i, ]
      snow_ice_restart <- snow_ice_thickness[i, , ]
      avg_surf_temp_restart <- avg_surf_temp[i, ]
      mixing_restart <- mixing_vars
      glm_depths_restart <- glm_depths[i, , ]
    }
  }
  
  return(list(x = x, 
              x_restart = x_restart, 
              qt_restart = qt_restart, 
              x_prior = x_prior,
              surface_height_restart = surface_height_restart,
              snow_ice_restart = snow_ice_restart,
              snow_ice_thickness = snow_ice_thickness,
              surface_height = surface_height,
              avg_surf_temp_restart = avg_surf_temp_restart,
              running_residuals = running_residuals,
              mixing_restart = mixing_restart,
              glm_depths_restart = glm_depths_restart,
              diagnostics = diagnostics))
}