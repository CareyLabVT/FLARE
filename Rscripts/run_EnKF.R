run_EnKF <- function(x,
                     z,
                     qt,
                     qt_pars,
                     psi_slope,
                     psi_intercept,
                     full_time_local,
                     working_directory,
                     npars,
                     modeled_depths,
                     surface_height,
                     wq_start,
                     wq_end,
                     met_file_names,
                     include_wq,
                     spin_up_days,
                     z_states,
                     glm_output_vars,
                     process_uncertainty,
                     initial_condition_uncertainty,
                     parameter_uncertainty,
                     machine,
                     hist_days,
                     print_glm2screen,
                     x_phyto_groups,
                     inflow_file_names,
                     outflow_file_names,
                     management_input,
                     forecast_sss_on,
                     snow_ice_thickness,
                     avg_surf_temp,
                     running_residuals){
  
  nsteps <- length(full_time_local)
  nmembers <- dim(x)[2]
  n_met_members <- length(met_file_names) - 1
  nstates <- dim(x)[3] - npars
  ndepths_modeled <- length(modeled_depths)
  
  if(include_wq){
    num_wq_vars <- length(wq_start)
    num_phytos <- length(tchla_components_vars)
  }
  
  full_time_day_local <- strftime(full_time_local,
                                  format="%Y-%m-%d",
                                  tz = local_tzone)
  
  x_prior <- array(NA, dim = c(nsteps, nmembers, nstates + npars))
  
  
  old_DYLD_LIBRARY_PATH <- Sys.getenv("DYLD_FALLBACK_LIBRARY_PATH")
  Sys.setenv(DYLD_FALLBACK_LIBRARY_PATH = paste(old_DYLD_LIBRARY_PATH, working_directory, sep = ":"))
  ###START EnKF
  
  for(i in 2:nsteps){
    
    print(paste0("Running time step ", i-1, " : ", full_time_local[i - 1], " - ", full_time_local[i]))
    met_index <- 1
    inflow_outflow_index <- 1
    
    curr_start <- (full_time_local[i - 1])
    curr_stop <- (full_time_local[i])
    
    setwd(working_directory)
    
    #Create array to hold GLM predictions for each ensemble
    x_star <- array(NA, dim = c(nmembers, nstates))
    x_corr <- array(NA, dim = c(nmembers, nstates))
    
    if(include_wq){
      phyto_groups_star <-  array(NA, dim = c(nmembers, length(modeled_depths), num_phytos))
    }
    
    #Matrix to store calculated ensemble specific deviations and innovations
    dit <- array(NA, dim = c(nmembers, nstates))
    dit_combined <- array(NA, dim = c(nmembers, nstates + npars))
    
    if(npars > 0){
      pars_corr <-  array(NA, dim = c(nmembers, npars))
      dit_pars<- array(NA, dim = c(nmembers, npars))
    }
    
    nqt <- rmvnorm(n = nmembers, sigma = as.matrix(qt)) 
    if(npars > 0){
      if(i > (hist_days + 1) && parameter_uncertainty == FALSE){
        pqt <- rep(0, length(nqt[, (nstates+1):(nstates+npars)]))
      }else{
        pqt <- nqt[, (nstates+1):(nstates+npars)] 
      }
    }
    
    # Start loop through ensemble members
    for(m in 1:nmembers){
      
      if(i > (hist_days + 1) & use_future_met == TRUE){
        curr_met_file <- met_file_names[1 + met_index]
      }else{
        curr_met_file <- met_file_names[1]
      }
      
      if(npars > 0){
        curr_pars <- x[i - 1, m , (nstates+1):(nstates+npars)]
      }
      
      ########################################
      #BEGIN GLM SPECIFIC PART
      ########################################
      # REQUIRES:
      #curr_met_file, curr_pars, par_names, par_nml, x[i - 1, m, ], surface_height[i - 1, m],
      # curr_start, curr_stop
      # RETURNS;
      # x_star[m, ], 
      
      if(npars > 0){
        
        sed_temp_mean_index <- which(par_names == "sed_temp_mean")
        non_sed_temp_mean_index <- which(par_names != "sed_temp_mean")
        if(length(sed_temp_mean_index) == 1){
          update_var(c(curr_pars[sed_temp_mean_index],zone2_temp_init_mean),
                     par_names[sed_temp_mean_index],
                     working_directory, par_nml[sed_temp_mean_index])
        }else if(length(sed_temp_mean_index) == 2){
          update_var(c(curr_pars[sed_temp_mean_index[1]],curr_pars[sed_temp_mean_index[2]]),
                     par_names[sed_temp_mean_index[1]],
                     working_directory, par_nml[sed_temp_mean_index[1]])
        }else if(length(sed_temp_mean_index) > 2){
          stop(paste0("Too many sediment temperature zones"))
        }
        
        if(length(non_sed_temp_mean_index) > 0){
          for(par in non_sed_temp_mean_index){
            update_var(curr_pars[par], par_names[par], working_directory, par_nml[par])
          }
        }
      }
      
      if(include_wq){
        non_tchla_states <- c(wq_start[1]:wq_end[16])
        wq_init_vals <- round(c(x[i - 1, m, non_tchla_states], x_phyto_groups[i-1,m ,]), 3)
        update_var(wq_init_vals, "wq_init_vals" ,working_directory, "glm3.nml")
        
        if(simulate_SSS){
          create_sss_input_output(x, i, m, full_time_day_local, working_directory, wq_start, management_input, hist_days, forecast_sss_on)
        }
      }
      
      update_var(round(x[i - 1, m, 1:length(modeled_depths)], 3), "the_temps", working_directory, "glm3.nml")
      update_var(surface_height[i - 1, m], "lake_depth", working_directory, "glm3.nml") 
      
      update_var(0.0, "snow_thickness", working_directory, "glm3.nml") 
      update_var(snow_ice_thickness[i - 1, m, 2], "white_ice_thickness", working_directory, "glm3.nml") 
      update_var(snow_ice_thickness[i - 1, m, 3], "blue_ice_thickness", working_directory, "glm3.nml")
      update_var(avg_surf_temp[i - 1, m], "avg_surf_temp", working_directory, "glm3.nml")
      
      #ALLOWS THE LOOPING THROUGH NOAA ENSEMBLES
      
      update_var(curr_met_file, "meteo_fl", working_directory, "glm3.nml")
      
      if(n_inflow_outflow_members == 1){
        tmp <- file.copy(from = inflow_file_names[1], to = "inflow_file1.csv", overwrite = TRUE)
        tmp <- file.copy(from = inflow_file_names[2], to = "inflow_file2.csv", overwrite = TRUE)
        tmp <- file.copy(from = outflow_file_names, to = "outflow_file1.csv", overwrite = TRUE)
      }else{
        tmp <- file.copy(from = inflow_file_names[inflow_outflow_index, 1], to = "inflow_file1.csv", overwrite = TRUE)
        tmp <- file.copy(from = inflow_file_names[inflow_outflow_index, 2], to = "inflow_file2.csv", overwrite = TRUE)
        tmp <- file.copy(from = outflow_file_names[inflow_outflow_index], to = "outflow_file1.csv", overwrite = TRUE)      
      }
      
      update_time(start_value  = curr_start, stop_value = curr_stop, working_directory)
      
      #Use GLM NML files to run GLM for a day
      # Only allow simulations without NaN values in the output to proceed. 
      #Necessary due to random Nan in AED output
      pass <- FALSE
      num_reruns <- 0
      
      #Sys.setenv(DYLD_LIBRARY_PATH="/opt/intel/lib")
      #Sys.setenv(DYLD_LIBRARY_PATH=working_directory)
      
      while(!pass){
        unlink(paste0(working_directory, "/output.nc")) 
        
        if(machine == "unix" | machine == "mac"){
          system2(paste0(working_directory, "/", "glm"), stdout = print_glm2screen, stderr = print_glm2screen)
        }else if(machine == "windows"){
          system2(paste0(working_directory, "/", "glm.exe"), invisible = print_glm2screen)
        }else{
          print("Machine not identified")
          stop()
        }
        
        if(file.exists(paste0(working_directory, "/output.nc")) & 
           !has_error(nc <- nc_open(paste0(working_directory, "/output.nc")))){
          
          if(length(ncvar_get(nc, "time")) > 1){
            nc_close(nc)
            
            if(include_wq){
              output_vars <- c(glm_output_vars, tchla_components_vars)
            }else{
              output_vars <- c(glm_output_vars)
            }
            GLM_temp_wq_out <- get_glm_nc_var_all_wq(ncFile = "/output.nc",
                                                     working_dir = working_directory,
                                                     z_out = modeled_depths,
                                                     vars = output_vars)
            
            x_star[m, 1:nstates] <- round(c(GLM_temp_wq_out$output)[1:nstates], 3)
            
            surface_height[i, m] <- round(GLM_temp_wq_out$surface_height, 3) 
            
            snow_ice_thickness[i, m, ] <- round(GLM_temp_wq_out$snow_wice_bice, 3)
            
            avg_surf_temp[i, m] <- round(GLM_temp_wq_out$avg_surf_temp, 3)
            
            if(include_wq){
              phyto_groups_star[m, , ] <- round(GLM_temp_wq_out$output[ , (length(glm_output_vars) + 1): (length(glm_output_vars)+ num_phytos)], 3)
            }
            
            if(length(which(is.na(x_star[m, ]))) == 0){
              pass = TRUE
            }else{
              num_reruns <- num_reruns + 1
            }
          }else{
            num_reruns <- num_reruns + 1
          }
        }else{
          num_reruns <- num_reruns + 1
        }
        if(num_reruns > 1000){
          stop(paste0("Too many re-runs (> 1000) due to NaN values in output"))
        }
      }
      
      ########################################
      #END GLM SPECIFIC PART
      ########################################
      
      #INCREMENT ThE MET_INDEX TO MOVE TO ThE NEXT NOAA ENSEMBLE
      met_index <- met_index + 1
      if(met_index > n_met_members){
        met_index <- 1
      }
      
      inflow_outflow_index <- inflow_outflow_index + 1
      if(inflow_outflow_index > n_inflow_outflow_members){
        inflow_outflow_index <- 1
      }
      
    } # END ENSEMBLE LOOP
    
    
    # DEAL WITh ENSEMBLE MEMBERS ThAT ARE "BAD" AND 
    # PRODUCE NA VALUES OR hAVE NEGATIVE TEMPERATURES
    # ThIS RANDOMLY REPLACES IT WITh A GOOD ENSEMBLE MEMBER
    if(length(which(is.na(c(x_star)))) > 0){
      good_index <- NULL
      for(m in 1:nmembers){
        if(length(which(is.na(c(x_star[m, ])))) == 0 &
           length(which(c(x_star[m, ]) <= 0)) == 0){
          good_index <- c(good_index, m)
        }
      }
      for(m in 1:nmembers){
        if(length(which(is.na(c(x_star[m, ])))) > 0 |
           length(which(c(x_star[m, ]) <= 0) > 0)){
          replace_index <- sample(good_index, 1)
          x_star[m, ] <- x_star[replace_index, ]
          surface_height[i, m] <- surface_height[i, replace_index]
          snow_ice_thickness[i, m, ] <- snow_ice_thickness[i, replace_index, ]
        }
      }
    }
    
    #Corruption [nmembers x nstates] 
    x_corr <- x_star + nqt[, 1:nstates]
    
    if(npars > 0){
      pars_corr <- x[i - 1, , (nstates+1):(nstates+npars)] + pqt
      pars_star <- x[i - 1, , (nstates+1):(nstates+npars)]
    }
    
    if(include_wq){
      for(m in 1:nmembers){
        index <- which(x_corr[m,] < 0.0)
        x_corr[m, index[which(index < wq_end[num_wq_vars])]] <- 0.0
      }
    }
    
    if(npars > 0){
      x_prior[i, , ] <- cbind(x_corr, pars_corr)
    }else{
      x_prior[i, , ] <- x_corr
    }
    
    z_index <- which(!is.na(z[i, ]))
    
    #if no observations at a time step then just propogate model uncertainity
    
    if(length(z_index) == 0 | i > (hist_days + 1) | i < (spin_up_days+1) | hist_days == 0){
      
      if(npars > 0){
        
        if(i > (hist_days + 1)){
          x[i, , ] <- cbind(x_corr, pars_corr)
        }else{
          x[i, , ] <- cbind(x_corr, pars_corr)
        }
        
        if(process_uncertainty == FALSE & i > (hist_days + 1)){
          x[i, , ] <- cbind(x_star, pars_corr)
        }
        
        if(i == (hist_days + 1) & initial_condition_uncertainty == FALSE){
          for(m in 1:nmembers){
            x[i, m, ] <- c(colMeans(x_star), pars_star[m, ]) 
            x[i, m, ] <- c(colMeans(x_star), pars_corr[m, ]) 
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
      zt <- z[i, z_index]
      z_states_t <- z_states[i, z_index]
      
      #Assign which states have obs in the time step
      h <- array(0, dim = c(length(zt) ,nstates))
      h_combined <- array(0, dim = c(length(zt) ,nstates + npars))
      for(j in 1:length(z_index)){
        h[j, z_states_t[j]] <- 1
        h_combined[j, z_states_t[j]] <- 1
      }
      
      #Extract the data uncertainity for the data 
      #types present during the time-step 
      
      curr_psi <- psi_intercept[z_index] + psi_slope[z_index] * zt
      
      if(length(z_index) > 1){
        psi_t <- diag(curr_psi)
      }else{
        #Special case where there is only one data 
        #type during the time-step
        psi_t <- curr_psi
      }
      
      #Ensemble mean
      ens_mean <- apply(x_corr[,], 2, mean)
      
      if(npars > 0){
        par_mean <- apply(pars_corr, 2, mean)
      }
      
      n_psi = t(rmvnorm(n = 1, mean = zt, sigma=as.matrix(psi_t)))
      
      #Set any negative observations of water quality variables to zero
      #n_psi[which(z_index > length(modeled_depths) & n_psi < 0.0)] <- 0.0 
      
      d_mat <- t(matrix(rep(n_psi, each = nmembers), 
                        nrow = nmembers, 
                        ncol = length(n_psi)))
      
      #Loop through ensemble members
      for(m in 1:nmembers){  
        #  #Ensemble specific deviation
        dit[m, ] <- x_corr[m, ] - ens_mean
        if(npars > 0){
          dit_pars[m, ] <- pars_corr[m, ] - par_mean
          dit_combined[m, ] <- c(dit[m, ], dit_pars[m, ])
        }
        if(m == 1){
          p_it <- dit[m, ] %*% t(dit[m, ]) 
          if(npars > 0){
            p_it_pars <- dit_pars[m, ] %*% t(dit[m, ])
            p_it_combined <- dit_combined[m, ] %*% t(dit_combined[m, ]) 
          }
        }else{
          p_it <- dit[m, ] %*% t(dit[m, ]) +  p_it 
          if(npars > 0){
            p_it_pars <- dit_pars[m, ] %*% t(dit[m, ]) + p_it_pars
            p_it_combined <- dit_combined[m, ] %*% t(dit_combined[m, ]) + p_it_combined
          }
        }
      }
      
      #estimate covariance
      p_t <- p_it / (nmembers - 1)
      if(npars > 0){
        p_t_pars <- p_it_pars / (nmembers - 1)
        p_t_combined <- p_it_combined/ (nmembers - 1)
      }
      #Kalman gain
      k_t <- p_t %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t)
      if(npars > 0){
        k_t_pars <- p_t_pars %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t)
        k_t_combined <- p_t_combined %*% t(h_combined) %*% solve(h_combined %*% p_t_combined %*% t(h_combined) + psi_t)
      }
      
      #Update states array (transposes are necessary to convert 
      #between the dims here and the dims in the EnKF formulations)
      update_increment <-  k_t %*% (d_mat - h %*% t(x_corr))
      x[i, , 1:nstates] <- t(t(x_corr) + update_increment)
      if(npars > 0){
        x[i, , (nstates+1):(nstates+npars)] <- t(t(pars_corr) + k_t_pars %*% (d_mat - h %*% t(x_corr)))
      }
      
      
      
      if(adapt_qt_method == 1){
        #does previous day have an observation
        previous_day_obs <- FALSE
        if(length(which(!is.na(z[i-1, ]))) > 0){
          previous_day_obs <- TRUE
        }
        
        if(previous_day_obs){
          running_residuals[1:(num_adapt_days - 1), ] <- running_residuals[2:num_adapt_days, ]
          running_residuals[num_adapt_days, ] <- NA
          running_residuals[num_adapt_days, z_index] <- zt - ens_mean[z_index]
          #running_residuals[num_adapt_days, z_index] <- rowMeans(update_increment[z_index,])
          if(!is.na(running_residuals[1, z_index[1]])){
            qt <- update_qt(running_residuals, modeled_depths, qt, include_wq, npars, nstates, wq_start, wq_end, num_wq_vars)
          }
        }
      }else if(adapt_qt_method == 2){
        
        if(npars > 0){
          qt <- update_sigma(qt,
                             p_t = p_t_combined,
                             h = h_combined,
                             x_star,
                             pars_star,
                             x_corr,
                             pars_corr,
                             psi_t,
                             zt,
                             npars,
                             qt_pars,
                             include_pars_in_qt_update,
                             nstates,
                             qt_alpha)
        }else{
          qt <- update_sigma(qt, p_t, h, x_star, pars_star = NA, x_corr, pars_corr = NA, psi_t, zt, npars, qt_pars, include_pars_in_qt_update, nstates, qt_alpha)
        }
        
        qt <- localization(mat= qt,
                           nstates,
                           modeled_depths,
                           num_wq_vars,
                           wq_start,
                           wq_end)
      }
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
    if(include_wq){
      for(m in 1:nmembers){
        index <- which(x[i,m,] < 0.0)
        x[i, m, index[which(index <= wq_end[num_wq_vars])]] <- 0.0
      }
    }
    
    #Correct any parameter values outside bounds
    if(npars > 0){
      for(par in 1:npars){
        low_index <- which(x[i, ,nstates + par] < par_lowerbound[par])
        high_index <- which(x[i, ,nstates + par] > par_upperbound[par]) 
        x[i,low_index ,nstates + par] <- par_lowerbound[par]
        x[i,high_index ,nstates + par] <- par_upperbound[par]
      }
    }
    
    if(include_wq){
      phyto_proportions <- array(NA, dim = c(nmembers, ndepths_modeled, num_phytos))
      for(m in 1:nmembers){
        phyto_groups_chla <- phyto_groups_star[m, ,] / biomass_to_chla
        total <- rowSums(phyto_groups_chla)
        phyto_proportions[m, , ] <- phyto_groups_chla/total
        updated_tchla <- x[i, m, wq_start[num_wq_vars]:wq_end[num_wq_vars]]
        tmp <- matrix(rep(biomass_to_chla,each=ndepths_modeled),nrow=ndepths_modeled)
        x_phyto_groups[i, m , ] <- c(phyto_proportions[m, , ] * updated_tchla * tmp) 
      }
    } 
    
    ###############
    
    #Print parameters to screen
    if(npars > 0){
      for(par in 1:npars){
        print(paste0(par_names_save[par],": mean ", round(mean(pars_corr[,par]),4)," sd ", round(sd(pars_corr[,par]),4)))
      }
    }
    
    #Save the states after the last historical (data assimilation) time step (before forecasting)
    if(i == (hist_days + 1)){
      x_restart <- x[i, , ]
      qt_restart <- qt
      surface_height_restart <- surface_height[i, ]
      snow_ice_restart <- snow_ice_thickness[i, , ]
      avg_surf_temp_restart <- avg_surf_temp[i, ]
      if(include_wq){
        x_phyto_groups_restart <- x_phyto_groups[i, ,]
      }else{
        x_phyto_groups_restart <- NA
      }
    }else if(hist_days == 0 & i == 2){
      x_restart <- x[1, , ]
      qt_restart <- qt
      surface_height_restart <- surface_height[i, ]
      snow_ice_restart <- snow_ice_thickness[i, , ]
      avg_surf_temp_restart <- avg_surf_temp[i, ]
      if(include_wq){
        x_phyto_groups_restart <- x_phyto_groups[i, ,]
      }else{
        x_phyto_groups_restart <- NA
      }
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
              x_phyto_groups_restart = x_phyto_groups_restart,
              x_phyto_groups = x_phyto_groups,
              running_residuals = running_residuals))
}