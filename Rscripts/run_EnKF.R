run_EnKF <- function(x,
                     z,
                     qt,
                     qt_pars,
                     psi,
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
                     resid30day,
                     hist_days,
                     print_glm2screen,
                     x_phyto_groups){
  
  nsteps <- length(full_time_local)
  nmembers <- dim(x)[2]
  n_met_members <- length(met_file_names) - 1
  nstates <- dim(x)[3] - npars
  num_wq_vars <- length(wq_start)
  
  
  x_prior <- array(NA, dim = c(nsteps, nmembers, nstates + npars))
  
  ###START EnKF
  
  for(i in 2:nsteps){
    print(paste0("Running time step ", i, " : ", full_time_local[i - 1], " - ", full_time_local[i]))
    met_index <- 1
    #1) Update GLM NML files to match the current day of the simulation
    curr_start <- (full_time_local[i - 1])
    curr_stop <- (full_time_local[i])
    
    setwd(working_directory)
    
    #Create array to hold GLM predictions for each ensemble
    x_star <- array(NA, dim = c(nmembers, nstates))
    x_corr <- array(NA, dim = c(nmembers, nstates))
    
    donc <- array(NA, dim = c(nmembers, length(modeled_depths)))
    dopc <- array(NA, dim = c(nmembers, length(modeled_depths)))
    #Matrix to store calculated ensemble specific deviations and innovations
    dit <- array(NA, dim = c(nmembers, nstates))
    dit_combined <- array(NA, dim = c(nmembers, nstates + npars))
    
    if(npars > 0){
      pars_corr <-  array(NA, dim = c(nmembers, npars))
      dit_pars<- array(NA, dim = c(nmembers, npars))
    }
    
    nqt <- rmvnorm(n = nmembers, sigma = as.matrix(qt)) 
    if(npars > 0){
      pqt <- nqt[, (nstates+1):(nstates+npars)]
    }
    
    for(m in 1:nmembers){
      
      update_var(round(x[i - 1, m, 1:length(modeled_depths)], 3), "the_temps", working_directory, "glm3.nml")
      update_var(surface_height[i - 1, m], "lake_depth", working_directory, "glm3.nml")
      if(npars > 0){
        
        if(i > (hist_days + 1)){
          curr_pars <- x[i - 1, m , (nstates+1):(nstates+npars)]
        }else{
          curr_pars <- x[i - 1, m , (nstates+1):(nstates+npars)] + pqt[m, ]
        }
        
        if(parameter_uncertainty == FALSE){
          curr_pars <- colMeans(x[i - 1, , (nstates+1):(nstates+npars)])
        }
        
        sed_temp_mean_index <- which(par_names == "sed_temp_mean")
        non_sed_temp_mean_index <- which(par_names != "sed_temp_mean")
        if(length(sed_temp_mean_index) >= 1){
          update_var(c(curr_pars[sed_temp_mean_index[1]],curr_pars[sed_temp_mean_index[2]]),
                     par_names[sed_temp_mean_index[1]],
                     working_directory, par_nml[sed_temp_mean_index[1]])
          #update_var(c(curr_pars[sed_temp_mean_index[1]],curr_pars[sed_temp_mean_index[1]]),
          #           par_names[sed_temp_mean_index[1]],
          #           working_directory, par_nml[sed_temp_mean_index[1]])
        }
        
        if(length(non_sed_temp_mean_index) > 0){
          for(par in non_sed_temp_mean_index){
            if(par_names[par] == "pd%R_growth"){
              update_var(curr_pars[par], par_names[par], working_directory, par_nml[par])
            }else{
              update_var(curr_pars[par], par_names[par], working_directory, par_nml[par])
            }
          }
        }
      }
      
      if(include_wq){
        non_phytos <- round(c(x[i - 1, m, wq_start[1]:wq_end[num_wq_vars-1]]), 3)
        phytos <- round(x_phyto_groups[i-1, m, ],3)
        wq_init_vals_w_phytos <- c(non_phytos, phytos)
        update_var(wq_init_vals_w_phytos, "wq_init_vals" ,working_directory, "glm3.nml")
      }
      
      
      #ALLOWS ThE LOOPING ThROUGh NOAA ENSEMBLES
      
      working_directory_docker <- "/GLM/TestLake"
      if(i > (hist_days + 1)){
        update_var(met_file_names[1 + met_index], "meteo_fl", working_directory, "glm3.nml")
        #update_var(paste0("FCR_inflow.csv"), "inflow_fl", working_directory)
        #update_var(paste0("FCR_spillway_outflow.csv"), "outflow_fl", working_directory)
      }else{
        update_var(met_file_names[1], "meteo_fl", working_directory, "glm3.nml")
        update_var("GLM_met.csv", "meteo_fl", working_directory, "glm3.nml")
        #update_var(paste0("FCR_weir_inflow.csv"), "inflow_fl", working_directory)
        #update_var(paste0("FCR_spillway_outflow.csv"), "outflow_fl", working_directory)
      }
      
      
      
      update_time(start_value  = curr_start, stop_value = curr_stop, working_directory)
      #Use GLM NML files to run GLM for a day
      # Only allow simulations without NaN values in the output to proceed. 
      #Necessary due to random Nan in AED output
      pass <- FALSE
      num_reruns <- 0
      
      while(!pass){
        unlink(paste0(working_directory, "/output.nc")) 
        
        #if(!print_glm2screen){
        #  system(paste0('docker run -it -d -v ',working_directory,':/GLM/TestLake hydrobert/glm-aed2 /bin/bash -c \"cd TestLake; /GLM/glm\"'))
        #}else{
        # start docker as background process (detached)
        #  system(paste0('docker run -it -d -v ',working_directory,':/GLM/TestLake hydrobert/glm-aed2 /bin/bash'))
        # get the id of your running container
        #  dockerps <- system('docker ps',intern = TRUE)
        #  dockerid <- strsplit(dockerps, split = "/t")
        #  dockerid <- strsplit(dockerid[[2]], split = " ")
        #  dockerid <- dockerid[[1]][1]
        # start the simulation (i - interactive, t - tty (user input))
        #  system(paste('docker exec -t',dockerid,'/bin/bash -c \"cd TestLake; /GLM/glm\"'))
        
        # stops and removes all running dockers
        #  system('docker kill $(docker ps -q)')
        #  system('docker rm $(docker ps -a -q)')
        #}
        
        
        #ptm <- proc.time()
        #system(paste0('docker run -it -d -v ',working_directory,':/GLM/TestLake hydrobert/glm-aed2 /bin/bash -c \"cd TestLake; /GLM/glm\"'))
        #docker <- proc.time() - ptm
        
        #ptm <- proc.time()
        #system2(paste0(working_directory, "/", "glm"), stdout = print_glm2screen, stderr = print_glm2screen)
        #local <- proc.time() - ptm
        
        #((docker - local)/local)*100
        
        
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
              GLM_temp_wq_out <- get_glm_nc_var_all_wq(ncFile = "/output.nc",
                                                       working_dir = working_directory,
                                                       z_out = modeled_depths,
                                                       vars = c(glm_output_vars,tchla_components_vars))
              x_star[m, 1:nstates] <- c(GLM_temp_wq_out$output[1:nstates])
              x_phyto_groups[i, m, ] <- GLM_temp_wq_out$output[(nstates+1):length(GLM_temp_wq_out$output)]
              donc[m, ] <- GLM_temp_wq_out$output[wq_start[11]:wq_end[11]]/GLM_temp_wq_out$output[wq_start[9]:wq_end[9]]
              dopc[m, ] <- GLM_temp_wq_out$output[wq_start[13]:wq_end[13]]/GLM_temp_wq_out$output[wq_start[9]:wq_end[9]]
            }else{
              GLM_temp_wq_out <- get_glm_nc_var_all_wq(ncFile = "/output.nc",
                                                       working_dir = working_directory,
                                                       z_out = modeled_depths,
                                                       vars = "temp")
              x_star[m, 1:length(modeled_depths)] <- c(GLM_temp_wq_out$output)
            }
            
            surface_height[i, m] <- round(GLM_temp_wq_out$surface_height, 3) 
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
      
      #INCREMENT ThE MET_INDEX TO MOVE TO ThE NEXT NOAA ENSEMBLE
      met_index <- met_index + 1
      if(met_index > n_met_members){
        met_index <- 1
      }
      
    }
    
    
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
        }
      }
    }
    
    #Corruption [nmembers x nstates] 
    x_corr <- x_star + nqt[, 1:nstates]

    if(npars > 0){
      pars_corr <- x[i - 1, , (nstates+1):(nstates+npars)] + pqt
      pars_star <- x[i - 1, , (nstates+1):(nstates+npars)]
      for(par in 1:npars){
      print(c(mean(pars_corr[,par]), sd(pars_corr[,par])))
      }
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
    
    if(i >= spin_up_days + 1){
      #Obs for time step
      z_index <- which(!is.na(z[i, ]))
      
      #if no observations at a time step then just propogate model uncertainity
      if(length(z_index) == 0 | i > (hist_days + 1)){
        if(npars > 0){
          if(i > (hist_days + 1)){
            x[i, , ] <- cbind(x_corr, pars_star)
          }else{
            x[i, , ] <- x_prior[i, , ]
          }
          if(process_uncertainty == FALSE & i > (hist_days + 1)){
            x[i, , ] <- cbind(x_star, pars_corr)
          }
          if(i == (hist_days + 1) & initial_condition_uncertainty == FALSE){
            for(m in 1:nmembers){
              x[i, m, ] <- colMeans(cbind(x_star, pars_corr)) 
            }
          }
        }else{
          if(i > (hist_days + 1)){
            x[i, , ] <- cbind(x_corr)
          }else{
            x[i, , ] <- x_prior[i, , ]
          }
          if(process_uncertainty == FALSE & i > (hist_days + 1)){
            x[i, , ] <- cbind(x_star)
          }
          if(i == (hist_days + 1) & initial_condition_uncertainty == FALSE){
            for(m in 1:nmembers){
              x[i, m, ] <- colMeans(cbind(x_star))
            }
          }
        }
      }else{
        
        #does previous day have an observation
        previous_day_obs <- FALSE
        if(length(which(!is.na(z[i-1, ]))) > 0){
          previous_day_obs <- TRUE
        }
        
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
        
        if(length(z_index) > 1){
          psi_t <- diag(psi[z_index])
        }else{
          #Special case where there is only one data 
          #type during the time-step
          psi_t <- psi[z_index]
        }
        
        #Ensemble mean
        ens_mean <- apply(x_corr[,], 2, mean)
        
        if(previous_day_obs){
          resid30day[1:29, ] <- resid30day[2:30, ]
          resid30day[30, z_index] <- ens_mean[z_index] - zt
          if(!is.na(resid30day[1, z_index[1]])){
            #qt <- update_qt(resid30day, modeled_depths, qt, include_wq, num_wq_vars)
            qt <- qt
          }
        }
        
        
        if(npars > 0){
          par_mean <- apply(pars_corr, 2, mean)
        }
        
        n_psi = t(rmvnorm(n = 1, mean = zt, sigma=as.matrix(psi_t)))
        
        #Set any negative observations of water quality variables to zero
        n_psi[which(z_index > length(modeled_depths) & n_psi < 0.0)] <- 0.0 
        
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
        x[i, , 1:nstates] <- t(t(x_corr) + k_t %*% (d_mat - h %*% t(x_corr)))
        #x[i, , ] <- t(t(x_prior[i, , ]) + k_t_combined %*% (d_mat - h_combined %*% t(x_prior[i, , ])))
        write.csv(k_t,paste0(working_directory,"/iter_",i,"_kalman_gain.csv"),row.names = FALSE)
        if(npars > 0){
          x[i, , (nstates+1):(nstates+npars)] <- t(t(pars_corr) + 
                                                     k_t_pars %*% (d_mat - h %*% t(x_corr)))
        }
        
        if(npars > 0){
          qt <- update_sigma(qt, p_t_combined, h_combined, x_star, pars_star, x_corr, pars_corr, psi_t, zt)
        }else{
          qt <- update_sigma(qt, p_t, h, x_star, pars_star, x_corr, pars_corr, psi_t, zt) 
        }

        if(include_wq){
          for(m in 1:nmembers){
            index <- which(x[i,m,] < 0.0)
            x[i, m, index[which(index < wq_end[num_wq_vars])]] <- 0.0
          }
        }
        
        #IF NO INITIAL CONDITION UNCERTAINITY ThEN SET EACh ENSEMBLE MEMBER TO ThE MEAN
        #AT THE INITIATION OF ThE FUTURE FORECAST
        if(i == (hist_days + 1) & initial_condition_uncertainty == FALSE){
          if(npars > 0){
            for(m in 1:nmembers){
              if(parameter_uncertainty == FALSE){
                x[i, m, ] <- colMeans(cbind(x_star, pars_corr)) 
              }else{
                x[i, m, ] <- c(colMeans(x_star),
                               x[i, m, (nstates + 1):(nstates + npars)])
              }
            }
          }else{
            x[i, m, ] <- colMeans(x_star)
          }
        }
      }
    }else{
      if(npars > 0){
        x[i, , ] <- cbind(x_star, pars_star)
      }else{
        x[i, , ] <- x_star
      }
    }
    
    #for(par in 1:npars){
    #  x[i,which(x[i, , nstates+par] < par_init_lowerbound[par]), nstates + par] <- par_init_lowerbound[par]
    #  x[i,which(x[i, , nstates+par] > par_init_upperbound[par]), nstates + par] <- par_init_upperbound[par]
    #}
    
    if(include_wq){
      for(m in 1:nmembers){
        phyto_biomass <- matrix(x_phyto_groups[i, m, ], nrow = length(modeled_depths), ncol = length(tchla_components_vars))
        phyto_proportions <- phyto_biomass
        index <- 0
        for(d in 1:length(modeled_depths)){
          for(pp in 1:length(tchla_components_vars)){
            index <- index + 1
            phyto_proportions[d, pp] <- (phyto_biomass[d, pp]/biomass_to_chla[pp])/sum(phyto_biomass[d,]/biomass_to_chla)
            #Update the phyto groups based on there ratios before CHLA updating
            x_phyto_groups[i, m, index] <- biomass_to_chla[pp] * phyto_proportions[d, pp] * x[i, m, wq_start[num_wq_vars] + (d-1)]
          }
        }
        #Maintain the DOC/DON and DOC/DOP ratios after updating of DOC
        x[i, m, wq_start[11]:wq_end[11]] <- x[i, m, wq_start[9]:wq_end[9]] * donc[m, ]
        x[i, m, wq_start[13]:wq_end[13]] <- x[i, m, wq_start[9]:wq_end[9]] * dopc[m, ]
      }
    }
    
    if(i == (hist_days + 1)){
      x_restart <- x[i, , ]
      qt_restart <- qt
    }
    
  }
  
  return(list(x = x, 
              x_restart = x_restart, 
              qt_restart = qt_restart, 
              x_prior = x_prior,
              resid30day = resid30day))
}