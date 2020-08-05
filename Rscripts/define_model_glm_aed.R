run_model <- function(i,
                    m,
                    mixing_vars_start,
                    curr_start,
                    curr_stop,
                    par_names,
                    curr_pars,
                    working_directory,
                    par_nml,
                    num_phytos,
                    glm_depths_start,
                    surface_height_start,
                    simulate_SSS,
                    x_start,
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
                    snow_ice_thickness_start,
                    avg_surf_temp_start,
                    nstates,
                    states_config,
                    include_wq){
  
  
  update_glm_nml_list <- list()
  update_aed_nml_list <- list()
  update_glm_nml_names <- c()
  update_aed_nml_names <- c()
  list_index <- 1
  
  update_glm_nml_list[[list_index]] <- mixing_vars_start
  update_glm_nml_names[list_index] <- "restart_variables"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- curr_start
  update_glm_nml_names[list_index] <- "start"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- curr_stop
  update_glm_nml_names[list_index] <- "stop"
  list_index <- list_index + 1
  
  glm_depths_end <- rep(NA,length(glm_depths_start))
  
  diagnostics <- array(NA, dim = c(ndepths_modeled, length(diagnostics_names)))
  
  x_star_end <- rep(NA, nstates)
  
  if(npars > 0){
    
    sed_temp_mean_index <- which(par_names == "sed_temp_mean")
    non_sed_temp_mean_index <- which(par_names != "sed_temp_mean" & 
                                       par_names != "Fsed_oxy" &
                                       par_names != "Fsed_frp")
    
    fsed_oxy_index <- which(par_names == "Fsed_oxy")
    
    if(length(fsed_oxy_index) == 1){
      update_var(round(curr_pars[fsed_oxy_index], 4), 
                 par_names[fsed_oxy_index], 
                 working_directory, 
                 par_nml[fsed_oxy_index])
    }else if(length(fsed_oxy_index) == 2){
      update_var(round(c(curr_pars[fsed_oxy_index[1]],curr_pars[fsed_oxy_index[2]]), 4), 
                 par_names[fsed_oxy_index[1]], 
                 working_directory, 
                 par_nml[fsed_oxy_index[1]])
      
    }else if(length(fsed_oxy_index) > 2){
      stop(paste0("Too many sediment Fsed oxy zones"))
    }
    
    fsed_frp_index <- which(par_names == "Fsed_frp")
    
    if(length(fsed_frp_index) == 1){
      update_var(round(curr_pars[fsed_frp_index], 4), 
                 par_names[fsed_frp_index], 
                 working_directory, 
                 par_nml[fsed_frp_index])
    }else if(length(fsed_frp_index) == 2){
      update_var(round(c(curr_pars[fsed_frp_index[1]],curr_pars[fsed_frp_index[2]]), 4), 
                 par_names[fsed_frp_index[1]], 
                 working_directory, 
                 par_nml[fsed_frp_index[1]])
      
    }else if(length(fsed_frp_index) > 2){
      stop(paste0("Too many sediment Fsed frp zones"))
    }
    
    if(length(sed_temp_mean_index) == 1){
      update_glm_nml_list[[list_index]] <- round(c(curr_pars[sed_temp_mean_index],
                                                   zone2_temp_init_mean),4) 
      update_glm_nml_names[list_index] <- "sed_temp_mean"
      list_index <- list_index + 1
      
    }else if(length(sed_temp_mean_index) == 2){
      update_glm_nml_list[[list_index]] <-  round(c(curr_pars[sed_temp_mean_index[1]],
                                                    curr_pars[sed_temp_mean_index[2]]), 4)
      update_glm_nml_names[list_index] <- "sed_temp_mean"
      list_index <- list_index + 1
      
    }else if(length(sed_temp_mean_index) > 2){
      stop(paste0("Too many sediment temperature zones"))
    }
    
    if(length(non_sed_temp_mean_index) > 0){
      for(par in non_sed_temp_mean_index){
        if(par_nml[par] == "glm3.nml"){
          if(par_names[par] == "inflow_factor"){
            if(include_wq){
              if(include_wetland_inflow){
                update_glm_nml_list[[list_index]] <- c( round(curr_pars[par],4), 
                                                        round(curr_pars[par],4), 
                                                        sss_inflow_factor)
              }else{
                update_glm_nml_list[[list_index]] <- c( round(curr_pars[par],4),
                                                        sss_inflow_factor)
              }
              update_glm_nml_names[list_index] <- par_names[par]
              list_index <- list_index + 1
              
              update_glm_nml_list[[list_index]] <- c( round(curr_pars[par],4), 
                                                      sss_inflow_factor)
              update_glm_nml_names[list_index] <- "outflow_factor"
              list_index <- list_index + 1
            }else{
              update_glm_nml_list[[list_index]] <-  round(curr_pars[par],4)
              update_glm_nml_names[list_index] <- par_names[par]
              list_index <- list_index + 1
              update_glm_nml_list[[list_index]] <-  round(curr_pars[par],4)
              update_glm_nml_names[list_index] <- "outflow_factor"
              list_index <- list_index + 1
            }
          }else{
            update_glm_nml_list[[list_index]] <- round(curr_pars[par],4)
            update_glm_nml_names[list_index] <- par_names[par]
            list_index <- list_index + 1
          }
        }else{
          if(par_nml[par] == "aed2_phyto_pars.nml"){
            update_var(rep(round(curr_pars[par],4), num_phytos), 
                       par_names[par], 
                       working_directory, 
                       par_nml[par])
          }else{
            update_var(curr_pars[par], 
                       par_names[par], 
                       working_directory, 
                       par_nml[par])
          }
        }
      }
    }
  }
  
  
  glm_depths_tmp <- glm_depths_start[!is.na(glm_depths_start)]
  
  glm_depths_tmp_tmp <- c(glm_depths_tmp, surface_height_start)
  glm_depths_mid <- glm_depths_tmp_tmp[1:(length(glm_depths_tmp_tmp)-1)] + diff(glm_depths_tmp_tmp)/2
  
  if(include_wq){
    
    wq_init_vals <- c()
    
    for(wq in 1:num_wq_vars){
      wq_enkf_tmp <- x_start[wq_start[wq]:wq_end[wq]]
      wq_enkf_tmp[wq_enkf_tmp < 0] <- 0
      wq_init_vals <- c(wq_init_vals, 
                        approx(modeled_depths,wq_enkf_tmp, glm_depths_mid, rule = 2)$y)
    }
    update_glm_nml_list[[list_index]] <- round(wq_init_vals, 4)
    update_glm_nml_names[list_index] <- "wq_init_vals"
    list_index <- list_index + 1
    
    if(simulate_SSS){
      create_sss_input_output(x_start, i, m, full_time_local, working_directory, 
                              wq_start, management_input, hist_days, 
                              forecast_sss_on, sss_depth,use_specified_sss, states_config, include_wq)
    }
  }
  
  the_temps_enkf_tmp <- x_start[1:ndepths_modeled]
  
  the_temps_glm <- approx(modeled_depths,the_temps_enkf_tmp, glm_depths_mid, rule = 2)$y
  
  update_glm_nml_list[[list_index]] <- round(the_temps_glm, 4)
  update_glm_nml_names[list_index] <- "the_temps"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- rep(0.0,length(the_temps_glm))
  update_glm_nml_names[list_index] <- "the_sals"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- round(glm_depths_tmp, 4)
  update_glm_nml_names[list_index] <- "the_depths"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- length(glm_depths_tmp)
  update_glm_nml_names[list_index] <- "num_depths"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- round(surface_height_start, 4)
  update_glm_nml_names[list_index] <- "lake_depth"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- 0.0
  update_glm_nml_names[list_index] <- "snow_thickness"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- round(snow_ice_thickness_start[2], 4)
  update_glm_nml_names[list_index] <- "white_ice_thickness"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- round(snow_ice_thickness_start[3], 4)
  update_glm_nml_names[list_index] <- "blue_ice_thickness"
  list_index <- list_index + 1
  
  update_glm_nml_list[[list_index]] <- round(avg_surf_temp_start, 4)
  update_glm_nml_names[list_index] <- "avg_surf_temp"
  list_index <- list_index + 1
  
  #ALLOWS THE LOOPING THROUGH NOAA ENSEMBLES
  
  update_glm_nml_list[[list_index]] <- curr_met_file
  update_glm_nml_names[list_index] <- "meteo_fl"
  list_index <- list_index + 1
  
  update_nml(update_glm_nml_list, 
             update_glm_nml_names, 
             working_directory, 
             "glm3.nml")
  
  tmp <- file.copy(from = inflow_file_names[inflow_outflow_index, 1], 
                   to = "inflow_file1.csv", overwrite = TRUE)
  tmp <- file.copy(from = inflow_file_names[inflow_outflow_index, 2], 
                   to = "inflow_file2.csv", overwrite = TRUE)
  tmp <- file.copy(from = outflow_file_names[inflow_outflow_index], 
                   to = "outflow_file1.csv", overwrite = TRUE)      
  
  #Use GLM NML files to run GLM for a day
  # Only allow simulations without NaN values in the output to proceed. 
  #Necessary due to random Nan in AED output
  pass <- FALSE
  num_reruns <- 0
  
  if(i == 2 & m == 1){
    file.copy(from = paste0(working_directory, "/", "glm3.nml"), #GLM SPECIFIC
              to = paste0(working_directory, "/", "glm3_initial.nml"), 
              overwrite = TRUE) #GLM SPECIFIC
  }
  
  while(!pass){
    unlink(paste0(working_directory, "/output.nc")) 
    
    if(machine == "unix" | machine == "mac"){
      system2(paste0(working_directory, "/", "glm"), 
              stdout = FALSE, 
              stderr = FALSE,
              env = paste0("DYLD_LIBRARY_PATH=",working_directory))
    }else if(machine == "windows"){
      system2(paste0(working_directory, "/", "glm.exe"), 
              invisible = FALSE)
    }else{
      print("Machine not identified")
      stop()
    }
    
    if(file.exists(paste0(working_directory, "/output.nc")) & 
       !has_error(nc <- nc_open(paste0(working_directory, "/output.nc")))){
      
      if(length(ncvar_get(nc, "time")) > 1){
        nc_close(nc)
        
        output_vars <- c(glm_output_vars)
        
        GLM_temp_wq_out <- get_glm_nc_var_all_wq(ncFile = "/output.nc",
                                                 working_dir = working_directory,
                                                 z_out = modeled_depths,
                                                 vars = output_vars,
                                                 diagnostic_vars = diagnostics_names)
        
        num_glm_depths <- length(GLM_temp_wq_out$depths_enkf)
        glm_temps <- rev(GLM_temp_wq_out$output[ ,1])
        glm_depths_end[1:num_glm_depths] <- GLM_temp_wq_out$depths_enkf
        
        glm_depths_tmp <- c(GLM_temp_wq_out$depths_enkf,GLM_temp_wq_out$surface_height)
        
        glm_depths_mid <- glm_depths_tmp[1:(length(glm_depths_tmp)-1)] + diff(glm_depths_tmp)/2
        
        x_star_end[1:ndepths_modeled] <- approx(glm_depths_mid,glm_temps, modeled_depths, rule = 2)$y
        
        if(include_wq){
          for(wq in 1:num_wq_vars){
            glm_wq <-  rev(GLM_temp_wq_out$output[ ,1+wq])
            x_star_end[wq_start[wq]:wq_end[wq]] <- approx(glm_depths_mid,glm_wq, modeled_depths, rule = 2)$y
          }
        }
        
        if(length(diagnostics_names) > 0){
          for(wq in 1:length(diagnostics_names)){
            glm_wq <-  rev(GLM_temp_wq_out$diagnostics_output[ , wq])
            diagnostics[ , wq] <- approx(glm_depths_mid,glm_wq, modeled_depths, rule = 2)$y
          }
        }
        
        if(length(which(is.na(x_star_end))) == 0){
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
    
    return(list(x_star_end  = x_star_end,
                surface_height_end  = GLM_temp_wq_out$surface_height, 
                snow_ice_thickness_end  = GLM_temp_wq_out$snow_wice_bice,
                avg_surf_temp_end  = GLM_temp_wq_out$avg_surf_temp,
                mixing_vars_end = GLM_temp_wq_out$mixing_vars,
                diagnostics_end  = diagnostics,
                glm_depths_end  = glm_depths_end))
  }
}


set_up_model <- function(code_folder, 
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
                         include_wq){
                         
                         
  GLM_folder <- paste0(code_folder, "/", "glm", "/", machine) 
  fl <- c(list.files(GLM_folder, full.names = TRUE))
  tmp <- file.copy(from = fl, to = working_directory, overwrite = TRUE)
  
  file.copy(from = paste0(base_GLM_nml), 
            to = paste0(working_directory, "/", "glm3.nml"), overwrite = TRUE)
  
  update_var(num_wq_vars, "num_wq_vars", working_directory, "glm3.nml") #GLM SPECIFIC
  
  if(include_wq){
    
    file.copy(from = paste0(base_AED_nml), 
              to = paste0(working_directory, "/", "aed2.nml"), overwrite = TRUE)
    file.copy(from = paste0(base_AED_phyto_pars_nml), 
              to = paste0(working_directory, "/", "aed2_phyto_pars.nml"), overwrite = TRUE)
    file.copy(from = paste0(base_AED_zoop_pars_nml), 
              to = paste0(working_directory, "/", "aed2_zoop_pars.nml"), overwrite = TRUE)
  }
  
  update_var(ndepths_modeled, "num_depths", working_directory, "glm3.nml") #GLM SPECIFIC
  update_var(modeled_depths, "the_depths", working_directory, "glm3.nml") #GLM SPECIFIC
  update_var(rep(the_sals_init, ndepths_modeled), "the_sals", working_directory, "glm3.nml") #GLM SPECIFIC
  
  #Create a copy of the NML to record starting initial conditions
  file.copy(from = paste0(working_directory, "/", "glm3.nml"), #GLM SPECIFIC
            to = paste0(working_directory, "/", "glm3_initial.nml"), overwrite = TRUE) #GLM SPECIFIC
}