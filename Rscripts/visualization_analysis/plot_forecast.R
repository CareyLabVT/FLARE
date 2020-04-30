plot_forecast <- function(pdf_file_name,
                          output_file,
                          catwalk_fname,
                          include_wq,
                          forecast_days,
                          code_folder,
                          save_location,
                          data_location,
                          plot_summaries,
                          push_to_git,
                          pull_from_git,
                          modeled_depths){
  
  
  source(paste0(code_folder,"/","Rscripts/extract_observations.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/in_situ_qaqc.R"))  
  
  npars <- length(par_names)
  
  nMETmembers <- 21
  
  ## Convert depths to modeled depths index
  
  for(i in 1:length(focal_depths_wq)){
    focal_depths_wq[i] <- which.min(abs(modeled_depths - focal_depths_wq[i]))
  }
  
  for(i in 1:length(focal_depths_manager)){
    focal_depths_manager[i]  <- which.min(abs(modeled_depths - focal_depths_manager[i]))
  }
  
  turnover_index_1 <-  which.min(abs(modeled_depths - turnover_index_1))
  turnover_index_2 <-  which.min(abs(modeled_depths - turnover_index_2))
  
  ####
  
  mia_location <- paste0(data_location,'/','mia-data')
  setwd(mia_location)
  if(pull_from_git){
    system(paste0("git pull"))
  }
  catwalk_fname <- paste0(mia_location,'/','Catwalk.csv')
  
  wq_names_w_phytos <- c(wq_names, tchla_components_vars)
  
  nc <- nc_open(output_file)
  t <- ncvar_get(nc,'time')
  local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
  full_time_local <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = local_tzone)
  full_time_day_local <- as_date(full_time_local)
  temp_mean <- ncvar_get(nc,'temp_mean')
  temp <- ncvar_get(nc,'temp')
  temp_upper <- ncvar_get(nc,'temp_upperCI')
  temp_lower  <- ncvar_get(nc,'temp_lowerCI')
  depths <- ncvar_get(nc,'z')
  forecasted <- ncvar_get(nc,'forecasted')
  par_list <- list()
  if(npars > 0){
    for(par in 1:npars){
      par_list[[par]] <- ncvar_get(nc,par_names_save[par])
    }
  }
  
  if(include_wq ){
    if("PHY_TCHLA" %in% wq_names){
      wq_output <- array(NA,dim = c(length(wq_names_w_phytos),dim(temp)[1],dim(temp)[2],dim(temp)[3]))
      for(i in 1:length(wq_names_w_phytos)){
        wq_output[i,,,] <- ncvar_get(nc,wq_names_w_phytos[i])
      }
      
      OXY_oxy <- wq_output[which(wq_names_w_phytos == 'OXY_oxy'),,,]
      PHY_TCHLA <- wq_output[which(wq_names_w_phytos == 'PHY_TCHLA'),,,]
      OGM_doc <- wq_output[which(wq_names_w_phytos == 'OGM_doc'),,,]
      
      NIT_amm <- wq_output[which(wq_names_w_phytos == 'NIT_amm'),,,]
      NIT_nit <- wq_output[which(wq_names_w_phytos == 'NIT_nit'),,,]
      PHS_frp <- wq_output[which(wq_names_w_phytos == 'PHS_frp'),,,]
    }else{
      wq_output <- array(NA,dim = c(length(wq_names),dim(temp)[1],dim(temp)[2],dim(temp)[3]))
      for(i in 1:length(wq_names)){
        wq_output[i,,,] <- ncvar_get(nc,wq_names[i])
      }
      OXY_oxy <- wq_output[which(wq_names_w_phytos == 'OXY_oxy'),,,]
    }
  }
  
  nc_close(nc)
  
  
  #PROCESS TEMPERATURE OBSERVATIONS
  
  cleaned_observations_file_long <- paste0(save_location, 
                                           "/observations_postQAQC_long.csv")
  
  in_situ_qaqc(insitu_obs_fname = insitu_obs_fname, 
               data_location = data_location, 
               maintenance_file = maintenance_file,
               ctd_fname = ctd_fname, 
               nutrients_fname = nutrients_fname,
               cleaned_observations_file_long = cleaned_observations_file_long,
               lake_name,
               code_folder)
  
  #####
  
  print("Extracting temperature observations")
  obs_temp <- extract_observations(fname = cleaned_observations_file_long,
                                   full_time_local,
                                   modeled_depths = modeled_depths,
                                   local_tzone,
                                   target_variable = "temperature",
                                   time_threshold_seconds = time_threshold_seconds_temp,
                                   distance_threshold_meter = distance_threshold_meter,
                                   methods = temp_methods)
  
  if(include_wq){
    
    print("Extracting DO observations")
    obs_do <- extract_observations(fname = cleaned_observations_file_long,
                                   full_time_local,
                                   modeled_depths = modeled_depths,
                                   local_tzone,
                                   target_variable = "oxygen",
                                   time_threshold_seconds = time_threshold_seconds_oxygen,
                                   distance_threshold_meter = distance_threshold_meter,
                                   methods = do_methods)
    
    print("Extracting Chl-a observations")
    obs_chla <- extract_observations(fname = cleaned_observations_file_long,
                                     full_time_local,
                                     modeled_depths = modeled_depths,
                                     local_tzone,
                                     target_variable = "chla",
                                     time_threshold_seconds = time_threshold_seconds_chla,
                                     distance_threshold_meter = distance_threshold_meter,
                                     methods = chla_methods)
    
    print("Extracting fdom observations")
    obs_fdom <- extract_observations(fname = cleaned_observations_file_long,
                                     full_time_local,
                                     modeled_depths = modeled_depths,
                                     local_tzone,
                                     target_variable = "fdom",
                                     time_threshold_seconds = time_threshold_seconds_fdom,
                                     distance_threshold_meter = distance_threshold_meter,
                                     methods = fdom_methods)
    
    print("Extracting NH4 observations")
    obs_NH4 <- extract_observations(fname = cleaned_observations_file_long,
                                    full_time_local,
                                    modeled_depths = modeled_depths,
                                    local_tzone,
                                    target_variable = "NH4",
                                    time_threshold_seconds = time_threshold_seconds_nh4,
                                    distance_threshold_meter = distance_threshold_meter,
                                    methods = nh4_methods)
    
    print("Extracting NO3 observations")
    obs_NO3 <- extract_observations(fname = cleaned_observations_file_long,
                                    full_time_local,
                                    modeled_depths = modeled_depths,
                                    local_tzone,
                                    target_variable = "NO3NO2",
                                    time_threshold_seconds = time_threshold_seconds_no3,
                                    distance_threshold_meter = distance_threshold_meter,
                                    methods = no3_methods)
    
    print("Extracting SRP observations")
    obs_SRP <- extract_observations(fname = cleaned_observations_file_long,
                                    full_time_local,
                                    modeled_depths = modeled_depths,
                                    local_tzone,
                                    target_variable = "SRP",
                                    time_threshold_seconds = time_threshold_seconds_srp,
                                    distance_threshold_meter = distance_threshold_meter,
                                    methods = srp_methods)
  }
  
  
  nsteps <- length(full_time_local)
  if(length(which(forecasted == 1))>0){
    forecast_index <- which(forecasted == 1)[1]
  }else{
    forecast_index <- 0
  }
  nlayers <- length(depths)
  
  if(include_wq){
    z <- cbind(obs_temp,obs_do, obs_fdom, obs_chla)
  }else{
    z <- cbind(obs_temp) 
  }
  
  num_wq_vars <- length(wq_names) 
  if(include_wq){
    temp_start <- 1
    temp_end <- length(modeled_depths)
    wq_start <- rep(NA,num_wq_vars)
    wq_end <- rep(NA,num_wq_vars)
    for(wq in 1:num_wq_vars){
      if(wq == 1){
        wq_start[wq] <- temp_end+1
        wq_end[wq] <- temp_end + (length(modeled_depths))
      }else{
        wq_start[wq] <- wq_end[wq-1]+1
        wq_end[wq] <- wq_end[wq-1] + (length(modeled_depths))
      }
    }
  }else{
    temp_start <- 1
    temp_end <- length(modeled_depths)
  }
  
  #FIGURE OUT WHICH DEPTHS HAVE OBSERVATIONS
  if(include_wq){
    obs_index <- rep(NA,length(modeled_depths)*(num_wq_vars+1))
    obs_index[1:length(modeled_depths)] <- seq(1,length(modeled_depths),1)
    for(wq in 1:num_wq_vars){
      obs_index[wq_start[wq]:wq_end[wq]] <- seq(wq_start[wq],wq_end[wq],1)
    }
  }else{
    obs_index <- rep(NA,length(modeled_depths))
    obs_index[1:length(modeled_depths)] <- seq(1,length(modeled_depths),1)
  }
  
  
  #Matrix for knowing which state the observation corresponds to
  z_states <- t(matrix(obs_index, nrow = length(obs_index), ncol = nsteps))
  
  pdf(paste0(save_location,'/',pdf_file_name, ".pdf"),width = 12, height = 12)
  par(mfrow=c(4,3))
  
  for(i in 1:nlayers){
    model = i
    if(length(which(z_states[1,] == i) > 0)){
      obs = which(z_states[1,] == i)
    }else{
      obs = NA
    }
    ylim = range(c(temp_mean[,],temp_upper[,],temp_lower[,],c(z[,1:length(modeled_depths)])),na.rm = TRUE) 
    #ylim = range(c(temp_mean[,model],temp_upper[,model],temp_lower[,model],c(z[,obs])),na.rm = TRUE)
    if(plot_summaries){
      plot(full_time_local,temp_mean[,model],type='l',ylab=expression(~degree~C),xlab='Day',main = paste('Depth: ',depths[i],' m',sep=''),ylim=ylim)
      points(full_time_local,temp_upper[,model],type='l',lty='dashed')
      points(full_time_local,temp_lower[,model],type='l',lty='dashed')
    }else{
      plot(full_time_local,temp[,1,model],type='l',ylab=expression(~degree~C),xlab='Day',main = paste('Depth: ',depths[i],' m',sep=''),ylim=ylim)
      if(length(temp[1,,model]) > 1){
        for(m in 2:length(temp[1,,model])){
          points(full_time_local,temp[,m,model],type='l')
        }
      }
    }
    if(!is.na(obs)){
      tmp = z[,obs]
      tmp[is.na(tmp)] = -999
      points(full_time_local,tmp,col='red',pch=19,cex=1.0)
    }
    if(forecast_index > 1){
      abline(v = full_time_local[forecast_index-1])
    }
  }
  
  ###PLOT OF PARAMETERS IF FIT
  par(mfrow=c(4,3))
  if(npars > 0){
    for(par in 1:npars){
      plot(full_time_local,rowMeans(par_list[[par]][,]),xlab ='Day',ylab = par_names_save[par],type='l',ylim = range(c(par_list[[par]][,]),na.rm=TRUE))
      if(length(par_list[[par]][1,]) > 1){
        for(m in 1:length(par_list[[par]][1,])){
          points(full_time_local,par_list[[par]][,m],type='l')
        }
      }
    }
  }
  
  if(include_wq){
    par(mfrow=c(4,3))
    
    for(i in 1:nlayers){
      model = i
      
      obs <- obs_do[,i]
      ylim = range(c(OXY_oxy[,,]),na.rm = TRUE) 
      if(plot_summaries){
        mean_oxy <- array(NA,dim=c(length(t),length(depths)))
        upper95_oxy <- array(NA,dim=c(length(t),length(depths)))
        lower95_oxy <- array(NA,dim=c(length(t),length(depths)))
        for(ii in 1:length(depths)){
          for(j in 1:length(t)){
            mean_oxy[j,ii] <- mean(OXY_oxy[j,,ii])
            lower95_oxy[j,ii] <- quantile(OXY_oxy[j,,ii],0.025)
            upper95_oxy[j,ii] <- quantile(OXY_oxy[j,,ii],0.975)
          }
        }
        plot(full_time_local,mean_oxy[ ,model],type='l',ylab='Oxygen (mmol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
        points(full_time_local,lower95_oxy[ ,model],type='l',lty='dashed')
        points(full_time_local,upper95_oxy[ ,model],type='l',lty='dashed')   
        
      }else{
        plot(full_time_local,OXY_oxy[,1,model],type='l',ylab='Oxygen (mmol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
        if(length(temp[1,,model]) > 1){
          for(m in 2:length(temp[1,,model])){
            points(full_time_local,OXY_oxy[,m,model],type='l')
          }
        }
      }
      
      obs[is.na(obs)] = -999
      points(full_time_local,obs,col='red',pch=19,cex=1.0)
      
      if(forecast_index > 1){
        abline(v = full_time_local[forecast_index-1])
      }
    }
    
    if("PHY_TCHLA" %in% wq_names){
      
      par(mfrow=c(4,3))
      PHY_TCHLA[,,] <- PHY_TCHLA[,,]
      for(i in 1:nlayers){
        model = i
        obs <- obs_chla[,i]
        
        ylim = range(c(PHY_TCHLA[,,]),na.rm = TRUE) 
        if(plot_summaries){
          mean_TCHLA <- array(NA,dim=c(length(t),length(depths)))
          upper95_TCHLA <- array(NA,dim=c(length(t),length(depths)))
          lower95_TCHLA <- array(NA,dim=c(length(t),length(depths)))
          for(ii in 1:length(depths)){
            for(j in 1:length(t)){
              mean_TCHLA[j,ii] <- mean(PHY_TCHLA[j,,ii])
              lower95_TCHLA[j,ii] <- quantile(PHY_TCHLA[j,,ii],0.025)
              upper95_TCHLA[j,ii] <- quantile(PHY_TCHLA[j,,ii],0.975)
            }
          }
          plot(full_time_local,mean_TCHLA[,model],type='l',ylab='PHY_TCHLA (ug/L)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          points(full_time_local,lower95_TCHLA[ ,model],type='l',lty='dashed')
          points(full_time_local,upper95_TCHLA[ ,model],type='l',lty='dashed')   
        }else{
          plot(full_time_local,PHY_TCHLA[,1,model],type='l',ylab='PHY_TCHLA (ug/L)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          if(length(temp[1,,model]) > 1){
            for(m in 2:length(temp[1,,model])){
              points(full_time_local,PHY_TCHLA[,m,model],type='l')
            }
          }
        }
        obs[is.na(obs)] = -999
        points(full_time_local,obs,col='red',pch=19,cex=1.0)
        
        if(forecast_index > 1){
          abline(v = full_time_local[forecast_index-1])
        }
      }
      
      
      par(mfrow=c(4,3))
      
      for(i in 1:nlayers){
        model = i
        obs <- obs_fdom[,i]
        ylim = range(c(OGM_doc[,,]),na.rm = TRUE) 
        
        if(plot_summaries){
          mean_doc <- array(NA,dim=c(length(t),length(depths)))
          upper95_doc <- array(NA,dim=c(length(t),length(depths)))
          lower95_doc <- array(NA,dim=c(length(t),length(depths)))
          for(ii in 1:length(depths)){
            for(j in 1:length(t)){
              mean_doc[j,ii] <- mean(OGM_doc[j,,ii])
              lower95_doc[j,ii] <- quantile(OGM_doc[j,,ii],0.025)
              upper95_doc[j,ii] <- quantile(OGM_doc[j,,ii],0.975)
            }
          }
          plot(full_time_local,mean_doc[,model],type='l',ylab='OGM_doc (mmol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          points(full_time_local,lower95_doc[ ,model],type='l',lty='dashed')
          points(full_time_local,upper95_doc[ ,model],type='l',lty='dashed') 
        }else{
          
          plot(full_time_local,OGM_doc[,1,model],type='l',ylab='OGM_doc (mmol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          if(length(temp[1,,model]) > 1){
            for(m in 2:length(temp[1,,model])){
              points(full_time_local,OGM_doc[,m,model],type='l')
            }
          }
        }
        obs[is.na(obs)] = -999
        points(full_time_local,obs,col='red',pch=19,cex=1.0)
        
        if(forecast_index > 1){
          abline(v = full_time_local[forecast_index-1])
        }
      }
      
      par(mfrow=c(4,3))
      
      for(i in 1:nlayers){
        model = i
        obs <- obs_NO3[,i]
        ylim = range(c(NIT_nit[,,]),na.rm = TRUE) 
        
        if(plot_summaries){
          mean_doc <- array(NA,dim=c(length(t),length(depths)))
          upper95_doc <- array(NA,dim=c(length(t),length(depths)))
          lower95_doc <- array(NA,dim=c(length(t),length(depths)))
          for(ii in 1:length(depths)){
            for(j in 1:length(t)){
              mean_doc[j,ii] <- mean(NIT_nit[j,,ii])
              lower95_doc[j,ii] <- quantile(NIT_nit[j,,ii],0.025)
              upper95_doc[j,ii] <- quantile(NIT_nit[j,,ii],0.975)
            }
          }
          plot(full_time_local,mean_doc[,model],type='l',ylab='NIT_nit (mmol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          points(full_time_local,lower95_doc[ ,model],type='l',lty='dashed')
          points(full_time_local,upper95_doc[ ,model],type='l',lty='dashed') 
        }else{
          
          plot(full_time_local,NIT_nit[,1,model],type='l',ylab='NIT_nit (mmol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          if(length(temp[1,,model]) > 1){
            for(m in 2:length(temp[1,,model])){
              points(full_time_local,NIT_nit[,m,model],type='l')
            }
          }
        }

        if(length(which(!is.na(obs))) > 0){
        points(full_time_local,obs,col='red',pch=19,cex=1.0)
        }
        
        if(forecast_index > 1){
          abline(v = full_time_local[forecast_index-1])
        }
      }
      
      par(mfrow=c(4,3))
      
      for(i in 1:nlayers){
        model = i
        obs <- obs_NH4[,i]
        ylim = range(c(NIT_amm[,,]),na.rm = TRUE) 
        
        if(plot_summaries){
          mean_doc <- array(NA,dim=c(length(t),length(depths)))
          upper95_doc <- array(NA,dim=c(length(t),length(depths)))
          lower95_doc <- array(NA,dim=c(length(t),length(depths)))
          for(ii in 1:length(depths)){
            for(j in 1:length(t)){
              mean_doc[j,ii] <- mean(NIT_amm[j,,ii])
              lower95_doc[j,ii] <- quantile(NIT_amm[j,,ii],0.025)
              upper95_doc[j,ii] <- quantile(NIT_amm[j,,ii],0.975)
            }
          }
          plot(full_time_local,mean_doc[,model],type='l',ylab='NIT_amm (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          points(full_time_local,lower95_doc[ ,model],type='l',lty='dashed')
          points(full_time_local,upper95_doc[ ,model],type='l',lty='dashed') 
        }else{
          
          plot(full_time_local,NIT_amm[,1,model],type='l',ylab='NIT_amm (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          if(length(temp[1,,model]) > 1){
            for(m in 2:length(temp[1,,model])){
              points(full_time_local,NIT_amm[,m,model],type='l')
            }
          }
        }
        if(length(which(!is.na(obs))) > 0){
          points(full_time_local,obs,col='red',pch=19,cex=1.0)
        }
        if(forecast_index > 1){
          abline(v = full_time_local[forecast_index-1])
        }
      }    
      
      par(mfrow=c(4,3))
      
      for(i in 1:nlayers){
        model = i
        obs <- obs_SRP[,i]
        ylim = range(c(PHS_frp[,,]),na.rm = TRUE) 
        
        if(plot_summaries){
          mean_doc <- array(NA,dim=c(length(t),length(depths)))
          upper95_doc <- array(NA,dim=c(length(t),length(depths)))
          lower95_doc <- array(NA,dim=c(length(t),length(depths)))
          for(ii in 1:length(depths)){
            for(j in 1:length(t)){
              mean_doc[j,ii] <- mean(PHS_frp[j,,ii])
              lower95_doc[j,ii] <- quantile(PHS_frp[j,,ii],0.025)
              upper95_doc[j,ii] <- quantile(PHS_frp[j,,ii],0.975)
            }
          }
          plot(full_time_local,mean_doc[,model],type='l',ylab='PHS_frp (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          points(full_time_local,lower95_doc[ ,model],type='l',lty='dashed')
          points(full_time_local,upper95_doc[ ,model],type='l',lty='dashed') 
        }else{
          
          plot(full_time_local,PHS_frp[,1,model],type='l',ylab='PHS_frp (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          if(length(temp[1,,model]) > 1){
            for(m in 2:length(temp[1,,model])){
              points(full_time_local,PHS_frp[,m,model],type='l')
            }
          }
        }
        if(length(which(!is.na(obs))) > 0){
          points(full_time_local,obs,col='red',pch=19,cex=1.0)
        }
        if(forecast_index > 1){
          abline(v = full_time_local[forecast_index-1])
        }
      }    
      
      
      
      
      par(mfrow=c(4,3))
      for(wq in 1:length(wq_names_w_phytos)){
        if(!wq_names_w_phytos[wq]  %in% c("PHY_TCHLA","OGM_doc","OXY_oxy","NIT_amm","NIT_nit","PHS_frp")){
          
          for(i in focal_depths_wq){
            model <- i
            ylim = range(c(wq_output[wq,,,]),na.rm = TRUE) 
            if(plot_summaries){
              mean_wq <- array(NA,dim=c(length(t),length(depths)))
              upper95_wq <- array(NA,dim=c(length(t),length(depths)))
              lower95_wq <- array(NA,dim=c(length(t),length(depths)))
              for(ii in 1:length(depths)){
                for(j in 1:length(t)){
                  mean_wq[j,ii] <- mean(wq_output[wq,j,,ii])
                  lower95_wq[j,ii] <- quantile(wq_output[wq,j,,ii],0.025)
                  upper95_wq[j,ii] <- quantile(wq_output[wq,j,,ii],0.975)
                }
              }
              plot(full_time_local,mean_wq[,model],type='l',ylab=wq_names_w_phytos[wq],xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
              points(full_time_local,lower95_wq[ ,model],type='l',lty='dashed')
              points(full_time_local,upper95_wq[ ,model],type='l',lty='dashed')
            }else{
              plot(full_time_local,wq_output[wq,,1,model],type='l',ylab=wq_names_w_phytos[wq],xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
              for(m in 2:length(temp[1,,model])){
                points(full_time_local,wq_output[wq,,m,model],type='l')
              }
            }
            if(forecast_index > 1){
              abline(v = full_time_local[forecast_index-1])
            }
          }
        }
      }
    }
    
  }
  
  dev.off()
  
  forecasted_index <- which(forecasted == 1)
  
  if(length(forecasted_index) == 16 & forecasted_index[1] == 3){
    
    full_time_local_past <- seq(full_time_local[1] - days(5), full_time_local[2], by = "1 day") # grid
    full_time_local_combined <- seq(full_time_local_past[1], full_time_local[length(full_time_local)], by = "1 day")
    full_time_local_plotting <- seq(full_time_local_past[1]-days(3), full_time_local[length(full_time_local)]+days(5), by = "1 day")
    
    print("Extracting temperature observations")
    obs_temp <- extract_observations(fname = cleaned_observations_file_long,
                                     full_time_local_past,
                                     modeled_depths = modeled_depths,
                                     local_tzone,
                                     target_variable = "temperature",
                                     time_threshold_seconds = time_threshold_seconds_temp,
                                     distance_threshold_meter = distance_threshold_meter,
                                     methods = temp_methods)

    
    
    nlayers <- length(depths)
    
    focal_depths <- focal_depths_manager
    png( paste0(save_location,'/',pdf_file_name, "_management.png"),width = 12, height = 6,units = 'in',res=300)
    par(mfrow=c(1,2))
    
    #PLOT OF TURNOVER PROBABILITY
    
    
    prob_zero <- rep(NA,length(seq(3,18,1)))
    for(i in forecasted_index){
      prob_zero[i-2] = 100*length(which(temp[i,,turnover_index_1] - temp[i,,turnover_index_2] < 1))/length((temp[i,,obs_index[1]]))
    }
    
    plot(full_time_local_plotting,rep(-99,length(full_time_local_plotting)),ylim=c(0,100),xlab = 'date',ylab = '% chance')
    title('Turnover forecast',cex.main=0.9)
    
    points(full_time_local[3:18],prob_zero,type='o',ylim=c(0,100),xlab = 'date',ylab = 'Probablity of turnover')
    axis(1, at=full_time_local_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
    abline(v = full_time_local_past[length(full_time_local_past)])
    text(full_time_local_past[length(full_time_local_past)-2],80,'past')
    text(full_time_local[4],80,'future')
    #HISTORICAL AND FUTURE TEMPERATURE
    if(length(depths) == 10){
      depth_colors <- c("firebrick4",
                        "firebrick1",
                        "DarkOrange1",
                        "gold",
                        "greenyellow",
                        "medium sea green",
                        "sea green",
                        "DeepSkyBlue4",
                        "blue2",
                        "blue4")
      
    }else if(length(depths) == 19){
      depth_colors <- c("firebrick4",
                        NA,
                        "firebrick1",
                        NA,
                        "DarkOrange1",
                        NA,
                        "gold",
                        NA,
                        "greenyellow",
                        NA,
                        "medium sea green",
                        NA,
                        "sea green",
                        NA,
                        "DeepSkyBlue4",
                        NA,
                        "blue2",
                        NA,
                        "blue4")
    }else{
      depth_colors <- c("firebrick4",
                        NA,
                        NA,
                        "firebrick1",
                        NA,
                        NA,
                        "DarkOrange1",
                        NA,
                        NA,
                        "gold",
                        NA,
                        NA,
                        "greenyellow",
                        NA,
                        NA,
                        "medium sea green",
                        NA,
                        NA,
                        "sea green",
                        NA,
                        NA,
                        "DeepSkyBlue4",
                        NA,
                        NA,
                        "blue2",
                        NA,
                        NA,
                        "blue4",
                        NA)
    }
    
    
    plot(full_time_local_plotting,rep(-99,length(full_time_local_plotting)),ylim=c(-5,35),xlab = 'date',ylab = expression(~degree~C))
    title(paste0('Water temperature forecast'),cex.main=0.9)
    tmp_day <- full_time_local[-1][1]
    axis(1, at=full_time_local_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
    depth_colors_index = 0
    for(i in 1:length(depths)){
      if(length(which(depths[i]  == modeled_depths)) >= 1 ){
        depth_colors_index <- i
        points(full_time_local_past, obs_temp[1:length(full_time_local_past),i],type='l',col=depth_colors[depth_colors_index],lwd=1.5)
        index <- which(focal_depths == obs_index[i])
        if(length(index) == 1){
          points(full_time_local[-1], temp_mean[-1,i],type='l',lty='dashed',col=depth_colors[depth_colors_index],lwd=1.5)
          points(full_time_local[-1], temp_upper[-1,i],type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=1.5)
          points(full_time_local[-1], temp_lower[-1,i],type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=1.5)
        }
      }
    }
    
    abline(v = full_time_local_past[length(full_time_local_past)])
    text(full_time_local_past[length(full_time_local_past)-2],30,'past')
    text(full_time_local[4],30.1,'future')
    if(temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]] == temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]] |
       temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]] == temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]] |
       temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]] == temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]]){
      text(full_time_local_plotting[length(full_time_local_plotting)-3], temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]], '1m:1660', col='firebrick1')
      text(full_time_local_plotting[length(full_time_local_plotting)-2], temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]], '5m:1647', col='medium sea green')
      text(full_time_local_plotting[length(full_time_local_plotting)-1], temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]], '8m:1637', col='blue2')
    }else{
      text(full_time_local_plotting[length(full_time_local_plotting)-2], temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]], '1m:1660', col='firebrick1')
      text(full_time_local_plotting[length(full_time_local_plotting)-2], temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]], '5m:1647', col='medium sea green')
      text(full_time_local_plotting[length(full_time_local_plotting)-2], temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]], '8m:1637', col='blue2')
    }
    
    legend("left",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m","8m", "9m"),
           text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                      "DeepSkyBlue4", "blue2", "blue4"), cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
    legend('topright', c('mean','confidence bounds'), lwd=1.5, lty=c('dashed','dotted'),bty='n',cex = 1)
    
    mtext(paste0('Falling Creek Reservoir\n',month(tmp_day),'/',day(tmp_day),'/',year(tmp_day)), side = 3, line = -2, outer = TRUE, font = 2)
    dev.off()
    #   
    #   jpeg(paste0(save_location,'/TO_',pdf_file_name, ".png"),width = 6, height = 6,units = 'in',res=300)
    #   plot(full_time_local_plotting,rep(-99,length(full_time_local_plotting)),ylim=c(0,100),xlab = 'date',ylab = '% chance')
    #   title('Turnover forecast',cex.main=0.9)
    #   points(full_time_local[3:18],prob_zero,type='o',ylim=c(0,100),xlab = 'date',ylab = 'Probablity of turnover')
    #   axis(1, at=full_time_local_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
    #   abline(v = full_time_local_past[length(full_time_local_past)])
    #   text(full_time_local_past[length(full_time_local_past)-2],80,'past')
    #   text(full_time_local[4],80,'future')
    #   abline(v = as.POSIXct('2018-10-21 20:00:00 EDT', tz = 'EST5EDT'),col='red')
    #   dev.off()
    #
    
    if(include_wq){
      png(paste0(save_location,'/',pdf_file_name, "_water_quality_management.png"),width = 12, height = 12,units = 'in',res=300)
      par(mfrow=c(3,3), oma = c(5, 2, 5, 2))
      
      for(i in 1:nlayers){
        model = i
        if(i %in% focal_depths_wq){
          obs <- obs_do[,i]
          
          ylim = range(c(OXY_oxy[,,focal_depths_wq]),na.rm = TRUE) 
          if(plot_summaries){
            mean_oxy <- array(NA,dim=c(length(t),length(depths)))
            upper95_oxy <- array(NA,dim=c(length(t),length(depths)))
            lower95_oxy <- array(NA,dim=c(length(t),length(depths)))
            for(ii in 1:length(depths)){
              for(j in 1:length(t)){
                mean_oxy[j,ii] <- mean(OXY_oxy[j,,ii])
                lower95_oxy[j,ii] <- quantile(OXY_oxy[j,,ii],0.025)
                upper95_oxy[j,ii] <- quantile(OXY_oxy[j,,ii],0.975)
              }
            }
            plot(full_time_local,mean_oxy[ ,model],type='l',ylab='Oxygen (mmol/m3)',xlab='day',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
            points(full_time_local,lower95_oxy[ ,model],type='l',lty='dashed')
            points(full_time_local,upper95_oxy[ ,model],type='l',lty='dashed')   
            
          }else{
            plot(full_time_local,OXY_oxy[,1,model],type='l',ylab='Oxygen (mmol/m3)',xlab='day',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
            if(length(temp[1,,model]) > 1){
              for(m in 2:length(temp[1,,model])){
                points(full_time_local,OXY_oxy[,m,model],type='l')
              }
            }
          }
          
          obs[is.na(obs)] = -999
          points(full_time_local[1:forecast_index-1],obs[1:forecast_index-1],col='red',pch=19,cex=1.0)
          
          if(forecast_index > 1){
            abline(v = full_time_local[forecast_index-1])
          }
        }
      }
      
      if("PHY_TCHLA" %in% wq_names){
        for(i in 1:nlayers){
          model = i
          
          if(i %in% focal_depths_wq){
            obs <- obs_chla[,i]
            obs <- obs/biomass_to_chla
            
            
            ylim = range(c(PHY_TCHLA[,,2]),na.rm = TRUE) 
            if(plot_summaries){
              mean_TCHLA <- array(NA,dim=c(length(t),length(depths)))
              upper95_TCHLA <- array(NA,dim=c(length(t),length(depths)))
              lower95_TCHLA <- array(NA,dim=c(length(t),length(depths)))
              for(ii in 1:length(depths)){
                for(j in 1:length(t)){
                  mean_TCHLA[j,ii] <- mean(PHY_TCHLA[j,,ii])
                  lower95_TCHLA[j,ii] <- quantile(PHY_TCHLA[j,,ii],0.025)
                  upper95_TCHLA[j,ii] <- quantile(PHY_TCHLA[j,,ii],0.975)
                }
              }
              plot(full_time_local,mean_TCHLA[,model],type='l',ylab='Chlorophyll a (ug/L)',xlab='day',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
              points(full_time_local,lower95_TCHLA[ ,model],type='l',lty='dashed')
              points(full_time_local,upper95_TCHLA[ ,model],type='l',lty='dashed')   
            }else{
              plot(full_time_local,PHY_TCHLA[,1,model],type='l',ylab='Chlorophyll a (ug/L)',xlab='day',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
              if(length(temp[1,,model]) > 1){
                for(m in 2:length(temp[1,,model])){
                  points(full_time_local,PHY_TCHLA[,m,model],type='l')
                }
              }
            }
            obs[is.na(obs)] = -999
            points(full_time_local[1:forecast_index-1],obs[1:forecast_index-1],col='red',pch=19,cex=1.0)
            
            if(forecast_index > 1){
              abline(v = full_time_local[forecast_index-1])
            }
          }
        }
        plot.new()
        plot.new()
        
        for(i in 1:nlayers){
          model = i
          if(i %in% focal_depths_wq){
            obs <- obs_fdom[,i]
            ylim = range(c(OGM_doc[,,2]),na.rm = TRUE) 
            
            if(plot_summaries){
              mean_doc <- array(NA,dim=c(length(t),length(depths)))
              upper95_doc <- array(NA,dim=c(length(t),length(depths)))
              lower95_doc <- array(NA,dim=c(length(t),length(depths)))
              for(ii in 1:length(depths)){
                for(j in 1:length(t)){
                  mean_doc[j,ii] <- mean(OGM_doc[j,,ii])
                  lower95_doc[j,ii] <- quantile(OGM_doc[j,,ii],0.025)
                  upper95_doc[j,ii] <- quantile(OGM_doc[j,,ii],0.975)
                }
              }
              plot(full_time_local,mean_doc[,model],type='l',ylab='Dissolved Organic Matter (mmol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
              points(full_time_local,lower95_doc[ ,model],type='l',lty='dashed')
              points(full_time_local,upper95_doc[ ,model],type='l',lty='dashed') 
            }else{
              
              plot(full_time_local,OGM_doc[,1,model],type='l',ylab='Dissolved Organic Matter (mmol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
              if(length(temp[1,,model]) > 1){
                for(m in 2:length(temp[1,,model])){
                  points(full_time_local,OGM_doc[,m,model],type='l')
                }
              }
            }
            obs[is.na(obs)] = -999
            points(full_time_local[1:forecast_index-1],obs[1:forecast_index-1],col='red',pch=19,cex=1.0)
            
            if(forecast_index > 1){
              abline(v = full_time_local[forecast_index-1])
            }
          }
        }
        plot.new()
        plot.new()
      }
      tmp_day <- full_time_local[-1][1]
      mtext(paste0('Falling Creek Reservoir (EXPERIMENTAL - RESEARCH ONLY): ',month(tmp_day),'/',day(tmp_day),'/',year(tmp_day),"\n"), side = 3, line = -2, outer = TRUE, font = 2)
      
      dev.off() 
    }
    
    if(push_to_git){
      setwd(save_location)
      if(include_wq){
        #file.copy(from = paste0(save_location,'/',pdf_file_name, "_water_quality_management.png"), to = paste0(save_location,'/','Current_forecast_WQ.png'),overwrite=TRUE)
        #system(paste0('git add Current_forecast_WQ.png'))
      }else{
        file.copy(from = paste0(save_location,'/',pdf_file_name, "_management.png"), to = paste0(save_location,'/','Current_forecast.png'),overwrite=TRUE)
        system(paste0('git add Current_forecast.png'))
        system(paste0('git add ', paste0(pdf_file_name, "_management.png")))
      }
      system(paste0('git add ', pdf_file_name, ".pdf"))
      system(paste0('git add ', pdf_file_name, ".pdf"))
      system('git commit -m "forecast and plots"')
      system('git push')
    }
  }
  
  unlink(cleaned_observations_file_long)
}

