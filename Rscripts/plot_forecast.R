plot_forecast <- function(pdf_file_name,
                          output_file,
                          catwalk_fname,
                          include_wq,
                          forecast_days,
                          code_folder,
                          save_location,
                          data_location,
                          plot_summaries,
                          pre_scc,
                          push_to_git,
                          pull_from_git,
                          use_ctd,
                          modeled_depths){
  
  source(paste0(code_folder,'/Rscripts/extract_temp_chain.R'))
  source(paste0(code_folder,'/Rscripts/extract_temp_CTD.R'))
  
  npars <- length(par_names)
  
  nMETmembers <- 21
  
  
  mia_location <- paste0(data_location,'/','mia-data')
  setwd(mia_location)
  if(pull_from_git){
    system(paste0("git pull"))
  }
  catwalk_fname <- paste0(mia_location,'/','Catwalk.csv')
  
  
  
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
  for(par in 1:npars){
  par_list[[par]] <- ncvar_get(nc,par_names_save[par])
  }
  
  if(include_wq){
    wq_output <- array(NA,dim = c(length(wq_names),dim(temp)[1],dim(temp)[2],dim(temp)[3]))
    for(i in 1:length(wq_names)){
      wq_output[i,,,] <- ncvar_get(nc,wq_names[i])
    }
    
    OXY_oxy <- wq_output[which(wq_names == 'OXY_oxy'),,,]
    PHY_TCHLA <- wq_output[which(wq_names == 'PHY_TCHLA'),,,]
    OGM_doc <- wq_output[which(wq_names == 'OGM_doc'),,,]
  }
  
  nc_close(nc)
  
  
  #PROCESS TEMPERATURE OBSERVATIONS
  
  obs_temp <- extract_temp_chain(fname = catwalk_fname,
                                 full_time_local,
                                 modeled_depths = modeled_depths,
                                 observed_depths_temp = observed_depths_temp,
                                 input_file_tz = "EST5EDT",
                                 local_tzone)
  for(i in 1:length(obs_temp$obs[,1])){
    for(j in 1:length(obs_temp$obs[1,])){
      if(obs_temp$obs[i,j] == 0 | is.na(obs_temp$obs[i,j]) | is.nan(obs_temp$obs[i,j])){
        obs_temp$obs[i,j] = NA
      } 
    }
  }
  
  
  
  #PROCESS DO OBSERVATIONS
  
  obs_do <- extract_do_chain(fname = catwalk_fname,
                             full_time_local,
                             modeled_depths = modeled_depths,
                             observed_depths_do= observed_depths_do,
                             input_file_tz = "EST5EDT", 
                             local_tzone)
  obs_do$obs <- obs_do$obs*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  
  obs_chla_fdom <- extract_chla_chain(fname = catwalk_fname,
                                      full_time_local,
                                      modeled_depths = modeled_depths,
                                      observed_depths_chla_fdom,
                                      input_file_tz = "EST5EDT", 
                                      local_tzone)
  #DIRRRRTY qsu -> mg/L ->  mmol/m3
  #Need to fix
  obs_chla_fdom$fDOM_obs <- obs_chla_fdom$fDOM_obs*1000/(12*6)  
  
  #Use the CTD observation rather than the sensor string when CTD data is avialable
  if(use_ctd){
    ## LOOK AT CTD DATA
    fl <- c(list.files('/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SCC_data/preSCC/', pattern = 'CTD', full.names = TRUE))
    #NEED TO DOUBLE CHECK TIME ZONE
    obs_ctd <- extract_temp_CTD(fname = fl[1],
                                full_time_day_local,
                                modeled_depths = modeled_depths,
                                input_file_tz = "EST5EDT",
                                local_tzone)
    
    obs_ctd$obs_do <- obs_ctd$obs_do*1000/32
    for(i in 1:length(full_time_day_local)){
      if(!is.na(obs_ctd$obs_temp[i, 1])){
        obs_temp$obs[i,] <- obs_ctd$obs_temp[i, ]
        obs_do$obs[i,] <- obs_ctd$obs_do[i, ]
        obs_chla_fdom$Chla_obs[i,] <- obs_ctd$obs_chla[i, ]
      }
    }
  }
  
  
  
  nsteps <- length(full_time_local)
  if(length(which(forecasted == 1))>0){
    forecast_index <- which(forecasted == 1)[1]
  }else{
    forecast_index <- 0
  }
  nlayers <- length(depths)
  
  if(include_wq){
    #NEED TO ADD ADDITIONAL OBSERVATION TYPES
    nobs <- length(observed_depths_temp) + 
      length(observed_depths_do) + 
      length(observed_depths_chla_fdom) + 
      length(observed_depths_chla_fdom) 
  }else{
    nobs <- length(observed_depths_temp)
  }
  #Observations for each observed state at each time step
  #an observation with at least 1 observation but without an observation in a time-step gets assigned an NA
  #z <- t(matrix(rep(NA,nobs), nrow = nobs, ncol = nsteps))
  
  if(include_wq){
    z <- cbind(obs_temp$obs,obs_do$obs, obs_chla_fdom$fDOM_obs, obs_chla_fdom$Chla_obs)
  }else{
    z <- cbind(obs_temp$obs) 
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
    ylim = range(c(temp_mean[,],temp_upper[,],temp_lower[,],c(z[,1:length(observed_depths_temp)])),na.rm = TRUE) 
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
      
      obs <- obs_do$obs[,i]
      ylim = range(c(OXY_oxy[,,]),na.rm = TRUE) 
      
      plot(full_time_local,OXY_oxy[,1,model],type='l',ylab='Oxygen (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
      if(length(temp[1,,model]) > 1){
        for(m in 2:length(temp[1,,model])){
          points(full_time_local,OXY_oxy[,m,model],type='l')
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
      obs <- obs_chla_fdom$Chla_obs[,i]
      ylim = range(c(PHY_TCHLA[,,]),na.rm = TRUE) 
      plot(full_time_local,PHY_TCHLA[,1,model],type='l',ylab='PHY_TCHLA (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
      if(length(temp[1,,model]) > 1){
        for(m in 2:length(temp[1,,model])){
          points(full_time_local,PHY_TCHLA[,m,model],type='l')
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
      obs <- obs_chla_fdom$fDOM_obs[,i]
      ylim = range(c(OGM_doc[,,]),na.rm = TRUE) 
      plot(full_time_local,OGM_doc[,1,model],type='l',ylab='OGM_doc (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
      if(length(temp[1,,model]) > 1){
        for(m in 2:length(temp[1,,model])){
          points(full_time_local,OGM_doc[,m,model],type='l')
        }
      }
      obs[is.na(obs)] = -999
      points(full_time_local,obs,col='red',pch=19,cex=1.0)
      
      if(forecast_index > 1){
        abline(v = full_time_local[forecast_index-1])
      }
    }
    
    par(mfrow=c(4,3))
    for(wq in 1:length(wq_names)){
      if(!wq_names[wq]  %in% c("PHY_TCHLA","OGM_doc","OXY_oxy")){
        
        for(i in focal_depths_wq){
          ylim = range(c(wq_output[wq,,,]),na.rm = TRUE) 
          plot(full_time_local,wq_output[wq,,1,i],type='l',ylab=wq_names[wq],xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          for(m in 2:length(temp[1,,model])){
            points(full_time_local,wq_output[wq,,m,i],type='l')
          }
          if(forecast_index > 1){
            abline(v = full_time_local[forecast_index-1])
          }
        }
      }
    }
    
  }
  
  dev.off()
  
  if(forecast_days == 16){
    
    full_time_local_past <- seq(full_time_local[1] - days(5), full_time_local[2], by = "1 day") # grid
    full_time_local_combined <- seq(full_time_local_past[1], full_time_local[length(full_time_local)], by = "1 day")
    full_time_local_plotting <- seq(full_time_local_past[1]-days(3), full_time_local[length(full_time_local)]+days(5), by = "1 day")
    
    obs_temp <- extract_temp_chain(fname = catwalk_fname,
                                   full_time_local = full_time_local_past,
                                   modeled_depths = modeled_depths,
                                   observed_depths_temp = observed_depths_temp,
                                   input_file_tz = "EST5EDT",
                                   local_tzone)
    for(i in 1:length(obs_temp$obs[,1])){
      for(j in 1:length(obs_temp$obs[1,])){
        if(obs_temp$obs[i,j] == 0 | is.na(obs_temp$obs[i,j]) | is.nan(obs_temp$obs[i,j])){
          obs_temp$obs[i,j] = NA
        } 
      }
    }
    init_temps <- obs_temp$obs[1,]
    
    forecast_index <- which(forecasted == 1)[1]
    nlayers <- length(depths)
    
    focal_depths <- focal_depths_manager
    png( paste0(save_location,'/',pdf_file_name, "_management.png"),width = 12, height = 6,units = 'in',res=300)
    par(mfrow=c(1,2))
    
    #PLOT OF TURNOVER PROBABILITY
    
    prob_zero <- rep(NA,length(seq(3,18,1)))
    for(i in 3:18){
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
    
    plot(full_time_local_plotting,rep(-99,length(full_time_local_plotting)),ylim=c(-5,35),xlab = 'date',ylab = expression(~degree~C))
    title(paste0('Water temperature forecast'),cex.main=0.9)
    tmp_day <- full_time_local[-1][1]
    axis(1, at=full_time_local_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
    depth_colors_index = 0
    for(i in 1:length(depths)){
      if(length(which(depths[i]  == observed_depths_temp)) >= 1 ){
        depth_colors_index <- i
        points(full_time_local_past, obs_temp$obs[1:length(full_time_local_past),i],type='l',col=depth_colors[depth_colors_index],lwd=1.5)
        index <- which(obs_index[i]  == focal_depths)
        if(length(index) == 1){
          points(full_time_local[-1], temp_mean[-1,obs_index[i]],type='l',lty='dashed',col=depth_colors[depth_colors_index],lwd=1.5) 
          points(full_time_local[-1], temp_upper[-1,obs_index[i]],type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=1.5)
          points(full_time_local[-1], temp_lower[-1,obs_index[i]],type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=1.5)
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
    
    jpeg(paste0(save_location,'/TO_',pdf_file_name, ".png"),width = 6, height = 6,units = 'in',res=300)
    plot(full_time_local_plotting,rep(-99,length(full_time_local_plotting)),ylim=c(0,100),xlab = 'date',ylab = '% chance')
    title('Turnover forecast',cex.main=0.9)
    points(full_time_local[3:18],prob_zero,type='o',ylim=c(0,100),xlab = 'date',ylab = 'Probablity of turnover')
    axis(1, at=full_time_local_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
    abline(v = full_time_local_past[length(full_time_local_past)])
    text(full_time_local_past[length(full_time_local_past)-2],80,'past')
    text(full_time_local[4],80,'future')
    abline(v = as.POSIXct('2018-10-21 20:00:00 EDT', tz = 'EST5EDT'),col='red')
    dev.off()
    
    if(push_to_git){
      setwd(save_location)
      
      file.copy(from = paste0(save_location,'/',pdf_file_name, "_management.png"), to = paste0(save_location,'/','Current_forecast.png'),overwrite=TRUE)
      system(paste0('git add ', pdf_file_name, ".pdf"))
      system(paste0('git add Current_forecast.png'))
      system('git commit -m "forecast and plots"')
      system('git push')
    }
  }
}

