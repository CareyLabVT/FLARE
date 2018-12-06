plot_forecast_netcdf <- function(pdf_file_name,output_file,catwalk_fname,include_wq,code_location,save_location,data_location,plot_summaries,use_ctd){
  library(ncdf4)
  library(lubridate)
  #code_location <- '/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SSC_forecasting/Rscripts'
  source(paste0(code_location,'/extract_temp_chain.R'))
  source(paste0(code_location,'/extract_temp_CTD.R'))
  
  nc <- nc_open(output_file)
  
  t <- ncvar_get(nc,'time')
  
  
  full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = 'EST5EDT')
  full_time_day <- strftime(full_time, format="%Y-%m-%d")
  
  #output_file <-'/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/test_forecast/FCR_hist_2018_9_7_forecast_2018_9_8_201898_9_59.nc'
  #include_wq <- FALSE
  #data_location <- '/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SCC_data'
  the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)
  
  num_pars <- 3
  
  mia_location <- paste0(data_location,'/','mia-data')
  setwd(mia_location)
  system(paste0('git pull'))
  
  catwalk_fname <- paste0(mia_location,'/','Catwalk.csv')
  
  #PROCESS TEMPERATURE OBSERVATIONS
  TempObservedDepths <- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8,9)
  obs_temp <- extract_temp_chain(fname = catwalk_fname,full_time,depths = the_depths_init,observed_depths_temp = TempObservedDepths,input_tz = 'EST5EDT', output_tz = reference_tzone)
  for(i in 1:length(obs_temp$obs[,1])){
    for(j in 1:length(obs_temp$obs[1,])){
      if(obs_temp$obs[i,j] == 0 | is.na(obs_temp$obs[i,j]) | is.nan(obs_temp$obs[i,j])){
        obs_temp$obs[i,j] = NA
      } 
    }
  }
  
  init_temps <- obs_temp$obs[1,]
  
  #PROCESS DO OBSERVATIONS
  DoObservedDepths <- c(1,5,9)
  obs_do <- extract_do_chain(fname = catwalk_fname,full_time,depths = the_depths_init,observed_depths_do= DoObservedDepths,input_tz = 'EST5EDT', output_tz = reference_tzone)
  obs_do$obs <- obs_do$obs*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  init_do1 <- obs_do$obs[1,]
  
  Chla_fDOM_ObservedDepths <- 1
  obs_chla_fdom <- extract_chla_chain(fname = catwalk_fname,full_time,depths = the_depths_init,observed_depths_chla_fdom= Chla_fDOM_ObservedDepths,input_tz = 'EST5EDT', output_tz = reference_tzone)
  
  #Use the CTD observation rather than the sensor string when CTD data is avialable
  if(use_ctd){
    ## LOOK AT CTD DATA
    fl <- c(list.files('/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SCC_data/preSCC/', pattern = 'CTD', full.names = TRUE))
    obs_ctd <- extract_temp_CTD(fname = fl[1],full_time_day,depths = the_depths_init,input_tz = 'EST5EDT', output_tz = reference_tzone)
    obs_ctd$obs_do <- obs_ctd$obs_do*1000/32
    for(i in 1:length(full_time_day)){
      if(!is.na(obs_ctd$obs_temp[i,1])){
        obs_temp$obs[i,] <- obs_ctd$obs_temp[i,]
        obs_do$obs[i,] <- obs_ctd$obs_do[i,]
        obs_chla_fdom$Chla_obs[i,] <- obs_ctd$obs_chla[i,]
      }
    }
  }
  
  temp_mean <- ncvar_get(nc,'temp_mean')
  temp <- ncvar_get(nc,'temp')
  temp_upper <- ncvar_get(nc,'temp_upperCI')
  temp_lower  <- ncvar_get(nc,'temp_lowerCI')
  depths <- ncvar_get(nc,'z')
  Kw <- ncvar_get(nc,'Kw')
  zone1temp <- ncvar_get(nc,'zone1temp')
  zone2temp <- ncvar_get(nc,'zone2temp')
  forecasted <- ncvar_get(nc,'forecasted')
  if(include_wq){
    OXY_oxy <- ncvar_get(nc,'OXY_oxy')
    PHY_CYANOPCH1 <- ncvar_get(nc,'PHY_CYANOPCH1')
  }
  
  nc_close(nc)
  
  nsteps <- length(full_time)
  if(length(which(forecasted == 1))>0){
    forecast_index <- which(forecasted == 1)[1]
  }else{
    forecast_index <- 0
  }
  nlayers <- length(depths)
  
  if(include_wq){
    nobs <- length(TempObservedDepths) + length(DoObservedDepths)
  }else{
    nobs <- length(TempObservedDepths)
  }
  
  #Observations for each observed state at each time step
  #an observation with at least 1 observation but without an observation in a time-step gets assigned an NA
  #z <- t(matrix(rep(NA,nobs), nrow = nobs, ncol = nsteps))
  
  if(include_wq){
    z <- cbind(obs_temp$obs,obs_do$obs)
  }else{
    z <- cbind(obs_temp$obs) 
  }
  
  wq_names <- c('OXY_oxy',
                'CAR_pH','CAR_dic','CAR_ch4',
                'SIL_rsi',
                'NIT_amm', 'NIT_nit',
                'PHS_frp',
                'OGM_doc','OGM_poc','OGM_don','OGM_pon','OGM_dop','OGM_pop',
                'PHY_CYANOPCH1','PHY_CYANONPCH2','PHY_CHLOROPCH3','PHY_DIATOMPCH4',
                'ZOO_COPEPODS1','ZOO_DAPHNIABIG2','ZOO_DAPHNIASMALL3')
  
  num_wq_vars <- length(wq_names) 
  if(include_wq){
    temp_start <- 1
    temp_end <- length(the_depths_init)
    wq_start <- rep(NA,num_wq_vars)
    wq_end <- rep(NA,num_wq_vars)
    for(wq in 1:num_wq_vars){
      if(wq == 1){
        wq_start[wq] <- temp_end+1
        wq_end[wq] <- temp_end + (length(the_depths_init))
      }else{
        wq_start[wq] <- wq_end[wq-1]+1
        wq_end[wq] <- wq_end[wq-1] + (length(the_depths_init))
      }
      
      if(num_pars > 0){
        par1 <- wq_end[num_wq_vars] + 1
        par2 <- par1 + 1
        par3 <-  par2 + 1
      }else{
        par1 <- wq_end[num_wq_vars]
        par2 <- wq_end[num_wq_vars]
        par3 <- wq_end[num_wq_vars]
      }
      
    }
  }else{
    temp_start <- 1
    temp_end <- length(the_depths_init)
    if(num_pars > 0){
      par1 <- temp_end + 1
      par2 <- par1 + 1
      par3 <-  par2 + 1
    }else{
      par1 <- temp_end
      par2 <- temp_end
      par3 <- temp_end
    }
  }
  
  #FIGURE OUT WHICH DEPTHS HAVE OBSERVATIONS
  if(include_wq){
    obs_index <- rep(NA,length(the_depths_init)*(num_wq_vars+1))
    obs_index[1:length(the_depths_init)] <- seq(1,length(the_depths_init),1)
    for(wq in 1:num_wq_vars){
      obs_index[wq_start[wq]:wq_end[wq]] <- seq(wq_start[wq],wq_end[wq],1)
    }
  }else{
    obs_index <- rep(NA,length(the_depths_init))
    obs_index[1:length(the_depths_init)] <- seq(1,length(the_depths_init),1)
  }
  
  
  #Matrix for knowing which state the observation corresponds to
  z_states <- t(matrix(obs_index, nrow = length(obs_index), ncol = nsteps))
  
  
  #print(full_time_day)
  #print(z[1,])
  nMETmembers =21
  pdf(paste0(save_location,'/',pdf_file_name),width = 12, height = 12)
  par(mfrow=c(4,3))
  
  for(i in 1:nlayers){
    model = i
    if(length(which(z_states[1,] == i) > 0)){
      obs = which(z_states[1,] == i)
    }else{
      obs = NA
    }
    ylim = range(c(temp_mean[,],temp_upper[,],temp_lower[,],c(z[,1:length(TempObservedDepths)])),na.rm = TRUE) 
    #ylim = range(c(temp_mean[,model],temp_upper[,model],temp_lower[,model],c(z[,obs])),na.rm = TRUE)
    if(plot_summaries){
      plot(full_time,temp_mean[,model],type='l',ylab=expression(~degree~C),xlab='Day',main = paste('Depth: ',depths[i],' m',sep=''),ylim=ylim)
      points(full_time,temp_upper[,model],type='l',lty='dashed')
      points(full_time,temp_lower[,model],type='l',lty='dashed')
    }else{
      plot(full_time,temp[,1,model],type='l',ylab=expression(~degree~C),xlab='Day',main = paste('Depth: ',depths[i],' m',sep=''),ylim=ylim)
      if(length(temp[1,,model]) > 1){
        for(m in 2:length(temp[1,,model])){
          points(full_time,temp[,m,model],type='l')
        }
      }
    }
    if(!is.na(obs)){
      tmp = z[,obs]
      tmp[is.na(tmp)] = -999
      points(full_time,tmp,col='red',pch=19,cex=1.0)
    }
    if(forecast_index > 1){
      abline(v = full_time[forecast_index-1])
    }
  }
  
  if(include_wq){
    par(mfrow=c(4,3))
    
    for(i in 1:nlayers){
      model = i
      if(length(which(z_states[1,] == length(the_depths_init)+i) > 0)){
        obs = which(z_states[1,] == length(the_depths_init)+i)
      }else{
        obs = NA
      }
      ylim = range(c(OXY_oxy[,,]),na.rm = TRUE) 
      #ylim = range(c(temp_mean[,model],temp_upper[,model],temp_lower[,model],c(z[,obs])),na.rm = TRUE)
      #if(plot_summaries){
      #  plot(full_time,temp_mean[,model],type='l',ylab='water temperature (celsius)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
      #  points(full_time,temp_upper[,model],type='l',lty='dashed')
      #  points(full_time,temp_lower[,model],type='l',lty='dashed')
      #}else{
      plot(full_time,OXY_oxy[,1,model],type='l',ylab='Oxygen (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
      if(length(temp[1,,model]) > 1){
        for(m in 2:length(temp[1,,model])){
          points(full_time,OXY_oxy[,m,model],type='l')
        }
      }
      
      if(!is.na(obs)){
        tmp = z[,obs]
        tmp[is.na(tmp)] = -999
        points(full_time,tmp,col='red',pch=19,cex=1.0)
      }
      
      if(forecast_index > 1){
        abline(v = full_time[forecast_index-1])
      }
    }
    
    par(mfrow=c(4,3))
    
    for(i in 1:nlayers){
      model = i
      if(length(which(z_states[1,] == length(the_depths_init)+i) > 0)){
        obs = which(z_states[1,] == length(the_depths_init)+i)
      }else{
        obs = NA
      }
      ylim = range(c(PHY_CYANOPCH1[,,]),na.rm = TRUE) 
      #ylim = range(c(temp_mean[,model],temp_upper[,model],temp_lower[,model],c(z[,obs])),na.rm = TRUE)
      #if(plot_summaries){
      #  plot(full_time,temp_mean[,model],type='l',ylab='water temperature (celsius)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
      #  points(full_time,temp_upper[,model],type='l',lty='dashed')
      #  points(full_time,temp_lower[,model],type='l',lty='dashed')
      #}else{
      plot(full_time,PHY_CYANOPCH1[,1,model],type='l',ylab='PHY_CYANOPCH1 (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
      if(length(temp[1,,model]) > 1){
        for(m in 2:length(temp[1,,model])){
          points(full_time,PHY_CYANOPCH1[,m,model],type='l')
        }
      }
      
      #      if(!is.na(obs)){
      #        tmp = z[,obs]
      #        tmp[is.na(tmp)] = -999
      #        points(full_time,tmp,col='red',pch=19,cex=1.0)
      #      }
      
      if(forecast_index > 1){
        abline(v = full_time[forecast_index-1])
      }
    }
  }
  
  
  
  
  ###PLOT OF PARAMETERS IF FIT
  plot.new()
  plot(rowMeans(Kw[,]),xlab ='Day',ylab = 'Kw parameter',type='l')
  plot(rowMeans(zone1temp[,]),xlab ='Day',ylab = 'Zone 1 sediment temp',type='l')
  plot(rowMeans(zone2temp[,]),xlab ='Day',ylab = 'Zone 2 sediment temp',type='l')
  
  
  ###PLOT HISTOGRAMS OF FORECAST
  par(mfrow=c(4,3))
  if(!is.na(forecast_index)){
    if(length(which(forecast_index == 1)) > 6){
      xlim<- range(c(temp[forecast_index+7,,obs_index[1]],z[forecast_index,1]),na.rm = TRUE)
      hist(temp[forecast_index+7,,obs_index[1]],main='0.1m temp. 7 days forecast',xlab='Temperature',xlim=xlim)
      abline(v= z[forecast_index+7,1],col='red')
      xlim<- range(c(temp[forecast_index+7,,obs_index[5]],z[forecast_index+7,5]),na.rm = TRUE)
      hist(temp[forecast_index+7,,obs_index[5]],main='4m temp. 7 days forecast',xlab='Temperature',xlim=xlim)
      abline(v= z[forecast_index+7,5],col='red')
      xlim<- range(c(temp[forecast_index+7,,obs_index[10]],z[forecast_index+7,10]),na.rm = TRUE)
      hist(temp[forecast_index+7,,obs_index[10]],main='9m temp. 7 days forecast',xlab='Temperature',xlim=xlim)
      abline(v= z[forecast_index+7,10],col='red')
    }
    if(length(which(forecast_index == 1)) > 13){
      xlim<- range(c(temp[forecast_index+14,,obs_index[1]],z[forecast_index+14,1]),na.rm = TRUE)
      hist(temp[forecast_index+14,,obs_index[1]],main='0.1m temp. 14 days forecast',xlab='Temperature',xlim=xlim)
      abline(v= z[forecast_index+14,1],col='red')
      xlim<- range(c(temp[forecast_index+14,,obs_index[5]],z[forecast_index+14,5]),na.rm = TRUE)
      hist(temp[forecast_index+14,,obs_index[5]],main='4m temp. 14 days forecast',xlab='Temperature',xlim=xlim)
      abline(v= temp[forecast_index+14,5],col='red')
      xlim<- range(c(temp[forecast_index+14,,obs_index[10]],z[forecast_index+14,10]),na.rm = TRUE)
      hist(temp[forecast_index+14,,obs_index[10]],main='9m temp. 14 days forecast',xlab='Temperature',xlim=xlim)
      abline(v= z[forecast_index+14,10],col='red')
    }
  }
  
  #par(mfrow=c(3,5))
  #for(i in 3:17){
  #  xlim = range(c(temp[,,obs_index[1]] - temp[,,obs_index[9]]))
  #  prob_zero = length(which(temp[i,,obs_index[1]] - temp[i,,obs_index[9]] < 1))/length((temp[i,,obs_index[1]]))
  #  plot(density(temp[i,,obs_index[1]] - temp[i,,obs_index[9]]), main = paste0(month(full_time_day[i]),'-',day(full_time_day[i]),' (Tover = ',prob_zero*100, '% chance)'),xlab = '1 m - 8 m temperature',xlim=xlim)
  #}
  
  dev.off()
}

