evaluate_forecast <- function(forecast_folder,Folder,sim_name = '2018_7_6',forecast_location){
  ###LOAD FORECAST FOR ANALYSIS
  #sim_name = '2018_7_6'
  #forecast_folder = 'forecast_2018_7_6_2018726_12_9'
  #Folder = '/Users/quinn/Dropbox/Research/SSC_forecasting/SSC_forecasting/'
  if(is.na(forecast_location)){
  forecast_folder = paste0(Folder,'/','Forecasts','/',forecast_folder)
  }else{
    forecast_folder = paste0(forecast_location,'/',forecast_folder)
  }
  evaluation_folder <- paste0(forecast_folder,'/','evaluation')
  load(file = paste0(forecast_folder,'/',sim_name,'_EnKF_output.Rdata'))
  
  switch(Sys.info() [['sysname']],
         Linux = { machine <- 'unix' },
         Darwin = { machine <- 'mac' })
  
  workingGLM <- evaluation_folder
  if(!dir.exists(evaluation_folder)){ 
  dir.create(evaluation_folder)
  }
  
  full_time_hour_obs <- seq(as.POSIXct(full_time[1]), as.POSIXct(full_time[length(full_time)]), by = "1 hour") # grid
  full_time_day <- strftime(full_time, format="%Y-%m-%d")
  
  ###LOAD SHARE R FUNCTIONS
  source(paste0(Folder,'/','Rscripts/mcmc_enkf_shared_functions.R'))
  source(paste0(Folder,'/','Rscripts/create_obs_met_input.R'))
  source(paste0(Folder,'/','Rscripts/extract_temp_chain.R'))
  source(paste0(Folder,'/','Rscripts/process_GEFS2GLM_v2.R'))
  source(paste0(Folder,'/','Rscripts/extract_temp_CTD.R'))
  source(paste0(Folder,'/','Rscripts/create_inflow_outflow_file.R'))
  
  ###SET FILE NAMES
  catwalk_fname <-  paste0(evaluation_folder,'/','Catwalk.csv')
  met_obs_fname <-paste0(evaluation_folder,'/','FCRmet.csv')
  
  ###DOWNLOAD FILES TO WORKING DIRECTORY
  download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv',paste0(evaluation_folder,'/','FCRmet.csv'))
  download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv',paste0(evaluation_folder,'/','Catwalk.csv'))
  
  ###CREATE HISTORICAL MET FILE
  obs_met_outfile <- paste0(evaluation_folder,'/','GLM_met_eval.csv')
  create_obs_met_input(fname = met_obs_fname,outfile=obs_met_outfile,full_time_hour_obs)
  
  ##CREATE INFLOW AND OUTFILE FILES
  #need to fix - this is just a place holder
  file.copy(from = paste0(forecast_folder,'/','FCR_weir_inflow_2013_2017_20180716.csv'),to = paste0(evaluation_folder,'/','FCR_weir_inflow_2013_2017_20180716.csv'))
  file.copy(paste0(forecast_folder,'/','FCR_spillway_outflow_2013_2017_20180716.csv'),paste0(evaluation_folder,'/','FCR_spillway_outflow_2013_2017_20180716.csv'))
  create_inflow_outflow_file(full_time,evaluation_folder)
  
  obs_temp <- extract_temp_chain(fname = catwalk_fname,full_time)
  
  #mg/L (obs) -> mol/m3 * 31.25
  obs_do <- extract_do_chain(fname = catwalk_fname,full_time)
  
  file.copy(from = paste0(forecast_folder,'/','glm3_initial.nml'), to = paste0(evaluation_folder,'/','glm3.nml'),overwrite = TRUE)
  
  TempObservedDepths <- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8,9)
  DoObservedDepths <- c(1,5,9)
  nlayers_init <- length(the_depths_init)
  num_wq <- 1
  include_wq = FALSE
  
  #NUMBER OF STATE SIMULATED = SPECIFIED DEPTHS
  if(include_wq){
    nstates <- nlayers_init*(1+num_wq)
  }else{
    nstates <- nlayers_init 
  }
  
  if(include_wq){
    nobs <- length(TempObservedDepths) + length(DoObservedDepths)
  }else{
    nobs <- length(TempObservedDepths)
  }
  
  #Observations for each observed state at each time step
  #an observation with at least 1 observation but without an observation in a time-step gets assigned an NA
  z <- t(matrix(rep(NA,nobs), nrow = nobs, ncol = length(full_time)))
  if(include_wq){
    z <- cbind(obs_temp$obs,obs_do$obs)
  }else{
    z <- cbind(obs_temp$obs) 
  }
  
  z_obs <- z
  
  #FIGURE OUT WHICH DEPTHS HAVE OBSERVATIONS
  if(include_wq){
    obs_index <- rep(NA,length(TempObservedDepths)+length(DoObservedDepths))
    for(i in 1:length(TempObservedDepths)){
      obs_index[i] <- which.min(abs(the_depths_init - TempObservedDepths[i]))
    }
    for(i in 1:length(DoObservedDepths)){
      obs_index[length(TempObservedDepths)+i] <- length(the_depths_init) + which.min(abs(the_depths_init - DoObservedDepths[i]))
    }
  }else{
    obs_index <- rep(NA,length(TempObservedDepths))
    for(i in 1:length(TempObservedDepths)){
      obs_index[i] <- which.min(abs(the_depths_init - TempObservedDepths[i]))
    } 
  }
  
  #Matrix for knowing which state the observation corresponds to
  z_states <- t(matrix(obs_index, nrow = length(obs_index), ncol =length(full_time)))
  
  ###
  setwd(evaluation_folder)
  fl <- list.files(forecast_folder,'*nml',full.names = TRUE)
  file.copy(from = fl, to = paste0(evaluation_folder),overwrite = TRUE)
  update_time(start_value  = full_time[1], stop_value = full_time[length(full_time)],workingGLM)
  update_var('GLM_met_eval.csv','meteo_fl',workingGLM)
  update_var(paste0('FCR_inflow.csv'),'inflow_fl',workingGLM)
  update_var(paste0('FCR_spillway_outflow.csv'),'outflow_fl',workingGLM)
  update_var(length(full_time),'num_days',workingGLM)
  
  obs_temp <- extract_temp_chain(fname = catwalk_fname,full_time)
  #KLUDGE TO GET WORKING
  TempObservedDepths <- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8,9)
  init_temps1 <- obs_temp$obs[2,]

  temp_inter <- approxfun(TempObservedDepths,init_temps1,rule=2)
  obs_temp <- extract_temp_chain(fname = catwalk_fname,full_time)
  the_temps_init <- temp_inter(the_depths_init)

  

  if(machine == 'mac'){
    fl <- list.files(forecast_folder,'*.dylib',full.names = TRUE)
  }
  if(machine == 'unix'){
    fl <- list.files(forecast_folder,'*.so',full.names = TRUE)
  }
  file.copy(from = fl, to = evaluation_folder,overwrite = TRUE)
  file.copy(from = paste0(forecast_folder,'/','glm'), to = paste0(evaluation_folder,'/','glm'),overwrite = TRUE)
  
  tmp <- update_temps(curr_temps = the_temps_init,the_depths_init,workingGLM)
  system(paste0(evaluation_folder,'/',"glm"))
  
  
  glm_prediction <- get_temp(file = "output.nc", reference = "surface", z_out = the_depths_init)
  

  ###PLOT FORECAST
 pdf(paste0(evaluation_folder,'/',sim_name,'_EnKF_output.pdf'))
  par(mfrow=c(4,3))
  
  z = z_obs
  
  nmembers = dim(x)[2]
  for(i in 1:nlayers_init){
    model = i
    if(length(which(z_states[1,] == i) > 0)){
      obs = which(z_states[1,] == i)
    }else{
      obs = NA
    }
    #if(!is.na(obs)){
    ylim = range(c(x[,,],c(z[,])),na.rm = TRUE)
    plot(as.POSIXct(full_time_day),x[,1,model],type='l',ylab='water temperature (celsius)',xlab='time step (day)',main = paste('depth: ',the_depths_init[i],' m',sep=''),ylim=ylim)
    if(nmembers > 1){
      for(m in 2:nmembers){
        points(as.POSIXct(full_time_day),x[,m,model],type='l')
      }
    }
    if(!is.na(obs)){
      tmp = z[,obs]
      tmp[is.na(tmp)] = -999
      points(as.POSIXct(full_time_day),tmp,col='red',pch=19,cex=1.0)
    }
    points(as.POSIXct(glm_prediction$DateTime),glm_prediction[,1+i],col='lightblue',type='o')
    abline(v = as.POSIXct(full_time_day[1+hist_days]))
    #}
  }
  
  ###PLOT HISTOGRAMS OF FORECAST
  par(mfrow=c(2,3))
  if(forecast_days > 6){
    xlim<- range(c(x[1+hist_days+7,,obs_index[1]],z[1+hist_days+7,1]),na.rm = TRUE)
    hist(x[1+hist_days+7,,obs_index[1]],main='0.1m temp. 7 days forecast',xlab='Temperature',xlim=xlim)
    abline(v= z[1+hist_days+7,1],col='red')
    xlim<- range(c(x[1+hist_days+7,,obs_index[5]],z[1+hist_days+7,5]),na.rm = TRUE)
    hist(x[1+hist_days+7,,obs_index[5]],main='4m temp. 7 days forecast',xlab='Temperature',xlim=xlim)
    abline(v= z[1+hist_days+7,5],col='red')
    xlim<- range(c(x[1+hist_days+7,,obs_index[10]],z[1+hist_days+7,10]),na.rm = TRUE)
    hist(x[1+hist_days+7,,obs_index[10]],main='9m temp. 7 days forecast',xlab='Temperature',xlim=xlim)
    abline(v= z[1+hist_days+7,10],col='red')
  }
  if(forecast_days > 13){
    xlim<- range(c(x[1+hist_days+14,,obs_index[1]],z[1+hist_days+14,1]),na.rm = TRUE)
    hist(x[1+hist_days+14,,obs_index[1]],main='0.1m temp. 14 days forecast',xlab='Temperature',xlim=xlim)
    abline(v= z[1+hist_days+14,1],col='red')
    xlim<- range(c(x[1+hist_days+14,,obs_index[5]],z[1+hist_days+14,5]),na.rm = TRUE)
    hist(x[1+hist_days+14,,obs_index[5]],main='4m temp. 14 days forecast',xlab='Temperature',xlim=xlim)
    abline(v= z[1+hist_days+14,5],col='red')
    xlim<- range(c(x[1+hist_days+14,,obs_index[10]],z[1+hist_days+14,10]),na.rm = TRUE)
    hist(x[1+hist_days+14,,obs_index[10]],main='9m temp. 14 days forecast',xlab='Temperature',xlim=xlim)
    abline(v= z[1+hist_days+14,10],col='red')
  }
  
  nMETmembers = 21
  ###PLOT NOAA MET TO VIEWING 
  d = read.csv(paste0(forecast_folder,'/',met_file_names[1]))
  air_temp = array(NA,dim=c(nMETmembers,length(d$AirTemp)))
  ShortWave = array(NA,dim=c(nMETmembers,length(d$AirTemp)))
  LongWave = array(NA,dim=c(nMETmembers,length(d$AirTemp)))
  RelHum = array(NA,dim=c(nMETmembers,length(d$AirTemp)))
  WindSpeed = array(NA,dim=c(nMETmembers,length(d$AirTemp)))
  Rain = array(NA,dim=c(nMETmembers,length(d$AirTemp)))
  
  o = read.csv(paste0(evaluation_folder,'/','GLM_met_eval.csv'))
  
  y <- as.POSIXct(d$time)
  include_times <- which(as.POSIXct(d$time) %in% full_time_hour_obs)
  
  for(ens in 1:nMETmembers){
    d = read.csv(paste0(forecast_folder,'/',met_file_names[ens]))
    air_temp[ens,] = d$AirTemp
    ShortWave[ens,] = d$ShortWave
    LongWave[ens,] = d$LongWave
    RelHum[ens,] = d$RelHum
    WindSpeed[ens,] = d$WindSpeed
    Rain[ens,] = d$Rain
  }
  par(mfrow=c(2,3))
  
  #
  ylim = range(c(air_temp,o$AirTemp))
  plot(y[include_times],air_temp[1,include_times],type='l',ylab='Air Temp',xlab = 'days in future',ylim=ylim)
  if(nMETmembers > 1){
    for(m in 2:nMETmembers){
      points(y[include_times],air_temp[m,include_times],type='l')
    }
  }
  points(as.POSIXct(o$time),o$AirTemp,col='red',type='l')
  #
  
  ylim = range(c(ShortWave,o$ShortWave),na.rm = TRUE)
  plot(y[include_times],ShortWave[1,include_times],type='l',ylab='Shortwave',xlab = 'days in future',ylim=ylim)
  if(nMETmembers > 1){
    for(m in 2:nMETmembers){
      points(y[include_times],ShortWave[m,include_times],type='l')
    }
  }
  points(as.POSIXct(o$time),o$ShortWave,col='red',type='l')
  
  ylim = range(c(LongWave,o$LongWave),na.rm = TRUE)
  plot(y[include_times],LongWave[1,include_times],type='l',ylab='Longwave',xlab = 'days in future',ylim=ylim)
  if(nMETmembers > 1){
    for(m in 2:nMETmembers){
      points(y[include_times],LongWave[m,include_times],type='l')
    }
  }
  points(as.POSIXct(o$time),o$LongWave,col='red',type='l')
  
  ylim = range(c(RelHum,o$RelHum),na.rm = TRUE)
  plot(y[include_times],RelHum[1,include_times],type='l',ylab='Rel Hum',xlab = 'days in future',ylim=ylim)
  if(nMETmembers > 1){
    for(m in 2:nMETmembers){
      points(y[include_times],RelHum[m,include_times],type='l')
    }
  }
  points(as.POSIXct(o$time),o$RelHum,col='red',type='l')
  
  
  ylim = range(c(WindSpeed,o$WindSpeedHum),na.rm = TRUE)
  plot(y[include_times],WindSpeed[1,include_times],type='l',ylab='Wind Speed',xlab = 'days in future',ylim=ylim)
  if(nMETmembers > 1){
    for(m in 2:nMETmembers){
      points(y[include_times],WindSpeed[m,include_times],type='l')
    }
  }
  points(as.POSIXct(o$time),o$WindSpeed,col='red',type='l')
  
  ylim = range(c(Rain,o$Rain),na.rm = TRUE)
  plot(y[include_times],Rain[1,include_times],type='l',ylab='Rain',xlab = 'days in future',ylim=ylim)
  if(nMETmembers > 1){
    for(m in 2:nMETmembers){
      points(y[include_times],Rain[m,include_times],type='l')
    }
  }
  points(as.POSIXct(o$time),o$Rain,col='red',type='l')
  
  dev.off()
  
  save(glm_prediction,z,full_time_day,x,file=paste0(evaluation_folder,'/',sim_name,'model_compare.Rdata'))

}