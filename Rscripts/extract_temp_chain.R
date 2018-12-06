
extract_temp_chain <- function(fname,full_time,depths = modeled_depths,observed_depths_temp = observed_depths_temp,input_tz, output_tz){
  d <- read.csv(fname, skip =4, na.strings = 'NAN')
  d_names <- read.csv(fname, skip =1)
  names(d) <- names(d_names)
  
  obs <- array(NA,dim=c(length(full_time),length(depths)))
  depths_w_obs <- observed_depths_temp
  obs_index <-   rep(NA,length(depths_w_obs))
  for(i in 1:length(depths_w_obs)){
    obs_index[i] <- which.min(abs(depths - depths_w_obs[i]))
  }
  
  TIMESTAMP_in <- as.POSIXct(d$TIMESTAMP,origin = '1970-01-01 00:00.00 UTC',tz = input_tz)
  d$TIMESTAMP <- as.POSIXct(TIMESTAMP_in,tz = output_tz)
  full_time <- as.POSIXct(full_time,tz = output_tz)
  for(i in 1:length(full_time)){
    index = which(d$TIMESTAMP==full_time[i])
    if(length(index)>0){
      obs[i,obs_index] <- unlist(d[index,5:14])
      if(is.na(obs[i,obs_index[2]]) & !is.na(d[index,23])){
        obs[i,obs_index[2]] <- d[index,23]
      }
      if(is.na(obs[i,obs_index[6]]) & !is.na(d[index,17])){
        obs[i,obs_index[6]] <- d[index,17]
      }
      if(is.na(obs[i,obs_index[10]]) & !is.na(d[index,20])){ 
        obs[i,obs_index[10]] <- d[index,20]
      }
    }
  }
  
  return(list(obs = obs, depths = depths))
}


extract_do_chain <- function(fname = catwalk_fname,full_time,depths = modeled_depths,observed_depths_do= observed_depths_do,input_tz = 'EST5EDT', output_tz = 'GMT'){
  d <- read.csv(fname, skip =3, na.strings = 'NAN')
  d_names <- read.csv(fname, skip =1)
  names(d) <- names(d_names)
  
  obs <- array(NA,dim=c(length(full_time),length(depths)))
  depths_w_obs <- observed_depths_do
  obs_index <-   rep(NA,length(depths_w_obs))
  for(i in 1:length(depths_w_obs)){
    obs_index[i] <- which.min(abs(depths - depths_w_obs[i]))
  }
  
  TIMESTAMP_in <- as.POSIXct(d$TIMESTAMP,origin = '1970-01-01 00:00.00 UTC',tz = input_tz)
  d$TIMESTAMP <- as.POSIXct(TIMESTAMP_in,tz = output_tz)
  full_time <- as.POSIXct(full_time,tz = output_tz)
  for(i in 1:length(full_time)){
    index = which(abs(as.POSIXct(d$TIMESTAMP)-as.POSIXct(full_time[i])) == min(abs(as.POSIXct(d$TIMESTAMP) - as.POSIXct(full_time[i]))))
    index = which(as.POSIXct(d$TIMESTAMP)==as.POSIXct(full_time[i]))
    if(length(index)>0){
      obs[i,obs_index[1]] <- max(c(d$doobs_1[index],0.0))
      obs[i,obs_index[2]] <- max(c(d$doobs_5[index],0.0))
      obs[i,obs_index[3]] <- max(c(d$doobs_9[index],0.0))
    }
  }
  return(list(obs = obs, depths = depths))
}

extract_chla_chain <- function(fname = catwalk_fname,full_time,depths = modeled_depths,observed_depths_chla_fdom= observed_depths_chla_fdom,input_tz = 'EST5EDT', output_tz = 'GMT'){
  d <- read.csv(fname, skip =3, na.strings = 'NAN')
  d_names <- read.csv(fname, skip =1)
  names(d) <- names(d_names)
  
  Chla_obs <- array(NA,dim=c(length(full_time),length(depths)))
  BGAPC_obs <- array(NA,dim=c(length(full_time),length(depths)))
  fDOM_obs <- array(NA,dim=c(length(full_time),length(depths)))
  
  obs <- array(NA,dim=c(length(full_time),length(depths)))
  depths_w_obs <- observed_depths_chla_fdom
  obs_index <-   rep(NA,length(depths_w_obs))
  for(i in 1:length(observed_depths_chla_fdom)){
    obs_index[i] <- which.min(abs(depths - observed_depths_chla_fdom[i]))
  }
  
  TIMESTAMP_in <- as.POSIXct(d$TIMESTAMP,origin = '1970-01-01 00:00.00 UTC',tz = input_tz)
  d$TIMESTAMP <- as.POSIXct(TIMESTAMP_in,tz = output_tz)
  full_time <- as.POSIXct(full_time,tz = output_tz)
  for(i in 1:length(full_time)){
    index = which(abs(as.POSIXct(d$TIMESTAMP)-as.POSIXct(full_time[i])) == min(abs(as.POSIXct(d$TIMESTAMP) - as.POSIXct(full_time[i]))))
    index = which(as.POSIXct(d$TIMESTAMP)==as.POSIXct(full_time[i]))
    if(length(index)>0){
      Chla_obs[obs_index] <- d$Chla_1[index]
      BGAPC_obs[obs_index] <- d$BGAPC_1[index]
      fDOM_obs[obs_index] <- d$fDOM_QSU_1[index]
    }
  }
  return(list(Chla_obs = Chla_obs, BGAPC_obs = BGAPC_obs, fDOM_obs = fDOM_obs, depths = depths))
}