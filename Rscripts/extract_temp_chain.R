
extract_temp_chain <- function(fname,
                               full_time_local, 
                               modeled_depths = modeled_depths,
                               observed_depths_temp = observed_depths_temp,
                               input_file_tz, 
                               local_tzone){

  #Different lakes are going to have to modify this for their temperature data format
  d <- read.csv(fname, skip = 4, na.strings = 'NAN', stringsAsFactors = FALSE)
  d_names <- read.csv(fname, skip =1, stringsAsFactors = FALSE)
  names(d) <- names(d_names)
  

  obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
  depths_w_obs <- observed_depths_temp
  obs_index <-   rep(NA,length(depths_w_obs))
  for(i in 1:length(depths_w_obs)){
    obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
  }

  TIMESTAMP_in <- as_datetime(d$TIMESTAMP,tz = input_file_tz)
  d$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
  
  full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
  for(i in 1:length(full_time_local)){
    index = which(d$TIMESTAMP==full_time_local[i])
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
  
  return(list(obs = obs, depths = modeled_depths))
}


extract_do_chain <- function(fname = catwalk_fname, 
                             full_time_local,
                             modeled_depths = modeled_depths, 
                             observed_depths_do= observed_depths_do,
                             input_file_tz = 'EST5EDT', 
                             local_tzone){
  
  d <- read.csv(fname, skip =3, na.strings = 'NAN', stringsAsFactors = FALSE)
  d_names <- read.csv(fname, skip =1)
  names(d) <- names(d_names)
  
  obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
  depths_w_obs <- observed_depths_do
  obs_index <-   rep(NA,length(depths_w_obs))
  for(i in 1:length(depths_w_obs)){
    obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
  }
  
  TIMESTAMP_in <- as_datetime(d$TIMESTAMP,tz = input_file_tz)
  d$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
  full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
  for(i in 1:length(full_time_local)){
    index = which(abs(as.POSIXct(d$TIMESTAMP)-as.POSIXct(full_time_local[i])) == min(abs(as.POSIXct(d$TIMESTAMP) - as.POSIXct(full_time_local[i]))))
    index = which(as.POSIXct(d$TIMESTAMP)==as.POSIXct(full_time_local[i]))
    if(length(index)>0){
      obs[i,obs_index[1]] <- max(c(d$doobs_1[index],0.0))
      obs[i,obs_index[2]] <- max(c(d$doobs_5[index],0.0))
      obs[i,obs_index[3]] <- max(c(d$doobs_9[index],0.0))
    }
  }
  return(list(obs = obs, depths = modeled_depths))
}

extract_chla_chain <- function(fname = catwalk_fname,
                               full_time_local,
                               modeled_depths = modeled_depths,
                               observed_depths_chla_fdom= observed_depths_chla_fdom,
                               input_file_tz = 'EST5EDT',
                               local_tzone){
  d <- read.csv(fname, skip =3, na.strings = 'NAN', stringsAsFactors = FALSE)
  d_names <- read.csv(fname, skip =1)
  names(d) <- names(d_names)
  
  Chla_obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
  BGAPC_obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
  fDOM_obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
  
  obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
  depths_w_obs <- observed_depths_chla_fdom
  obs_index <-   rep(NA,length(depths_w_obs))
  for(i in 1:length(observed_depths_chla_fdom)){
    obs_index[i] <- which.min(abs(modeled_depths - observed_depths_chla_fdom[i]))
  }
  
  TIMESTAMP_in <- as_datetime(d$TIMESTAMP,tz = input_file_tz)
  d$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
  full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
  
  for(i in 1:length(full_time_local)){
    index = which(abs(as.POSIXct(d$TIMESTAMP)-as.POSIXct(full_time_local[i])) == min(abs(as.POSIXct(d$TIMESTAMP) - as.POSIXct(full_time_local[i]))))
    index = which(as.POSIXct(d$TIMESTAMP)==as.POSIXct(full_time_local[i]))
    if(length(index)>0){
      Chla_obs[i,obs_index] <- d$Chla_1[index]
      BGAPC_obs[i,obs_index] <- d$BGAPC_1[index]
      fDOM_obs[i,obs_index] <- d$fDOM_QSU_1[index]
    }
  }
  return(list(Chla_obs = Chla_obs, BGAPC_obs = BGAPC_obs, fDOM_obs = fDOM_obs, depths = modeled_depths))
}