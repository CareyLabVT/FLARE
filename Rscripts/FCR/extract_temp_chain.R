extract_temp_chain <- function(fname,
                               full_time_local, 
                               modeled_depths = modeled_depths,
                               observed_depths_temp = observed_depths_temp,
                               input_file_tz, 
                               local_tzone){
  
  if(length(fname) > 1){
    #Different lakes are going to have to modify this for their temperature data format
    d1 <- read.csv(fname[1], skip = 4, na.strings = 'NAN', stringsAsFactors = FALSE)
    d_names <- read.csv(fname[1], skip =1, stringsAsFactors = FALSE)
    names(d1) <- names(d_names)
    
    d2 <- read.csv(fname[2], na.strings = 'NA', stringsAsFactors = FALSE)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_temp
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }
    
    TIMESTAMP_in <- as_datetime(d1$TIMESTAMP,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    TIMESTAMP_in <- as_datetime(d2$DateTime,tz = input_file_tz)
    d2$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d1 <- d1[which(d1$TIMESTAMP > d2$TIMESTAMP[nrow(d2)] | d1$TIMESTAMP < d2$TIMESTAMP[1]), ]
    
    d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, wtr_surface = d1$wtr_surface, wtr_1 = d1$wtr_1, wtr_2 = d1$wtr_2, wtr_3 = d1$wtr_3, wtr_4 = d1$wtr_4,
                     wtr_5 = d1$wtr_5, wtr_6 = d1$wtr_6, wtr_7 = d1$wtr_7, wtr_8 = d1$wtr_8, wtr_9 = d1$wtr_9, wtr_1_exo = d1$EXO_wtr_1, wtr_5_do = d1$dotemp_5, wtr_9_do = d1$dotemp_9)
    
    d4 <- data.frame(TIMESTAMP = d2$TIMESTAMP, wtr_surface = d2$ThermistorTemp_C_surface, wtr_1 = d2$ThermistorTemp_C_1, wtr_2 = d2$ThermistorTemp_C_2, wtr_3 = d2$ThermistorTemp_C_3, wtr_4 = d2$ThermistorTemp_C_4,
                     wtr_5 = d2$ThermistorTemp_C_5, wtr_6 = d2$ThermistorTemp_C_6, wtr_7 = d2$ThermistorTemp_C_7, wtr_8 = d2$ThermistorTemp_C_8, wtr_9 = d2$ThermistorTemp_C_9, wtr_1_exo = d2$EXOTemp_C_1, wtr_5_do = d2$RDOTemp_C_5, wtr_9_do = d2$RDOTemp_C_9)
    
    d <- rbind(d3,d4)
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index = which(d$TIMESTAMP==full_time_local[i])
      if(length(index)>0){
        obs[i,obs_index] <- unlist(d[index,2:11])
        if(is.na(obs[i,obs_index[2]]) & !is.na(d[index,"wtr_1_exo"])){
          obs[i,obs_index[2]] <- d[index,"wtr_1_exo"]
        }
        if(is.na(obs[i,obs_index[6]]) & !is.na(d[index,"wtr_5_do"])){
          obs[i,obs_index[6]] <- d[index,"wtr_5_do"]
        }
        if(is.na(obs[i,obs_index[10]]) & !is.na(d[index,"wtr_9_do"])){ 
          obs[i,obs_index[10]] <- d[index,"wtr_9_do"]
        }
      }
    }
    
  }else{
    #Different lakes are going to have to modify this for their temperature data format
    d <- read.csv(fname[1], skip = 4, na.strings = 'NAN', stringsAsFactors = FALSE)
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
  }
  
  for(i in 1:length(obs[, 1])){
    for(j in 1:length(obs[1, ])){
      if(obs[i, j] == 0 | 
         is.na(obs[i, j]) | 
         is.nan(obs[i, j])){
        obs[i, j] = NA
      } 
    }
  }
  
  return(list(obs = obs, depths = modeled_depths))
}