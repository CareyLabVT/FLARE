extract_temp_chain <- function(fname,
                               full_time_local, 
                               modeled_depths = modeled_depths,
                               observed_depths_temp = observed_depths_temp,
                               local_tzone){
  
    d <- read_csv(fname, quoted_na = FALSE, guess_max = 10000)
    d$TIMESTAMP <- force_tz(d$TIMESTAMP, tzone = local_tzone)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_temp
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }

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