extract_do_chain <- function(fname = catwalk_fname, 
                             full_time_local,
                             modeled_depths = modeled_depths, 
                             observed_depths_do= observed_depths_do,
                             local_tzone){
  
  d <- read_csv(fname, quoted_na = FALSE, guess_max = 10000)
  d$TIMESTAMP <- force_tz(d$TIMESTAMP, tzone = local_tzone)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_do
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index <- which(d$TIMESTAMP <= full_time_local[i] & d$TIMESTAMP > full_time_local[i] - hours(1))
      if(length(index)>0){
        obs[i,obs_index[1]] <- max(c(mean(d$doobs_1[index], na.rm = TRUE),0.0))
        obs[i,obs_index[2]] <- max(c(mean(d$doobs_5[index], na.rm = TRUE),0.0))
        obs[i,obs_index[3]] <- max(c(mean(d$doobs_9[index], na.rm = TRUE),0.0))
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
