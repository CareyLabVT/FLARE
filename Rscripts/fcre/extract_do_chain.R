extract_do_chain <- function(fname = catwalk_fname, 
                             full_time_local,
                             modeled_depths = modeled_depths, 
                             observed_depths_do= observed_depths_do,
                             input_file_tz = 'EST5EDT', 
                             local_tzone){
  
  if(length(fname) > 1){
    #Different lakes are going to have to modify this for their temperature data format
    
    d1 <- read.csv(fname[1], na.strings = 'NA', stringsAsFactors = FALSE)
    #d1 <- read.csv(fname[1], skip = 4, na.strings = 'NAN', stringsAsFactors = FALSE)
    #d_names <- read.csv(fname[1], skip =1, stringsAsFactors = FALSE)
    #names(d1) <- names(d_names)
    
    d2 <- read.csv(fname[2], na.strings = 'NA', stringsAsFactors = FALSE)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_do
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }
    
    TIMESTAMP_in <- as_datetime(d1$DateTime,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    TIMESTAMP_in <- as_datetime(d2$DateTime,tz = input_file_tz)
    d2$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d1 <- d1[which(d1$TIMESTAMP > d2$TIMESTAMP[nrow(d2)] | d1$TIMESTAMP < d2$TIMESTAMP[1]), ]
    
    #d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, doobs_1 = d1$doobs_1, doobs_5 = d1$doobs_5, doobs_9 = d1$doobs_9)
    
    d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, doobs_1 = d1$EXODO_mgL_1, doobs_5 = d1$RDO_mgL_5, doobs_9 = d1$RDO_mgL_9)
    
    d4 <- data.frame(TIMESTAMP = d2$TIMESTAMP, doobs_1 = d2$EXODO_mgL_1, doobs_5 = d2$RDO_mgL_5, doobs_9 = d2$RDO_mgL_9)
    
    d <- rbind(d3,d4)
    
    d <- d %>% 
      arrange(TIMESTAMP)
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index <- which(d$TIMESTAMP <= full_time_local[i] & d$TIMESTAMP > full_time_local[i] - hours(1))
      if(length(index)>0){
        obs[i,obs_index[1]] <- max(c(mean(d$doobs_1[index], na.rm = TRUE),0.0))
        obs[i,obs_index[2]] <- max(c(mean(d$doobs_5[index], na.rm = TRUE),0.0))
        obs[i,obs_index[3]] <- max(c(mean(d$doobs_9[index], na.rm = TRUE),0.0))
      }
    }
    
  }else{
    #Different lakes are going to have to modify this for their temperature data format
    d1 <- read.csv(fname, na.strings = 'NA', stringsAsFactors = FALSE)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_do
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }
    
    TIMESTAMP_in <- as_datetime(d1$DateTime,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d <- data.frame(TIMESTAMP = d1$TIMESTAMP, doobs_1 = d1$EXODO_mgL_1, doobs_5 = d1$RDO_mgL_5, doobs_9 = d1$RDO_mgL_9)
 
    d <- d %>% 
      arrange(TIMESTAMP)
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index <- which(d$TIMESTAMP <= full_time_local[i] & d$TIMESTAMP > full_time_local[i] - hours(1))
      if(length(index)>0){
        obs[i,obs_index[1]] <- max(c(mean(d$doobs_1[index], na.rm = TRUE),0.0))
        obs[i,obs_index[2]] <- max(c(mean(d$doobs_5[index], na.rm = TRUE),0.0))
        obs[i,obs_index[3]] <- max(c(mean(d$doobs_9[index], na.rm = TRUE),0.0))
      }
    }
  }
  
  obs <- obs*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  
  return(list(obs = obs, depths = modeled_depths))
}
