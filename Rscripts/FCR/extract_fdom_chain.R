extract_fdom_chain <- function(fname = catwalk_fname,
                               full_time_local,
                               modeled_depths = modeled_depths,
                               observed_depths_chla_fdom = observed_depths_chla_fdom,
                               input_file_tz = 'EST5EDT',
                               local_tzone){
  
  
  if(length(fname) > 1){
    #Different lakes are going to have to modify this for their temperature data format
    d1 <- read.csv(fname[1], skip = 4, na.strings = 'NAN', stringsAsFactors = FALSE)
    d_names <- read.csv(fname[1], skip =1, stringsAsFactors = FALSE)
    names(d1) <- names(d_names)
    
    d2 <- read.csv(fname[2], na.strings = 'NA', stringsAsFactors = FALSE)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_chla_fdom
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }
    
    TIMESTAMP_in <- as_datetime(d1$TIMESTAMP,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    TIMESTAMP_in <- as_datetime(d2$DateTime,tz = input_file_tz)
    d2$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d1 <- d1[which(d1$TIMESTAMP > d2$TIMESTAMP[nrow(d2)] | d1$TIMESTAMP < d2$TIMESTAMP[1]), ]
    
    d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, fDOM_1 = d1$fDOM_QSU_1)
    
    d4 <- data.frame(TIMESTAMP = d2$TIMESTAMP, fDOM_1 = d2$EXOfDOM_QSU_1)
    
    d <- rbind(d3,d4)
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index = which(d$TIMESTAMP==full_time_local[i])
      if(length(index)>0){
        obs[i,obs_index] <- d$fDOM_1[index]
      }
    }
    
  }else{
    #Different lakes are going to have to modify this for their temperature data format
    d1 <- read.csv(fname, skip = 4, na.strings = 'NAN', stringsAsFactors = FALSE)
    d_names <- read.csv(fname, skip =1, stringsAsFactors = FALSE)
    names(d1) <- names(d_names)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_chla_fdom
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }
    
    TIMESTAMP_in <- as_datetime(d1$TIMESTAMP,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, fDOM_1 = d1$fDOM_QSU_1)
    
    d <- d3    
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index = which(d$TIMESTAMP==full_time_local[i])
      if(length(index)>0){
        obs[i,obs_index] <- d$fDOM_1[index]
      }
    }
    
  }
  
  #DIRRRRTY qsu -> mg/L ->  mmol/m3
  #From Mary Lofton Sept 8, Model_w/o_Oct_2018 (R2 = 0.593)
  obs<- -98.147 + 26.101* obs
  
  
  return(list(obs = obs, depths = modeled_depths))
}