#FCR specific functions for reading in observations.  
#Need to keep the same names and output format but need to modify for your lake data


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


extract_do_chain <- function(fname = catwalk_fname, 
                             full_time_local,
                             modeled_depths = modeled_depths, 
                             observed_depths_do= observed_depths_do,
                             input_file_tz = 'EST5EDT', 
                             local_tzone){
  
  if(length(fname) > 1){
    #Different lakes are going to have to modify this for their temperature data format
    d1 <- read.csv(fname[1], skip = 4, na.strings = 'NAN', stringsAsFactors = FALSE)
    d_names <- read.csv(fname[1], skip =1, stringsAsFactors = FALSE)
    names(d1) <- names(d_names)
    
    d2 <- read.csv(fname[2], na.strings = 'NA', stringsAsFactors = FALSE)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_do
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }
    
    TIMESTAMP_in <- as_datetime(d1$TIMESTAMP,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    TIMESTAMP_in <- as_datetime(d2$DateTime,tz = input_file_tz)
    d2$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d1 <- d1[which(d1$TIMESTAMP > d2$TIMESTAMP[nrow(d2)] | d1$TIMESTAMP < d2$TIMESTAMP[1]), ]
    
    d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, doobs_1 = d1$doobs_1, doobs_5 = d1$doobs_5, doobs_9 = d1$doobs_9)
    
    d4 <- data.frame(TIMESTAMP = d2$TIMESTAMP, doobs_1 = d2$EXODO_mgL_1, doobs_5 = d2$RDO_mgL_5, doobs_9 = d2$RDO_mgL_9)
    
    d <- rbind(d3,d4)
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index = which(d$TIMESTAMP==full_time_local[i])
      if(length(index)>0){
        obs[i,obs_index[1]] <- max(c(d$doobs_1[index],0.0))
        obs[i,obs_index[2]] <- max(c(d$doobs_5[index],0.0))
        obs[i,obs_index[3]] <- max(c(d$doobs_9[index],0.0))
      }
    }
    
  }else{
    #Different lakes are going to have to modify this for their temperature data format
    d1 <- read.csv(fname[1], skip = 4, na.strings = 'NAN', stringsAsFactors = FALSE)
    d_names <- read.csv(fname[1], skip =1, stringsAsFactors = FALSE)
    names(d1) <- names(d_names)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_do
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }
    
    TIMESTAMP_in <- as_datetime(d1$TIMESTAMP,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)

    d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, doobs_1 = d1$doobs_1, doobs_5 = d1$doobs_5, doobs_9 = d1$doobs_9)
    
    
    d <- d3
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index = which(d$TIMESTAMP==full_time_local[i])
      if(length(index)>0){
        obs[i,obs_index[1]] <- max(c(d$doobs_1[index],0.0))
        obs[i,obs_index[2]] <- max(c(d$doobs_5[index],0.0))
        obs[i,obs_index[3]] <- max(c(d$doobs_9[index],0.0))
      }
    }
  }
  
  obs <- obs*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  
  return(list(obs = obs, depths = modeled_depths))
  }

extract_chla_chain <- function(fname = catwalk_fname,
                               full_time_local,
                               modeled_depths = modeled_depths,
                               observed_depths_chla_fdom= observed_depths_chla_fdom,
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
    
    d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, Chla_1 = d1$Chla_1)
    
    d4 <- data.frame(TIMESTAMP = d2$TIMESTAMP, Chla_1 = d2$EXOChla_ugL_1)
    
    d <- rbind(d3,d4)
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index = which(d$TIMESTAMP==full_time_local[i])
      if(length(index)>0){
        obs[i,obs_index] <- d$Chla_1[index]
      }
    }
    
  }else{
    #Different lakes are going to have to modify this for their temperature data format
    d1 <- read.csv(fname[1], skip = 4, na.strings = 'NAN', stringsAsFactors = FALSE)
    d_names <- read.csv(fname[1], skip =1, stringsAsFactors = FALSE)
    names(d1) <- names(d_names)
    
    obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
    depths_w_obs <- observed_depths_chla_fdom
    obs_index <-   rep(NA,length(depths_w_obs))
    for(i in 1:length(depths_w_obs)){
      obs_index[i] <- which.min(abs(modeled_depths - depths_w_obs[i]))
    }
    
    TIMESTAMP_in <- as_datetime(d1$TIMESTAMP,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, Chla_1 = d1$Chla_1)
  
    d <- d3
    
    full_time_local <- as.POSIXct(full_time_local,tz = local_tzone)
    for(i in 1:length(full_time_local)){
      index = which(d$TIMESTAMP==full_time_local[i])
      if(length(index)>0){
        obs[i,obs_index] <- d$Chla_1[index]
      }
    }
  }
  
  obs <- exo_2_ctd_chla[1] + obs*exo_2_ctd_chla[2]
  
  return(list(obs = obs, depths = modeled_depths))
}

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
  obs<- obs*1000/(12*6)   

  
  return(list(obs = obs, depths = modeled_depths))
}

extract_CTD <- function(fname,
                             full_time_day_local,
                             modeled_depths,
                             input_file_tz,
                             local_tzone){
  
  d <- read.csv(fname)
  if(length(d[which(d$Reservoir == 'FCR' & d$Site == '50'),1])>0){
    d_fcr <- d[which(d$Reservoir == 'FCR' & d$Site == '50'),]
  }else{
    d_fcr <- d
  }
  d_fcr_day <- as_date(d_fcr$Date)
  obs_temp <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_chla <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_do <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_pH <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_sal <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  
  TIMESTAMP_in <- as_datetime(paste0(d_fcr_day, '12:00:00'),tz = input_file_tz)
  TIMESTAMP_out <- with_tz(TIMESTAMP_in,tz = local_tzone)
  d_fcr_day <- as_date(TIMESTAMP_out)
  
  for(i in 1:length(full_time_day_local)){
    index1 = which(d_fcr_day==full_time_day_local[i])
    if(length(index1)>0){
      curr_day <- d_fcr[index1,]
      for(j in 1:length(modeled_depths)){
        index2 <- which.min(abs(curr_day$Depth_m -modeled_depths[j]))
        obs_temp[i,j] <- curr_day$Temp_C[index2]
        obs_chla[i,j] <- curr_day$Chla_ugL[index2]
        obs_do[i,j] <- curr_day$DO_mgL[index2]
        obs_pH[i,j] <- curr_day$pH[index2]
        #obs_sal[i,j] <- curr_day$Salinity[index2]
      }
    }
  }
  
  obs_do <- obs_do*1000/32
  
  return(list(obs_temp = obs_temp,obs_chla = obs_chla, obs_do= obs_do, obs_pH = obs_pH,obs_sal= obs_sal, depths = modeled_depths))
}


create_obs_met_input <- function(fname,
                                 outfile,
                                 full_time_hour_local,
                                 input_file_tz = "EST5EDT",
                                 local_tzone) {
  
  d <- read.csv( fname, skip = 3)
  d_names <- read.csv(fname, skip = 1)
  names(d) <- names(d_names)
  
  d <- d[-85572, ]
  
  ShortWave <- rep(NA, length(full_time_hour_local) - 1)
  LongWave <- rep(NA, length(full_time_hour_local) - 1)
  AirTemp <- rep(NA, length(full_time_hour_local) - 1)
  RelHum <- rep(NA, length(full_time_hour_local) - 1)
  WindSpeed <- rep(NA, length(full_time_hour_local) - 1)
  Rain <- rep(NA, length(full_time_hour_local) - 1)
  Snow <- rep(NA, length(full_time_hour_local) - 1)
  
  TIMESTAMP_in <- as.POSIXct(d$TIMESTAMP, 
                             format= "%Y-%m-%d %H:%M",
                             tz = input_file_tz)
  
  d$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
  
  full_time_hour_local <- as.POSIXct(full_time_hour_local, tz = local_tz)
  
  if(length(which(d$TIMESTAMP == full_time_hour_local[1])) > 0){
    
    for(i in 1:(length(full_time_hour_local) - 1)){
      index <- which(d$TIMESTAMP == full_time_hour_local[i])
      index_2 <- which(d$TIMESTAMP == full_time_hour_local[i + 1])
      if(length(index) > 0 & length(index_2) > 0){
        ShortWave[i] <- max(mean(d$SR01Up_Avg[index:index_2]), 0.0)
        LongWave[i] <- mean(d$IR01UpCo_Avg[index:index_2])
        AirTemp[i] <- d$AirTC_Avg[index]
        RelHum[i] <- d$RH[index]
        if(is.na(RelHum[i])){
          RelHum[i] <- mean(c(d$RH[index-1],d$RH[index+1]))
        }
        WindSpeed[i] <- mean(d$WS_ms_Avg[index:index_2])
        Rain[i] <- (sum(d$Rain_mm_Tot[index:index_2]) * 24) / 1000
        Snow[i] <- 0
      }
    }
    
    remove_hours <- which(!is.na(AirTemp))
    
    ShortWave <- ShortWave[remove_hours]
    LongWave <- LongWave[remove_hours]
    AirTemp <- AirTemp[remove_hours]
    RelHum <- RelHum[remove_hours]
    WindSpeed <- WindSpeed[remove_hours]
    Rain <- Rain[remove_hours]
    Snow <- Snow[remove_hours]
    full_time_hour_local <- full_time_hour_local[remove_hours]
    
    
    historical_met <- data.frame(full_time_hour_local,
                                 ShortWave,
                                 LongWave,
                                 AirTemp,
                                 RelHum,
                                 WindSpeed,
                                 Rain,
                                 Snow)
    n <- noquote(c("time",
                   "ShortWave",
                   "LongWave",
                   "AirTemp",
                   "RelHum",
                   "WindSpeed",
                   "Rain",
                   "Snow"))
    colnames(historical_met) <- noquote(c("time",
                                          "ShortWave",
                                          "LongWave",
                                          "AirTemp",
                                          "RelHum",
                                          "WindSpeed",
                                          "Rain",
                                          "Snow"))
    write.csv(historical_met, file = outfile, row.names = FALSE, quote = FALSE)
  }
  
}


create_inflow_outflow_file <- function(full_time_day_local,
                                       working_directory,
                                       input_file_tz = 'EST5EDT', 
                                       start_forecast_step,
                                       hold_inflow_outflow_constant,
                                       inflow_file1,
                                       inflow_file2,
                                       outflow_file1,
                                       local_tzone){
  
  full_time_day_2017 <- as.POSIXct(full_time_day_local,
                                   tz = local_tzone) - 365*24*60*60
  full_time_day_2016 <- as.POSIXct(full_time_day_local,
                                   tz = local_tzone) - 2*365*24*60*60
  full_time_day_2015 <- as.POSIXct(full_time_day_local, 
                                   tz = local_tzone) - 3*365*24*60*60
  full_time_day_2014 <- as.POSIXct(full_time_day_local, 
                                   tz = local_tzone) - 4*365*24*60*60
  full_time_day_2015 <- as.POSIXct(full_time_day_local, 
                                   tz = local_tzone) - 5*365*24*60*60
  
  inflow = read.csv(paste0(working_directory,'/', inflow_file1))
  spillway = read.csv(paste0(working_directory,'/', outflow_file1))
  wetland = read.csv(paste0(working_directory,'/', inflow_file2))
  
  inflow_time_local <- as.POSIXct(inflow$time, tz = input_file_tz)
  inflow_time_tmp <- with_tz(inflow_time_local,local_tzone)
  
  spillway_time_local <- as.POSIXct(spillway$time, tz = input_file_tz)
  spillway_time_tmp <- with_tz(spillway_time_local,local_tzone)
  
  wetland_time_local <- as.POSIXct(wetland$time, tz = input_file_tz)
  wetland_time_tmp <- with_tz(wetland_time_local,local_tzone)
  
  inflow_2017 =  inflow[which(inflow_time_tmp %in% full_time_day_2017),]
  inflow_2016 =  inflow[which(inflow_time_tmp %in% full_time_day_2016),]
  inflow_2015 =  inflow[which(inflow_time_tmp %in% full_time_day_2015),]
  inflow_2014 =  inflow[which(inflow_time_tmp %in% full_time_day_2014),]
  inflow_2013 =  inflow[which(inflow_time_tmp %in% full_time_day_2015),]
  inflow_new = inflow_2014
  
  spillway_2017 =  spillway[which(spillway_time_tmp %in% full_time_day_2017),]
  spillway_2016 =  spillway[which(spillway_time_tmp %in% full_time_day_2016),]
  spillway_2015 =  spillway[which(spillway_time_tmp %in% full_time_day_2015),]
  spillway_2014 =  spillway[which(spillway_time_tmp %in% full_time_day_2014),]
  spillway_2013 =  spillway[which(spillway_time_tmp %in% full_time_day_2015),]
  spillway_new = spillway_2017
  
  wetland_2017 =  wetland[which(wetland_time_tmp %in% full_time_day_2017),]
  wetland_2016 =  wetland[which(wetland_time_tmp %in% full_time_day_2016),]
  wetland_2015 =  wetland[which(wetland_time_tmp %in% full_time_day_2015),]
  wetland_2014 =  wetland[which(wetland_time_tmp %in% full_time_day_2014),]
  wetland_2013 =  wetland[which(wetland_time_tmp %in% full_time_day_2015),]
  wetland_new = wetland_2017
  
  inflow_file_names <- rep(NA, n_inflow_outflow_members)
  spillway_file_names <- rep(NA, n_inflow_outflow_members)
  wetland_file_names <- rep(NA, n_inflow_outflow_members)
  
  for(m in 1:(n_inflow_outflow_members)){
    for(i in 1:length(full_time_day_local)){
      curr_day <- day(full_time_day_local[i])
      curr_month <- month(full_time_day_local[i])
      index1 <- which(day(inflow_time_tmp) == curr_day & month(inflow_time_tmp) == curr_month)
      index2 <- which(day(spillway_time_tmp) == curr_day & month(spillway_time_tmp) == curr_month)
      index3 <- which(day(wetland_time_tmp) == curr_day & month(wetland_time_tmp) == curr_month)
      if(i < (start_forecast_step+1)){
        hist_index1 <- index1
        hist_index2 <- index2
        hist_index3 <- index3
      }
      if(i < (start_forecast_step+1)){
        for(j in 2:ncol(inflow)){
          if(n_inflow_outflow_members == 1){
            inflow_new[i,j] <- mean(inflow[index1,j], na.rm = TRUE)
            wetland_new[i,j] <- mean(wetland[index3,j], na.rm = TRUE)
          }else{
            inflow_new[i,j] <- rnorm(1, mean(inflow[index1,j], na.rm = TRUE), sd(inflow[index1,j], na.rm = TRUE))
            wetland_new[i,j] <- rnorm(1, mean(wetland[index3,j], na.rm = TRUE), sd(wetland[index3,j], na.rm = TRUE))
          }
        }
        
        spillway_new[i,2] <- inflow_new[i,2] +  wetland_new[i,2] 
        
      }else{
        for(j in 2:ncol(inflow)){
          if(n_inflow_outflow_members == 1){
            if(hold_inflow_outflow_constant){
              inflow_new[i,j] <- mean(inflow[index1,j], na.rm = TRUE)
              wetland_new[i,j] <- mean(wetland[index3,j], na.rm = TRUE)
            }else{
              inflow_new[i,j] <- mean(inflow[hist_index1,j], na.rm = TRUE)
              wetland_new[i,j] <- mean(wetland[hist_index3,j], na.rm = TRUE)    
            }
          }else{
            if(hold_inflow_outflow_constant){
              inflow_new[i,j] <- rnorm(1, mean(inflow[index1,j], na.rm = TRUE), sd(inflow[index1,j], na.rm = TRUE))
              wetland_new[i,j] <- rnorm(1, mean(wetland[index3,j], na.rm = TRUE), sd(wetland[index3,j], na.rm = TRUE))
            }else{
              inflow_new[i,j] <- rnorm(1, mean(inflow[hist_index1,j], na.rm = TRUE), sd(inflow[index1,j], na.rm = TRUE))
              wetland_new[i,j] <- rnorm(1, mean(wetland[hist_index3,j], na.rm = TRUE), sd(wetland[index3,j], na.rm = TRUE))  
            }
          }
        }
        spillway_new[i,2] <- inflow_new[i,2] + wetland_new[i,2] 
      }
    }
    
    inflow_new$time =  full_time_day_local
    spillway_new$time =  full_time_day_local
    wetland_new$time =  full_time_day_local
    
    if(n_inflow_outflow_members == 1){
      inflow_file_names[m] <-   paste0(working_directory,'/','inflow_file1_mean.csv')
      spillway_file_names[m] <- paste0(working_directory,'/','outflow_file1_mean.csv')
      wetland_file_names[m] <- paste0(working_directory,'/','inflow_file2_mean.csv')
    }else{
      inflow_file_names[m] <-   paste0(working_directory,'/','inflow_file1_ens',m,'.csv')
      spillway_file_names[m] <- paste0(working_directory,'/','outflow_file1_ens',m,'.csv')
      wetland_file_names[m] <- paste0(working_directory,'/','inflow_file2_ens',m,'.csv')
    }
    
    write.csv(inflow_new,
              file = inflow_file_names[m],
              row.names = FALSE,
              quote = FALSE)
    write.csv(spillway_new,
              file = spillway_file_names[m],
              row.names = FALSE,
              quote = FALSE)
    write.csv(wetland_new,
              file = wetland_file_names[m],
              row.names = FALSE,
              quote = FALSE)
  }
  return(list(inflow_file_names = as.character(inflow_file_names),
              spillway_file_names = as.character(spillway_file_names),
              wetland_file_names = as.character(wetland_file_names)))
}

create_sss_input_output <- function(x, i, m, full_time_day_local, working_directory, wq_start, management_input){
  sss_depth <- 8
  depth_index <- which.min(abs(modeled_depths - sss_depth))
  
  time_sss <- c(full_time_day_local[i - 1],full_time_day_local[i])
  FLOW <- c(management_input[i-1, 1], management_input[i, 1])
  TEMP <- round(rep(x[i-1, m, depth_index],2), 3)
  SALT <- rep(0,2)
  
  oxy_temp <- c(x[i-1, m, wq_start[1] + depth_index - 1] + management_input[i-1, 2],
                x[i-1, m, wq_start[1] + depth_index - 1] + management_input[i, 2])
  OXY_oxy <- round(oxy_temp, 3)
  NIT_amm <-  round(rep(x[i-1, m, wq_start[6] + depth_index - 1],2), 3)
  NIT_nit <-  round(rep(x[i-1, m, wq_start[7] + depth_index - 1],2), 3)
  PHS_frp <-  round(rep(x[i-1, m, wq_start[8] + depth_index - 1],2), 3)
  OGM_doc <-  round(rep(x[i-1, m, wq_start[9] + depth_index - 1],2), 3)
  OGM_poc <-  round(rep(x[i-1, m, wq_start[10] + depth_index - 1],2), 3)
  OGM_don <-  round(rep(x[i-1, m, wq_start[11] + depth_index - 1],2), 3)
  OGM_dop <-  round(rep(x[i-1, m, wq_start[13] + depth_index - 1],2), 3)
  OGM_pop <-  round(rep(x[i-1, m, wq_start[14] + depth_index - 1],2), 3)
  OGM_pon <-  round(rep(x[i-1, m, wq_start[12] + depth_index - 1],2), 3)
  PHS_frp_ads <-  round(rep(x[i-1, m, wq_start[16] + depth_index - 1],2), 3)
  
  sss <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT, OXY_oxy,
                    NIT_amm = NIT_amm, NIT_nit = NIT_nit, PHS_frp = PHS_frp,
                    OGM_doc = OGM_doc, OGM_poc = OGM_poc, OGM_don = OGM_don,
                    OGM_dop = OGM_dop, OGM_pop = OGM_pop, OGM_pon = OGM_pon,
                    PHS_frp_ads = PHS_frp_ads)
  
  write.csv(sss, paste0(working_directory, "/sss_inflow.csv"), row.names = FALSE, quote = FALSE)
  write.csv(sss, paste0(working_directory, "/sss_outflow.csv"), row.names = FALSE, quote = FALSE)
}


read_sss_files <-  function(full_time_day_local,
                            working_directory,
                            input_file_tz = 'EST5EDT', 
                            sss_file,
                            local_tzone){
  
  d <- read.csv(paste0(working_directory, "/", sss_file))
  
  TIMESTAMP_in <- as.POSIXct(d$time, 
                             format= "%Y-%m-%d",
                             tz = input_file_tz)
  
  d$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
  

  sss_flow <- rep(NA, length(full_time_day_local))
  sss_OXY_oxy <- rep(NA, length(full_time_day_local))
  
  if(length(which(d$TIMESTAMP == full_time_day_local[1])) > 0){
    
    for(i in 1:(length(full_time_day_local))){
      index <- which(d$TIMESTAMP == full_time_day_local[i])
      sss_flow[i] <- d[index, "SSS_m3.day"]
      sss_OXY_oxy[i] <- d[index, "mmol.O2.m3.day"]
    }
  }
  
  management_input <- data.frame(sss_flow = sss_flow, sss_OXY_oxy = sss_OXY_oxy)
  
  return(management_input)
    
}
