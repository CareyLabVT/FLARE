create_obs_met_input <- function(fname,
                                 outfile,
                                 full_time_hour_local,
                                 input_file_tz = "EST5EDT",
                                 local_tzone) {
  if(length(fname) > 1){
    d1 <- read_csv(fname[1], skip = 3)
    d_names <- read_csv(fname[1], skip = 1, n_max = 3)
    names(d1) <- names(d_names)
    
    d1 <- d1[-85572, ]
    
    TIMESTAMP_in <- force_tz(d1$TIMESTAMP, tzone = input_file_tz)
    
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d2 <- read_csv(fname[2])
    
    TIMESTAMP_in <- force_tz(d2$DateTime, tzone = input_file_tz)
    
    d2$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    #d3 <- read.csv( fname[3])
    #TIMESTAMP_in <- as.POSIXct(d3$time, 
    #                           format= "%Y-%m-%d %H:%M",
    #                           tz = input_file_tz)
    
    
    #d3$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d1 <- data.frame(TIMESTAMP = d1$TIMESTAMP, ShortWave = d1$SR01Up_Avg, LongWave = d1$IR01UpCo_Avg, AirTemp = d1$AirTC_Avg, RelHum = d1$RH, WindSpeed = d1$WS_ms_Avg, Rain = d1$Rain_mm_Tot)
    d2 <- data.frame(TIMESTAMP = d2$TIMESTAMP, ShortWave = d2$ShortwaveRadiationUp_Average_W_m2, LongWave = d2$InfaredRadiationUp_Average_W_m2, AirTemp = d2$AirTemp_Average_C, RelHum = d2$RH_percent, WindSpeed = d2$WindSpeed_Average_m_s, Rain = d2$Rain_Total_mm)
    
    d1 <- d1[which(d1$TIMESTAMP > d2$TIMESTAMP[nrow(d2)] | d1$TIMESTAMP < d2$TIMESTAMP[1]), ]
    
    #d3 <- d3[which(d3$TIMESTAMP < d2$TIMESTAMP[1])]
    
    d <- rbind(d2, d1)
    
  }else{
    
    d1 <- read.csv( fname, skip = 3)
    d_names <- read.csv(fname, skip = 1)
    names(d1) <- names(d_names)
    
    d1 <- d1[-85572, ]
    
    TIMESTAMP_in <- as.POSIXct(d1$TIMESTAMP, 
                               format= "%Y-%m-%d %H:%M",
                               tz = input_file_tz)
    
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
    
    d <- data.frame(TIMESTAMP = d1$TIMESTAMP, ShortWave = d1$SR01Up_Avg, LongWave = d1$IR01UpCo_Avg, AirTemp = d1$AirTC_Avg, RelHum = d1$RH, WindSpeed = d1$WS_ms_Avg, Rain = d1$Rain_mm_Tot)
    
    
  }
  
  full_time_hour_local <- as.POSIXct(full_time_hour_local, tz = local_tz)
  ShortWave <- rep(NA, length(full_time_hour_local))
  LongWave <- rep(NA, length(full_time_hour_local))
  AirTemp <- rep(NA, length(full_time_hour_local))
  RelHum <- rep(NA, length(full_time_hour_local))
  WindSpeed <- rep(NA, length(full_time_hour_local))
  Rain <- rep(NA, length(full_time_hour_local))
  Snow <- rep(NA, length(full_time_hour_local))
  
  if(length(which(d$TIMESTAMP == full_time_hour_local[1])) > 0){
    
    for(i in 1:(length(full_time_hour_local) - 1)){
      index <- which(d$TIMESTAMP == full_time_hour_local[i])
      index_2 <- which(d$TIMESTAMP == full_time_hour_local[i + 1])
      if(length(index) > 0 & length(index_2) > 0){
        ShortWave[i] <- max(mean(d$ShortWave[index:index_2]), 0.0)
        LongWave[i] <- mean(d$LongWave[index:index_2])
        AirTemp[i] <- d$AirTemp[index]
        RelHum[i] <- d$RelHum[index]
        if(is.na(RelHum[i])){
          RelHum[i] <- mean(c(d$RelHum[index-1],d$RelHum[index+1]))
        }
        WindSpeed[i] <- mean(d$WindSpeed[index:index_2])
        Rain[i] <- (sum(d$Rain[index:index_2]) * 24) / 1000
        Snow[i] <- 0
      }
    }
    
   #remove_hours <- which(!is.na(AirTemp))
    
    #ShortWave <- ShortWave[remove_hours]
    #LongWave <- LongWave[remove_hours]
    #AirTemp <- AirTemp[remove_hours]
    #RelHum <- RelHum[remove_hours]
    #WindSpeed <- WindSpeed[remove_hours]
    #Rain <- Rain[remove_hours]
    #Snow <- Snow[remove_hours]
    #full_time_hour_local <- full_time_hour_local[remove_hours]
    
    
    ShortWave <- na.interpolation(ShortWave, option = "linear")
    LongWave <- na.interpolation(LongWave, option = "linear")
    AirTemp <- na.interpolation(AirTemp, option = "linear")
    RelHum <- na.interpolation(RelHum, option = "linear")
    WindSpeed <- na.interpolation(WindSpeed, option = "linear")
    Rain <- na.interpolation(Rain, option = "linear")        
    Snow <- na.interpolation(Snow, option = "linear")      
    
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
