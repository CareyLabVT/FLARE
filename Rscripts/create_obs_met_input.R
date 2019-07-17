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
    
    #Save in GLM Format
    #full_time_hour_obs <- strftime(full_time_hour_obs, 
    #                              format="%Y-%m-%d %H:%M",
    #                              tz = output_tz)
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
