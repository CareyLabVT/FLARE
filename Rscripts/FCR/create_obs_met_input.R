create_obs_met_input <- function(fname,
                                 outfile,
                                 full_time_hour_local,
                                 input_file_tz = "EST5EDT",
                                 local_tzone,
                                 working_directory,
                                 hist_days) {
  
  d <- read_csv(fname)
  
  d$timestamp <- force_tz(d$timestamp, tz = local_tzone)

  ShortWave <- rep(NA, length(full_time_hour_local))
  LongWave <- rep(NA, length(full_time_hour_local))
  AirTemp <- rep(NA, length(full_time_hour_local))
  RelHum <- rep(NA, length(full_time_hour_local))
  WindSpeed <- rep(NA, length(full_time_hour_local))
  Rain <- rep(NA, length(full_time_hour_local))
  Snow <- rep(NA, length(full_time_hour_local))
  
  if(length(which(d$timestamp %in% full_time_hour_local)) > 0){
    
    for(i in 1:(length(full_time_hour_local) - 1)){
      index <- which(d$timestamp == full_time_hour_local[i])
      index_2 <- which(d$timestamp == full_time_hour_local[i + 1])
      if(length(index) > 0 & length(index_2) > 0){
        ShortWave[i] <- max(mean(d$ShortWave[index:index_2], na.rm = TRUE), 0.0)
        LongWave[i] <- mean(d$LongWave[index:index_2], na.rm = TRUE)
        AirTemp[i] <- d$AirTemp[index]
        if(is.na(AirTemp[i])){
          AirTemp[i] <- mean(c(d$AirTemp[index-1],d$AirTemp[index+1]))
        }
        RelHum[i] <- d$RelHum[index]
        if(is.na(RelHum[i])){
          RelHum[i] <- mean(c(d$RelHum[index-1],d$RelHum[index+1]))
        }
        WindSpeed[i] <- mean(d$WindSpeed[index:index_2], na.rm = TRUE)
        Rain[i] <- (sum(d$Rain[index:index_2]) * 24) / 1000
        Snow[i] <- 0
      }
    }
  
    observed_hours <- which(full_time_hour_local <= d$timestamp[length(d$timestamp)])
    
    if(length(observed_hours) < (hist_days * 24)){
      missing_met <- TRUE
    }else{
      missing_met <- FALSE
    }

    ShortWave <- ShortWave[observed_hours]
    LongWave <- LongWave[observed_hours]
    AirTemp <- AirTemp[observed_hours]
    RelHum <- RelHum[observed_hours]
    WindSpeed <- WindSpeed[observed_hours]
    Rain <- Rain[observed_hours]
    Snow <- Snow[observed_hours]
    full_time_hour_local <- full_time_hour_local[observed_hours]
    
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
    write.csv(historical_met, file = paste0(working_directory, "/", outfile), row.names = FALSE, quote = FALSE)
  }else{
    missing_met <- TRUE
  }
  return(missing_met)
}
