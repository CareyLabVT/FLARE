process_GEFS2GLM <- function(in_directory, 
                             out_directory, 
                             file_name, 
                             input_tz = "GMT",
                             output_tz = "GMT") {
  
  
  f <- paste0(in_directory,"/",file_name,".csv")
  if(!file.exists(f)){
    print("Missing forecast file!")
    print(f)
    stop()
  }else{
    d <- read.csv(paste0(in_directory,"/",file_name,".csv")) 
    forecast.date_local <- as.POSIXct(d$forecast.date, tz = input_tz)
    d$forecast.date <- as.POSIXct(forecast.date_local, tz = output_tz)
    
    full_time <- rep(NA, length(d$forecast.date) * 6)
    
    begin_step <- head(d$forecast.date, 1)
    end_step <- tail(d$forecast.date, 1)
    full_time <- seq(begin_step, end_step, by = "1 hour") # grid
    
    ShortWave <- array(NA, dim = c(length(full_time), 21))
    LongWave <- array(NA, dim = c(length(full_time), 21))
    AirTemp <- array(NA, dim = c(length(full_time), 21))
    RelHum <- array(NA, dim = c(length(full_time), 21))
    WindSpeed <-array(NA, dim = c(length(full_time), 21))
    Rain <- array(NA, dim = c(length(full_time), 21))
    Snow <- array(0, dim = c(length(full_time), 21))
    
    
    for(ens in 1:21){
      for(i in 1:length(full_time)){
        index = which(d$forecast.date == full_time[i] & d$ensembles == ens)
        if(length(index) > 0){
          if(d$dswrfsfc[index] < 3000){
            ShortWave[(i-6):(i-1),ens] =  d$dswrfsfc[index]
          }
          if(d$dlwrfsfc[index] < 3000){
            LongWave[(i-6):(i-1),ens] = d$dlwrfsfc[index]
          }
          if(d$tmp2m[index] < 3000){
            AirTemp[i,ens] = d$tmp2m[index]
          }
          if(d$rh2m[index] < 3000){
            RelHum[i,ens] =  d$rh2m[index]
          }
          uwind = d$ugrd10m[index]
          vwind= d$vgrd10m[index]
          if(uwind < 3000 & vwind < 3000){
            WindSpeed[i,ens] = sqrt(uwind^2 + vwind^2)
          }
          if(d$pratesfc[index] < 3000){
            Rain[(i-6):(i-1),ens] = d$pratesfc[index]
          }
        }
      }
    }
    
    #Linearly interpolate the variables that are states of the system
    for(ens in 1:21){
      AirTemp[,ens] = imputeTS::na.interpolation(AirTemp[,ens], option = "linear")
      RelHum[,ens] = imputeTS::na.interpolation(RelHum[,ens], option = "linear")
      WindSpeed[,ens] = imputeTS::na.interpolation(WindSpeed[,ens], option = "linear")
    }
    
    #kg/m2/s -> m/day
    #1 kg/m2/s = mm/s
    Rain <- Rain * 60 * 60 * 24 * 0.001 #convert to kg/m2/s to mm/day then to m/day
    
    AirTemp <- AirTemp - 273.15
    
    #Save in GLM Format
    full_time <- strftime(full_time, format="%Y-%m-%d %H:%M", tz = output_tz)
    file_names <- rep(NA, 21)
    for(ens in 1:21){
      GLM_climate <- data.frame(full_time,
                                ShortWave[,ens],
                                LongWave[,ens],
                                AirTemp[,ens],
                                RelHum[,ens],
                                WindSpeed[,ens],
                                Rain[,ens],
                                Snow[,ens])
      n <- noquote(c("time",
                     "ShortWave",
                     "LongWave",
                     "AirTemp",
                     "RelHum", 
                     "WindSpeed", 
                     "Rain", 
                     "Snow"))
      
      colnames(GLM_climate) <- noquote(c("time", 
                                         "ShortWave",
                                         "LongWave",
                                         "AirTemp",
                                         "RelHum",
                                         "WindSpeed",
                                         "Rain",
                                         "Snow"))
      
      file_names[ens] <- paste0(out_directory, "/",
                                "met_hourly_", 
                                file_name, 
                                "_ens", ens, ".csv")
      
      write.csv(GLM_climate, file <- file_names[ens], row.names = FALSE, quote = FALSE)
    }
  }
  return(file_names)
}