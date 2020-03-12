met_qaqc <- function(fname, cleaned_met_file,input_file_tz,local_tzone,working_directory){

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
    
    d1 <- data.frame(timestamp = d1$TIMESTAMP, ShortWave = d1$SR01Up_Avg, LongWave = d1$IR01UpCo_Avg, AirTemp = d1$AirTC_Avg, RelHum = d1$RH, WindSpeed = d1$WS_ms_Avg, Rain = d1$Rain_mm_Tot)
    d2 <- data.frame(timestamp = d2$TIMESTAMP, ShortWave = d2$ShortwaveRadiationUp_Average_W_m2, LongWave = d2$InfaredRadiationUp_Average_W_m2, AirTemp = d2$AirTemp_Average_C, RelHum = d2$RH_percent, WindSpeed = d2$WindSpeed_Average_m_s, Rain = d2$Rain_Total_mm)
    
    d1 <- d1[which(d1$timestamp > d2$timestamp[nrow(d2)] | d1$timestamp < d2$timestamp[1]), ]
    
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
    
    d <- data.frame(timestamp = d1$TIMESTAMP, ShortWave = d1$SR01Up_Avg, LongWave = d1$IR01UpCo_Avg, AirTemp = d1$AirTC_Avg, RelHum = d1$RH, WindSpeed = d1$WS_ms_Avg, Rain = d1$Rain_mm_Tot)
  }
  
  
  wshgt <- 3
  roughlength <- 0.000114
  d$WindSpeed <- d$WindSpeed * log(10.00 / 0.000114) / log(wshgt / 0.000114)

  maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
  minTempC = -24 # an lower bound of realistic temperature for the study site in deg C

  d <- d %>%
    dplyr::mutate(ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
                  RelHum = ifelse(RelHum < 0, 0, RelHum),
                  RelHum = ifelse(RelHum > 100, 100, RelHum),
                  AirTemp = ifelse(AirTemp> maxTempC, NA, AirTemp),
                  AirTemp = ifelse(AirTemp < minTempC, NA, AirTemp),
                  LongWave = ifelse(LongWave < 0, NA, LongWave),
                  WindSpeed = ifelse(WindSpeed < 0, 0, WindSpeed)) %>%
    filter(is.na(timestamp) == FALSE)
  

  
  write.csv(d, cleaned_met_file)
}