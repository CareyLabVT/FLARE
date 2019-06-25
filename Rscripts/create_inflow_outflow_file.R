create_inflow_outflow_file <- function(full_time_day_local,
                                       working_glm,
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
  
  inflow = read.csv(paste0(working_glm,'/', inflow_file1))
  spillway = read.csv(paste0(working_glm,'/', outflow_file1))
  wetland = read.csv(paste0(working_glm,'/', inflow_file2))
  
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
    if(i < (start_forecast_step+1) & hold_inflow_outflow_constant){
      for(j in 2:16){
        inflow_new[i,j] <- mean(inflow[index1,j], na.rm = TRUE)
        wetland_new[i,j] <- mean(wetland[index3,j], na.rm = TRUE)
      }
      spillway_new[i,2] <- mean(spillway[index2,2], na.rm = TRUE)
    }else{
      for(j in 2:16){
        inflow_new[i,j] <- mean(inflow[hist_index1,j], na.rm = TRUE)
        wetland_new[i,j] <- mean(wetland[hist_index3,j], na.rm = TRUE)
      }
      spillway_new[i,2] <- mean(spillway[hist_index2,2], na.rm = TRUE) 
    }
  }
  
  inflow_new$time =  full_time_day_local
  spillway_new$time =  full_time_day_local
  wetland_new$time =  full_time_day_local
  
  write.csv(inflow_new,
            file = paste0(working_glm,'/','inflow_file1.csv'),
            row.names = FALSE,
            quote = FALSE)
  write.csv(spillway_new,
            file = paste0(working_glm,'/','outflow_file1.csv'),
            row.names = FALSE,
            quote = FALSE)
  write.csv(wetland_new,
            file = paste0(working_glm,'/','inflow_file2.csv'),
            row.names = FALSE,
            quote = FALSE)
}