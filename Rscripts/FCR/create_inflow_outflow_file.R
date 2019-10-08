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
  inflow_new = inflow_2017
  
  spillway_2017 =  spillway[which(spillway_time_tmp %in% full_time_day_2017),]
  spillway_new = spillway_2017
  
  wetland_2017 =  wetland[which(wetland_time_tmp %in% full_time_day_2017),]
  wetland_new = wetland_2017
  
  inflow_file_names <- rep(NA, n_inflow_outflow_members)
  spillway_file_names <- rep(NA, n_inflow_outflow_members)
  wetland_file_names <- rep(NA, n_inflow_outflow_members)
  
  forecast_start_day <- day(full_time_day_local[start_forecast_step])
  forecast_start_month <- month(full_time_day_local[start_forecast_step])
  
  for(m in 1:(n_inflow_outflow_members)){
    for(i in 1:length(full_time_day_local)){
      curr_day <- day(full_time_day_local[i])
      curr_month <- month(full_time_day_local[i])
      
      if(i < (start_forecast_step)){
        for(j in 2:ncol(inflow)){
          if(full_time_day_local[i] > inflow_time_tmp[nrow(inflow)] | full_time_day_local[i] < inflow_time_tmp[1]){
            index1 <- which(day(inflow_time_tmp) == curr_day & month(inflow_time_tmp) == curr_month)
            index2 <- which(day(wetland_time_tmp) == curr_day & month(wetland_time_tmp) == curr_month)
            if(n_inflow_outflow_members == 1){
              inflow_new[i,j] <- mean(inflow[index1,j], na.rm = TRUE)
              wetland_new[i,j] <- mean(wetland[index2,j], na.rm = TRUE)
            }else{
              inflow_new[i,j] <- rnorm(1, mean(inflow[index1,j], na.rm = TRUE), sd(inflow[index1,j], na.rm = TRUE))
              wetland_new[i,j] <- rnorm(1, mean(wetland[index2,j], na.rm = TRUE), sd(wetland[index2,j], na.rm = TRUE))
            }
          }else{
            index1 <- which(inflow_time_tmp == full_time_day_local[i])
            index2 <- which(wetland_time_tmp == full_time_day_local[i])
            inflow_new[i,j] <- inflow[index1,j]
            wetland_new[i,j] <- wetland[index2,j]
          }
        }
        spillway_new[i,2] <- inflow_new[i,2] +  wetland_new[i,2] 
        
      }else{
        
        for(j in 2:ncol(inflow)){
          index1 <- which(day(inflow_time_tmp) == curr_day & month(inflow_time_tmp) == curr_month)
          index2 <- which(day(wetland_time_tmp) == curr_day & month(wetland_time_tmp) == curr_month)
          if(n_inflow_outflow_members == 1){
            if(hold_inflow_outflow_constant){
              index1 <- which(day(inflow_time_tmp) == forecast_start_day & month(inflow_time_tmp) == forecast_start_month)
              index2 <- which(day(wetland_time_tmp) == forecast_start_day & month(wetland_time_tmp) == forecast_start_month)
              inflow_new[i,j] <- mean(inflow[index1,j], na.rm = TRUE)
              wetland_new[i,j] <- mean(wetland[index2,j], na.rm = TRUE)
            }else{
              inflow_new[i,j] <- mean(inflow[index1,j], na.rm = TRUE)
              wetland_new[i,j] <- mean(wetland[index2,j], na.rm = TRUE)    
            }
          }else{
            if(hold_inflow_outflow_constant){
              index1 <- which(day(inflow_time_tmp) == forecast_start_day & month(inflow_time_tmp) == forecast_start_month)
              index2 <- which(day(wetland_time_tmp) == forecast_start_day & month(wetland_time_tmp) == forecast_start_month)
              inflow_new[i,j] <- mean(inflow[index1,j], na.rm = TRUE)
              wetland_new[i,j] <- mean(wetland[index2,j], na.rm = TRUE)   
            }else{
              inflow_new[i,j] <- rnorm(1, mean(inflow[index1,j], na.rm = TRUE), sd(inflow[index1,j], na.rm = TRUE))
              wetland_new[i,j] <- rnorm(1, mean(wetland[index2,j], na.rm = TRUE), sd(wetland[index2,j], na.rm = TRUE))  
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