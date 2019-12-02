create_inflow_outflow_file <- function(full_time_day_local,
                                       working_directory,
                                       input_file_tz = 'EST5EDT', 
                                       start_forecast_step,
                                       inflow_file1,
                                       inflow_file2,
                                       outflow_file1,
                                       chemistry_file,
                                       local_tzone,
                                       met_file_names,
                                       forecast_days){
  
  inflow <- read.csv(inflow_file1)
  spillway <- read.csv(outflow_file1)
  wetland <- read.csv(inflow_file2)
  inflow_chemistry <- read.csv(chemistry_file)
  
  inflow_time <- inflow$time 
  spillway_time <- spillway$time
  wetland_time <- wetland$time
  chemistry_time <- inflow_chemistry$time
  
  inflow_new <- as.data.frame(array(NA, dim = c(length(full_time_day_local), 15)))
  wetland_new <- as.data.frame(array(NA, dim = c(length(full_time_day_local), 15)))
  spillway_new <- as.data.frame(array(NA, dim = c(length(full_time_day_local), 2)))
  
  inflow_file_names <- rep(NA, n_inflow_outflow_members)
  spillway_file_names <- rep(NA, n_inflow_outflow_members)
  wetland_file_names <- rep(NA, n_inflow_outflow_members)
  
  forecast_start_day <- day(full_time_day_local[start_forecast_step])
  forecast_start_month <- month(full_time_day_local[start_forecast_step])
  
  first_forecast_day <-  read_csv(paste0(working_directory,"/",met_file_names[1])) %>%
    mutate(day = as_date(time), hour = hour(time)) %>%
    filter(day == full_time_day_local[start_forecast_step] & hour < hour[1])
  
  if(forecast_days > 0 & use_future_inflow){
    curr_all_days <- NULL
    for(m in 2:length(met_file_names)){
      curr_met_daily <- read_csv(paste0(working_directory,"/",met_file_names[m])) %>% 
        #full_join(first_forecast_day) %>% 
        mutate(day = as_date(time)) %>% 
        group_by(day) %>% 
        summarize(Precip = mean(Rain),
                  AirTemp = mean(AirTemp)) %>% 
        mutate(ensemble = m)
      
      curr_all_days <- rbind(curr_all_days,curr_met_daily)
    }
  }
  
  met_ensemble_index <- 1
  
  for(m in 1:n_inflow_outflow_members){
    
    met_ensemble_index <- met_ensemble_index + 1
    if(met_ensemble_index > length(met_file_names)){
      met_ensemble_index <- 2
    }
    
    for(i in 1:length(full_time_day_local)){
      
      curr_day <- day(full_time_day_local[i])
      curr_month <- month(full_time_day_local[i])
      
      #if a historical period OR don't use future inflow (i.e., use the observed because you are hindcasting)
      if(i <= (start_forecast_step) || use_future_inflow == FALSE){
        
        for(j in 2:3){
          
          if(!(full_time_day_local[i] %in% inflow_time)){
            
            #NEED TO MAKE THIS A FUNCTION OF PREVIOUS FLOW
            index1 <- which(day(inflow_time) == curr_day & month(inflow_time) == curr_month)
            if(n_inflow_outflow_members == 1){
              inflow_new[i,j] <- mean(inflow[index1,j], na.rm = TRUE)
            }else{
              inflow_new[i,j] <- rnorm(1, mean(inflow[index1,j], na.rm = TRUE), sd(inflow[index1,j], na.rm = TRUE))
              inflow_new[i,j] <- max(inflow_new[i,j], 0.0)
            }
          }else{
            index1 <- which(inflow_time == full_time_day_local[i])
            inflow_new[i,j] <- inflow[index1,j]
          }
          
          
          if(!(full_time_day_local[i] %in% wetland_time)){
            
            #NEED TO MAKE THIS A FUNCTION OF PREVIOUS FLOW
            index1 <- which(day(wetland_time) == curr_day & month(wetland_time) == curr_month)
            if(n_inflow_outflow_members == 1){
              wetland_new[i,j] <- mean(wetland[index1,j], na.rm = TRUE)
            }else{
              wetland_new[i,j] <- rnorm(1, mean(wetland[index1,j], na.rm = TRUE), sd(wetland[index1,j], na.rm = TRUE))
              wetland_new[i,j] <- max(wetland_new[i,j], 0.0)
            }
          }else{
            index1 <- which(wetland_time == full_time_day_local[i])
            wetland_new[i,j] <- wetland[index1,j]
          }
        }
        
        for(j in 4:ncol(inflow_new)){
          
          if(!(full_time_day_local[i] %in% chemistry_time)){
            
            #NEED TO MAKE THIS A FUNCTION OF PREVIOUS FLOW
            index1 <- which(day(chemistry_time) == curr_day & month(chemistry_time) == curr_month)
            if(n_inflow_outflow_members == 1){
              inflow_new[i,j] <- mean(inflow_chemistry[index1,j], na.rm = TRUE)
            }else{
              inflow_new[i,j] <- rnorm(1, mean(inflow_chemistry[index1,j], na.rm = TRUE), sd(inflow_chemistry[index1,j], na.rm = TRUE))
              inflow_new[i,j] <- max(inflow_new[i,j], 0.0)
            }
          }else{
            index1 <- which(chemistry_time == full_time_day_local[i])
            inflow_new[i,j] <- inflow_chemistry[index1,j]
          }
          
          
          if(!(full_time_day_local[i] %in% wetland_time)){
            
            #NEED TO MAKE THIS A FUNCTION OF PREVIOUS FLOW
            index1 <- which(day(wetland_time) == curr_day & month(wetland_time) == curr_month)
            if(n_inflow_outflow_members == 1){
              wetland_new[i,j] <- mean(wetland[index1,j], na.rm = TRUE)
            }else{
              wetland_new[i,j] <- rnorm(1, mean(wetland[index1,j], na.rm = TRUE), sd(wetland[index1,j], na.rm = TRUE))
              wetland_new[i,j] <- max(wetland_new[i,j], 0.0)
            }
          }else{
            index1 <- which(wetland_time == full_time_day_local[i])
            wetland_new[i,j] <- wetland[index1,j]
          }
        }
        
      }else{ #FORECAST IN THE FUTURE
        
        curr_met_daily <- curr_all_days %>% 
          filter(ensemble == met_ensemble_index & day == full_time_day_local[i])
        
        if(n_inflow_outflow_members == 1){
          curr_met_daily <- curr_all_days %>%
            group_by(day) %>% 
            summarize(Precip = mean(Precip),
                      AirTemp = mean(AirTemp)) %>% 
            filter(day == full_time_day_local[i])
        }
        
        #flow and temperature
        if(n_inflow_outflow_members == 1){
          inflow_error <- 0.0
          temp_error  <- 0.0
        }else{
          inflow_error <- rnorm(1, 0, 0.009416283)
          temp_error <- rnorm(1, 0, 0.7173)
        }
        inflow_new[i,2] <- 0.9483  * inflow_new[i - 1,2] + 0.7093 * curr_met_daily$Precip + inflow_error
        inflow_new[i,2] <- max(c(inflow_new[i,2], 0.0))
        inflow_new[i,3] <- 0.322264   + 0.775594    * inflow_new[i - 1,3] +  0.192049 * curr_met_daily$AirTemp + temp_error
        
        #OVERWRITE FOR NOW UNTIL WE GET AN EQUATION
        #index1 <- which(day(inflow_time) == curr_day & month(inflow_time) == curr_month)
        #inflow_new[i,2] <- rnorm(1, mean(inflow[index1,2], na.rm = TRUE), sd(inflow[index1,2], na.rm = TRUE))
        #inflow_new[i,3]  <- rnorm(1, mean(inflow[index1,3], na.rm = TRUE), sd(inflow[index1,3], na.rm = TRUE))
        #inflow_new[i,2] <- max(inflow_new[i,2], 0.0)
        #inflow_new[i,3] <- max(inflow_new[i,3], 0.0)
        
        index2 <- which(day(wetland_time) == curr_day & month(wetland_time) == curr_month)
        wetland_new[i,2] <- rnorm(1, mean(wetland[index2,2], na.rm = TRUE), sd(wetland[index2,2], na.rm = TRUE))
        wetland_new[i,3]  <- rnorm(1, mean(wetland[index2,3], na.rm = TRUE), sd(wetland[index2,3], na.rm = TRUE))  
        wetland_new[i,2] <- max(wetland_new[i,2], 0.0)
        wetland_new[i,3] <- max(wetland_new[i,3], 0.0)
        
        for(j in 4:ncol(inflow_new)){
          
          index1 <- which(day(chemistry_time) == curr_day & month(chemistry_time) == curr_month)
          index2 <- which(day(wetland_time) == curr_day & month(wetland_time) == curr_month)
          
          if(n_inflow_outflow_members == 1){
            
            inflow_new[i,j] <- mean(inflow_chemistry[index1,j], na.rm = TRUE)
            wetland_new[i,j] <- mean(wetland[index2,j], na.rm = TRUE)
            
          }else{
            
            inflow_new[i,j] <- rnorm(1, mean(inflow_chemistry[index1,j], na.rm = TRUE), sd(inflow_chemistry[index1,j], na.rm = TRUE))
            wetland_new[i,j] <- rnorm(1, mean(wetland[index2,j], na.rm = TRUE), sd(wetland[index2,j], na.rm = TRUE))  
            inflow_new[i,j] <- max(inflow_new[i,j], 0.0)
            wetland_new[i,j] <- max(wetland_new[i,j], 0.0)
            
          }
        }
      }
      
      if(include_wetland_inflow){
        spillway_new[i,2] <- inflow_new[i,2] +  wetland_new[i,2] 
      }else{
        spillway_new[i,2] <- inflow_new[i,2]
      }
    }
    if(n_inflow_outflow_members == 1){
      inflow_file_names[m] <- paste0(working_directory,'/','inflow_file1_mean.csv')
      spillway_file_names[m] <- paste0(working_directory,'/','outflow_file1_mean.csv')
      wetland_file_names[m] <- paste0(working_directory,'/','inflow_file2_mean.csv')
    }else{
      inflow_file_names[m] <- paste0(working_directory,'/','inflow_file1_ens',m,'.csv')
      spillway_file_names[m] <- paste0(working_directory,'/','outflow_file1_ens',m,'.csv')
      wetland_file_names[m] <- paste0(working_directory,'/','inflow_file2_ens',m,'.csv')
    }
    
    inflow_new[,1] <- full_time_day_local
    spillway_new[,1] <- full_time_day_local
    wetland_new[,1] <- full_time_day_local
    
    names(inflow_new) <- c("time","FLOW","TEMP","SALT","OXY_oxy","NIT_amm","NIT_nit", "PHS_frp", "OGM_doc", "OGM_poc",
                           "OGM_don","OGM_dop","OGM_pop", "PHS_frp_ads","OGM_pon")    
    names(wetland_new) <- c("time","FLOW","TEMP","SALT","NIT_amm","NIT_nit", "PHS_frp", "OGM_doc", "OGM_poc",
                            "OGM_don","OGM_dop","OGM_pop", "PHS_frp_ads","OGM_pon","OXY_oxy")   
    names(spillway_new) <- c("time","FLOW")   
    
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