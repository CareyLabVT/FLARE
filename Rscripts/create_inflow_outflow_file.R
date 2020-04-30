create_inflow_outflow_file <- function(full_time_local,
                                       working_directory,
                                       input_file_tz = 'EST5EDT', 
                                       start_forecast_step,
                                       inflow_file1,
                                       inflow_file2,
                                       outflow_file1,
                                       chemistry_file,
                                       local_tzone,
                                       met_file_names,
                                       forecast_days,
                                       inflow_process_uncertainty,
                                       future_inflow_flow_coeff,
                                       future_inflow_flow_error,
                                       future_inflow_temp_coeff,
                                       future_inflow_temp_error)
{
  
  full_time_day_local <- as_date(full_time_local)
  
  inflow <- read_csv(inflow_file1)
  if(!is.na(inflow_file2)){
    wetland <- read_csv(inflow_file2)
  }
  
  if(include_wq){
    wq_names_tmp <- wq_names[which(wq_names %in% names(inflow))]
  }else{
    wq_names_tmp <- NULL
  }
  
  curr_all_days <- NULL
  
  col_types <- cols(
    time = col_datetime(format = ""),
    ShortWave = col_double(),
    LongWave = col_double(),
    AirTemp = col_double(),
    RelHum = col_double(),
    WindSpeed = col_double(),
    Rain = col_double(),
    Snow = col_double())
  
  for(m in 1:length(met_file_names)){
    curr_met_daily <- read_csv(paste0(working_directory,"/",met_file_names[m]),
                               col_types = col_types) %>% 
      mutate(time = as_date(time)) %>% 
      group_by(time) %>% 
      summarize(Rain = mean(Rain),
                AirTemp = mean(AirTemp)) %>% 
      mutate(ensemble = m) %>% 
      mutate(AirTemp_lag1 = lag(AirTemp, 1),
             Rain_lag1 = lag(Rain, 1))
    
    curr_all_days <- rbind(curr_all_days,curr_met_daily)
  }
  
  
  tmp <- left_join(curr_all_days, inflow, by = "time")
  
  forecasts_days <- full_time_day_local[start_forecast_step:length(full_time_day_local)]
  if(use_future_inflow == FALSE || start_forecast_step == length(full_time_day_local)){
    forecasts_days <- NULL 
  }
  
  tmp <- tmp %>%
    mutate(forecast = ifelse(time %in% forecasts_days, 1, 0),
           TEMP = ifelse(forecast == 1, NA, TEMP),
           FLOW = ifelse(forecast == 1, NA, FLOW))
  
  if(inflow_process_uncertainty == TRUE){
    inflow_error <- rnorm(nrow(tmp), 0, future_inflow_flow_error)
    temp_error <- rnorm(nrow(tmp), 0, future_inflow_temp_error)
  }else{
    inflow_error <- rep(0.0, nrow(tmp))
    temp_error <- rep(0.0, nrow(tmp))
  }
  
  for(i in 1:nrow(tmp)){
    if(tmp$forecast[i] == 0 & is.na(tmp$FLOW[i]) & !include_wq){
      tmp[i, c("FLOW", "TEMP",wq_names_tmp)]  <- inflow %>% 
        filter(time < full_time_day_local[start_forecast_step]) %>% 
        mutate(doy = yday(time)) %>% 
        filter(doy == yday(tmp$time[i])) %>% 
        summarize_at(.vars = c("FLOW", "TEMP", wq_names_tmp), mean, na.rm = TRUE) %>% 
        unlist()
    }
    
    if(tmp$forecast[i] == 1){
      tmp$FLOW[i] = future_inflow_flow_coeff[1] + 
        future_inflow_flow_coeff[2] * tmp$FLOW[i - 1] +  
        future_inflow_flow_coeff[3] * tmp$Rain_lag1[i] + inflow_error[i]
      tmp$TEMP[i] = future_inflow_temp_coeff[1] +  
        future_inflow_temp_coeff[2] * tmp$TEMP[i-1] +  
        future_inflow_temp_coeff[3] * tmp$AirTemp_lag1[i] + temp_error[i]
      if(include_wq){
        tmp[i, c(wq_names_tmp)] <- inflow %>% 
          filter(time < full_time_day_local[start_forecast_step]) %>% 
          mutate(doy = yday(time)) %>% 
          filter(doy == yday(tmp$time[i])) %>% 
          summarize_at(.vars = c(wq_names_tmp), mean, na.rm = TRUE) %>% 
          unlist()
      }
    }
  }
  
  tmp <- tmp %>% 
    mutate(FLOW = ifelse(FLOW < 0.0, 0.0, FLOW))
  
  file_name_base <- met_file_names %>% 
    str_sub(4) 
  inflow1_file_names <- paste0("inflow", file_name_base)
  outflow_file_names <- paste0("outflow", file_name_base)
  
  for(i in 1:n_distinct(tmp$ensemble)){
    tmp2 <- tmp %>% 
      filter(ensemble == i) %>% 
      mutate(SALT = 0.0) %>% 
      select(time, FLOW, TEMP, SALT, all_of(wq_names_tmp)) %>% 
      mutate_at(vars(c("FLOW", "TEMP", "SALT", all_of(wq_names_tmp))), funs(round(., 4)))
    
    
    write_csv(x = tmp2,
              path = paste0(working_directory,"/",inflow1_file_names[i]),
              quote_escape = "none")
    
    tmp2 <- tmp2 %>% 
      select(time, FLOW)
    
    write_csv(x = tmp2,
              path = paste0(working_directory,"/",outflow_file_names[i]),
              quote_escape = "none")
  }
  
  return(list(inflow_file_names = as.character(inflow1_file_names),
              spillway_file_names = as.character(outflow_file_names),
              wetland_file_names = as.character(inflow1_file_names)))
}
