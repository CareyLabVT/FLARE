create_met_drivers <- function(start_day_local,start_time_local, local_tzone, hist_days,lake_name, n_met_members, n_ds_members,
                               specified_metfile, working_directory,code_folder,noaa_location,cleaned_met_file,
                               FIT_PARAMETERS,DOWNSCALE_MET,
                               met_downscale_uncertainty,
                               downscaling_coeff,
                               met_ds_obs_start,
                               met_ds_obs_end,
                               weather_uncertainty, use_future_met){
  
  source(paste0(code_folder,"/","Rscripts/met_downscale/process_downscale_GEFS.R")) 
  source(paste0(code_folder,"/","Rscripts/create_obs_met_input.R"))
  
  total_days <- hist_days + forecast_days
  start_forecast_step <- hist_days + 1
  
  start_datetime_local <- as_datetime(paste0(start_day_local," ",start_time_local), tz = local_tzone)
  end_datetime_local <- start_datetime_local + total_days*24*60*60
  forecast_start_time_local <- start_datetime_local + hist_days*24*60*60
  
  full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")
  
  ### All of this is for working with the NOAA data #####
  start_datetime_GMT <- with_tz(first(full_time_local), tzone = "GMT")
  end_datetime_GMT <- with_tz(last(full_time_local), tzone = "GMT")
  forecast_start_time_GMT<- with_tz(forecast_start_time_local, tzone = "GMT")
  
  forecast_start_time_GMT_past <- forecast_start_time_GMT - days(1)
  
  noaa_hour <- NA
  if(!hour(forecast_start_time_GMT) %in% c(0,6,12,18) & forecast_days > 0){
    stop(paste0("local_start_datetime of ", local_start_datetime," does not have a corresponding GMT time with a NOAA forecast
                The GMT times that are avialable are 00:00:00, 06:00:00, 12:00:00, and 18:00:00"))
  }else{
    if(hour(forecast_start_time_GMT) == 0){
      noaa_hour <- "00"
    }
    if(hour(forecast_start_time_GMT) == 6){
      noaa_hour <- "06"
    }
    if(hour(forecast_start_time_GMT) == 12){
      noaa_hour <- "12"
    }
    if(hour(forecast_start_time_GMT) == 18){
      noaa_hour <- "18"
    }
  }
  
  if(day(forecast_start_time_GMT) < 10){
    forecast_day_GMT <- paste0("0", day(forecast_start_time_GMT))
  }else{
    forecast_day_GMT <- paste0(day(forecast_start_time_GMT))
  }
  if(month(forecast_start_time_GMT) < 10){
    forecast_month_GMT <- paste0("0", month(forecast_start_time_GMT))
  }else{
    forecast_month_GMT <- paste0(month(forecast_start_time_GMT))
  }
  
  if(day(forecast_start_time_GMT_past) < 10){
    forecast_day_GMT_past <- paste0("0", day(forecast_start_time_GMT_past))
  }else{
    forecast_day_GMT_past <- paste0(day(forecast_start_time_GMT_past))
  }
  if(month(forecast_start_time_GMT_past) < 10){
    forecast_month_GMT_past <- paste0("0", month(forecast_start_time_GMT_past))
  }else{
    forecast_month_GMT_past <- paste0(month(forecast_start_time_GMT_past))
  }
  
  forecast_base_name <- paste0(lake_name,"_",
                               year(forecast_start_time_GMT),
                               forecast_month_GMT,
                               forecast_day_GMT,"_",
                               "gep_all_",
                               noaa_hour,
                               "z")
  
  forecast_base_name_past <- paste0(lake_name,"_",
                                    year(forecast_start_time_GMT_past),
                                    forecast_month_GMT_past,
                                    forecast_day_GMT_past,"_",
                                    "gep_all_",
                                    noaa_hour,
                                    "z")
  
  met_forecast_base_file_name <- paste0("met_hourly_",
                                        forecast_base_name,
                                        "_ens")
  
  met_file_names <- rep(NA, (n_met_members*n_ds_members))
  obs_met_outfile <- "met_historical.csv"
  
  
  if(is.na(specified_metfile)){
    historical_met <- create_obs_met_input(fname = cleaned_met_file,
                                           outfile = obs_met_outfile,
                                           full_time_local, 
                                           local_tzone,
                                           working_directory,
                                           hist_days)
    
    
  }else{
    missing_met <- FALSE
    historical_met <- read_csv(specified_metfile, col_types = )
  }
  
  if(missing_met  == FALSE){
    met_file_names[] <- obs_met_outfile
  }else{
    if(hist_days > 1){
      stop(paste0("Running more than 1 hist_day but met data has ",
                  missing_met," values"))
    }
    in_directory <- paste0(noaa_location)
    out_directory <- working_directory
    file_name <- forecast_base_name_past
    
    VarInfo <- data.frame("VarNames" = c("AirTemp",
                                         "WindSpeed",
                                         "RelHum",
                                         "ShortWave",
                                         "LongWave",
                                         "Rain"),
                          "VarType" = c("State",
                                        "State",
                                        "State",
                                        "Flux",
                                        "Flux",
                                        "Flux"),
                          "ds_res" = c("hour",
                                       "hour",
                                       "hour",
                                       "hour",
                                       "6hr",
                                       "6hr"),
                          "debias_method" = c("lm",
                                              "lm",
                                              "lm",
                                              "lm",
                                              "lm",
                                              "none"),
                          "use_covariance" = c(TRUE,
                                               FALSE,
                                               TRUE,
                                               TRUE,
                                               TRUE,
                                               FALSE),
                          stringsAsFactors = FALSE)
    
    replaceObsNames <- c("AirTemp" = "AirTemp",
                         "WindSpeed" = "WindSpeed",
                         "RelHum" = "RelHum",
                         "ShortWave" = "ShortWave",
                         "LongWave" = "LongWave",
                         "Rain" = "Rain")
    
    temp_met_file<- process_downscale_GEFS(folder = code_folder,
                                           noaa_location,
                                           input_met_file = cleaned_met_file,
                                           working_directory,
                                           n_ds_members,
                                           n_met_members,
                                           file_name,
                                           local_tzone,
                                           FIT_PARAMETERS,
                                           DOWNSCALE_MET,
                                           met_downscale_uncertainty = FALSE,
                                           compare_output_to_obs = FALSE,
                                           VarInfo,
                                           replaceObsNames,
                                           downscaling_coeff,
                                           full_time_local,
                                           first_obs_date = met_ds_obs_start,
                                           last_obs_date = met_ds_obs_end,
                                           input_met_file_tz = local_tzone,
                                           weather_uncertainty)
    
    met_file_names[1] <- temp_met_file[1]
  }
  
  ###CREATE FUTURE MET FILES
  if(forecast_days > 0 & use_future_met){
    in_directory <- paste0(noaa_location)
    out_directory <- working_directory
    file_name <- forecast_base_name
    
    VarInfo <- data.frame("VarNames" = c("AirTemp",
                                         "WindSpeed",
                                         "RelHum",
                                         "ShortWave",
                                         "LongWave",
                                         "Rain"),
                          "VarType" = c("State",
                                        "State",
                                        "State",
                                        "Flux",
                                        "Flux",
                                        "Flux"),
                          "ds_res" = c("hour",
                                       "hour",
                                       "hour",
                                       "hour",
                                       "6hr",
                                       "6hr"),
                          "debias_method" = c("lm",
                                              "lm",
                                              "lm",
                                              "lm",
                                              "lm",
                                              "none"),
                          "use_covariance" = c(TRUE,
                                               TRUE,
                                               TRUE,
                                               TRUE,
                                               TRUE,
                                               FALSE),
                          stringsAsFactors = FALSE)
    
    replaceObsNames <- c("AirTemp" = "AirTemp",
                         "WindSpeed" = "WindSpeed",
                         "RelHum" = "RelHum",
                         "ShortWave" = "ShortWave",
                         "LongWave" = "LongWave",
                         "Rain" = "Rain")
    
    forecasted_met <- process_downscale_GEFS(folder = code_folder,
                                             noaa_location,
                                             input_met_file = cleaned_met_file,
                                             working_directory,
                                             n_ds_members,
                                             n_met_members,
                                             file_name,
                                             local_tzone,
                                             FIT_PARAMETERS,
                                             DOWNSCALE_MET,
                                             met_downscale_uncertainty,
                                             compare_output_to_obs = FALSE,
                                             VarInfo,
                                             replaceObsNames,
                                             downscaling_coeff,
                                             full_time_local,
                                             first_obs_date = met_ds_obs_start,
                                             last_obs_date = met_ds_obs_end,
                                             input_met_file_tz = local_tzone,
                                             weather_uncertainty)
    
    
    met_drivers <- rbind(forecasted_met,historical_met) %>% 
      arrange(NOAA.member,dscale.member,time)
  }else{
    met_drivers <- historical_met %>% 
      arrange(NOAA.member,dscale.member,time)
  }
  write_csv(met_drivers, paste0(working_directory, "/met_drivers.csv"))
}