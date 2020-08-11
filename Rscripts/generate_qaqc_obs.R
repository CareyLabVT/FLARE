generate_qaqc_obs <- function(working_directory,met_file,met_raw_obs_fname, local_tzone, forecast_start_day_local, start_day_local,
                              forecast_days,start_time_local, inflow1_file,inflow_raw_file1,nutrients_fname,insitu_obs_fname,
                              data_location, maintenance_file, ctd_fname, lake_name, code_folder){
  
  hist_days <- as.numeric(difftime(as_date(forecast_start_day_local),as_date(start_day_local)))
  
  total_days <- hist_days + forecast_days
  start_forecast_step <- hist_days + 1
  
  start_datetime_local <- as_datetime(paste0(start_day_local," ",start_time_local), tz = local_tzone)
  end_datetime_local <- start_datetime_local + total_days*24*60*60
  forecast_start_time_local <- start_datetime_local + hist_days*24*60*60
  
  full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")
  
  
  cleaned_met_file <- paste0(working_directory, "/met_full_postQAQC.csv")
  if(is.na(met_file)){
    met_qaqc(realtime_file = met_raw_obs_fname[1],
             qaqc_file = met_raw_obs_fname[2],
             cleaned_met_file,
             input_file_tz = "EST",
             local_tzone,
             full_time_local)
  }else{
    file.copy(met_file, cleaned_met_file)
  }
  
  cleaned_inflow_file <- paste0(working_directory, "/inflow_postQAQC.csv")
  
  if(is.na(inflow1_file)){
    inflow_qaqc(realtime_file = inflow_raw_file1[1],
                qaqc_file = inflow_raw_file1[2],
                nutrients_file = nutrients_fname,
                cleaned_inflow_file ,
                local_tzone, 
                input_file_tz = 'EST')
  }else{
    file.copy(inflow1_file, cleaned_inflow_file)
  }
  
  
  cleaned_observations_file_long <- paste0(working_directory, 
                                           "/observations_postQAQC_long.csv")
  if(is.na(combined_obs_file)){
    in_situ_qaqc(insitu_obs_fname = insitu_obs_fname, 
                 data_location = data_location, 
                 maintenance_file = maintenance_file,
                 ctd_fname = ctd_fname, 
                 nutrients_fname = nutrients_fname,
                 cleaned_observations_file_long = cleaned_observations_file_long,
                 lake_name,
                 code_folder)
  }else{
    file.copy(combined_obs_file, cleaned_observations_file_long)
  }
  
}