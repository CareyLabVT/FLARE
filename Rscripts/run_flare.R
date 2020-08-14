#----------------------------------------------------------#
# Program name: run_enkf_forecast                          #
# Author: R. Quinn Thomas, rqthomas@vt.edu                 #
# Purpose: Sets up and launches the ensemble Kalman Filter #
#          Downloads and process model inputs and sensor   #
#          observations                                    #
# ---------------------------------------------------------#

#' Short Description.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)

run_flare<-function(start_day_local,
                    start_time_local,
                    forecast_start_day_local,
                    sim_name = NA, 
                    hist_days = 1,
                    forecast_days = 16,  
                    spin_up_days = 0,
                    restart_file = NA,
                    uncert_mode = 1,
                    forecast_sss_on,
                    forecast_project_id){
  
  #################################################
  ### LOAD R FUNCTIONS AND OTHER INITIAL SET UP
  #################################################
  
  source(paste0(code_folder,"/","Rscripts/edit_nml_functions.R"))
  source(paste0(code_folder,"/","Rscripts/archive_forecast.R"))
  source(paste0(code_folder,"/","Rscripts/write_forecast_netcdf.R")) 
  source(paste0(code_folder,"/","Rscripts/run_EnKF.R")) 
  source(paste0(code_folder,"/","Rscripts/met_downscale/process_downscale_GEFS.R")) 
  source(paste0(code_folder,"/","Rscripts/update_qt.R"))
  source(paste0(code_folder,"/","Rscripts/glmtools.R"))
  source(paste0(code_folder,"/","Rscripts/localization.R")) 
  source(paste0(code_folder,"/","Rscripts/temperature_to_density.R"))
  source(paste0(code_folder,"/","Rscripts/extract_observations.R"))
  source(paste0(code_folder,"/","Rscripts/create_obs_met_input.R"))
  source(paste0(code_folder,"/","Rscripts/create_sss_input_output.R"))
  source(paste0(code_folder,"/","Rscripts/create_inflow_outflow_file.R"))
  source(paste0(code_folder,"/","Rscripts/read_sss_files.R"))
  source(paste0(code_folder,"/","Rscripts/create_flare_eml.R"))
  
  source(paste0(code_folder,"/","Rscripts/",lake_name_code,"/in_situ_qaqc.R"))  
  source(paste0(code_folder,"/","Rscripts/",lake_name_code,"/met_qaqc.R")) 
  source(paste0(code_folder,"/","Rscripts/",lake_name_code,"/inflow_qaqc.R")) 
  
  source(paste0(code_folder,"/","Rscripts/define_model_",model_name,".R")) 
  
  
  ### METEROLOGY DOWNSCALING OPTIONS
  if(is.na(downscaling_coeff)){
    FIT_PARAMETERS <- TRUE
  }else{
    FIT_PARAMETERS <- FALSE
  }
  
  if(DOWNSCALE_MET == FALSE){
    FIT_PARAMETERS <- FALSE
  }
  
  #################################################
  ### OPTIONS TO ISOLATE COMPONENTS OF UNCERTAINTY
  #################################################
  
  if(uncert_mode == 1){
    #All sources of uncertainty and data used to constrain 
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- TRUE
    weather_uncertainty <- TRUE
    initial_condition_uncertainty <- TRUE
    parameter_uncertainty <- TRUE
    met_downscale_uncertainty <- TRUE
    inflow_process_uncertainty <- TRUE
  }else if(uncert_mode == 2){
    #No sources of uncertainty  data used to constrain 
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    inflow_process_uncertainty <- FALSE
  }else if(uncert_mode == 3){
    #Only process uncertainty
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- TRUE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    inflow_process_uncertainty <- FALSE
  }else if(uncert_mode == 4){
    #only noaa weather uncertainty
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- TRUE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    inflow_process_uncertainty <- FALSE
  }else if(uncert_mode == 5){
    #only initial condition uncertainty with data constraint
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- TRUE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    inflow_process_uncertainty <- FALSE
  }else if(uncert_mode == 6){
    #only initial condition uncertainty without data constraint
    use_obs_constraint <- FALSE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- TRUE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    inflow_process_uncertainty <- FALSE
  }else if(uncert_mode == 7){
    #only parameter uncertainty
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- TRUE
    met_downscale_uncertainty <- FALSE
    inflow_process_uncertainty <- FALSE
  }else if(uncert_mode == 8){
    #only met downscale uncertainty
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- TRUE
    inflow_process_uncertainty <- FALSE
  }else if(uncert_mode == 9){
    #No sources of uncertainty and no data used to constrain 
    use_obs_constraint <- FALSE
    #SOURCES OF uncertainty
    observation_uncertainty <- FALSE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    inflow_process_uncertainty <- FALSE
  }else if(uncert_mode == 10){
    #Only inflow uncertainty
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    inflow_process_uncertainty <- TRUE
  }else if(uncert_mode == 11){
    #All sources of uncertainty and data used to constrain 
    use_obs_constraint <- FALSE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- TRUE
    weather_uncertainty <- TRUE
    initial_condition_uncertainty <- TRUE
    parameter_uncertainty <- TRUE
    met_downscale_uncertainty <- TRUE
    inflow_process_uncertainty <- TRUE
  }
  
  if(observation_uncertainty == FALSE){
    obs_error_temperature <- 0.000001
  }
  
  if(single_run){
    #No sources of uncertainty and no data used to constrain 
    use_obs_constraint <- FALSE
    #SOURCES OF uncertainty
    observation_uncertainty <- FALSE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    inflow_process_uncertainty <- FALSE
    spin_up_days <- hist_days + 2
    ensemble_size <- 3
  }
  
  ####################################################
  #### DETECT PLATFORM  
  ####################################################
  
  switch(Sys.info() [["sysname"]],
         Linux = { machine <- "unix" },
         Darwin = { machine <- "mac" },
         Windows = { machine <- "windows"})
  
  #INSTALL libnetcdf
  if(machine == "unix") {
    system("if [ $(dpkg-query -W -f='${Status}' libnetcdf-dev 2>/dev/null | grep -c 'ok installed') -eq 0 ]; then sudo apt update && sudo apt install libnetcdf-dev; fi;")
    Sys.setenv(LD_LIBRARY_PATH=paste("../glm/unix/", Sys.getenv("LD_LIBRARY_PATH"),sep=":"))
  }
  
  ####################################################
  #### STEP 1: CREATE TIME VECTORS
  ####################################################
  
  total_days <- hist_days + forecast_days
  start_forecast_step <- hist_days + 1
  
  start_datetime_local <- as_datetime(paste0(start_day_local," ",start_time_local), tz = local_tzone)
  end_datetime_local <- start_datetime_local + total_days*24*60*60
  forecast_start_time_local <- start_datetime_local + hist_days*24*60*60
  
  full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")
  
  ####################################################
  #### STEP 2: SET ARRAY DIMENSIONS
  ####################################################
  
  if(!is.na(par_file)){
    pars_config <- read_csv(par_file, col_types = cols())
    npars <- nrow(pars_config)
  }else{
    npars <- 0
  }
  
  obs_config <- read_csv(obs_config_file, col_types = cols())
  
  states_config <- read_csv(states_config_file, col_types = cols())
  
  nsteps <- length(full_time_local)
  if(spin_up_days > nsteps){
    spin_up_days <- nsteps
  }
  
  if(nrow(states_config) > 0){
    include_wq <- TRUE
  }else{
    include_wq <- FALSE
  }
  
  ndepths_modeled <- length(modeled_depths)
  nstates <- ndepths_modeled * length(states_config$state_names)
  num_wq_vars <-  length(states_config$state_names) - 1
  
  glm_output_vars <- states_config$state_names
  
  n_met_members <- 21
  # SET UP NUMBER OF ENSEMBLE MEMBERS
  if(forecast_days > 0 & use_future_met == TRUE  & (ensemble_size %% (n_met_members * n_ds_members)) != 0){
    stop(paste0("ensemble_size (",ensemble_size,") is not a multiple of the number of
                n_met_members (",n_met_members,
                ") * n_ds_members (",n_ds_members,")"))
  }
  
  if(single_run){
    n_met_members <- 3
    n_ds_members <- 1
  }
  
  ####################################################
  #### STEP 3: ORGANIZE FILES
  ####################################################
  
  ###CREATE DIRECTORY PATHS AND STRUCTURE
  working_directory <- paste0(execute_location, "/", "working_directory")
  if(!dir.exists(working_directory)){
    dir.create(working_directory, showWarnings = FALSE)
  }
  ####Clear out temp GLM working directory
  unlink(paste0(working_directory, "/*"), recursive = FALSE)   
  
  
  if(is.na(sim_name)){
    sim_name <- paste0(year(full_time_local[1]), "_",
                       month(full_time_local[1]), "_",
                       day(full_time_local[1]))
  }
  
  ####################################################
  #### STEP 4: PROCESS RAW INPUT AND OBSERVATION DATA
  ####################################################
  
  #### START QAQC CONTAINER ####
  
  if(pull_from_git){
    
    if(!file.exists(realtime_insitu_location)){
      stop("Missing temperature data GitHub repo")
    }
    if(!file.exists(realtime_met_station_location)){
      stop("Missing met station data GitHub repo")
    }
    if(!file.exists(noaa_location)){
      stop("Missing NOAA forecast GitHub repo")
    }
    if(!file.exists(manual_data_location)){
      stop("Missing Manual data GitHub repo")
    }
    
    if(!file.exists(realtime_inflow_data_location)){
      stop("Missing Inflow data GitHub repo")
    }
    
    setwd(realtime_insitu_location)
    system(paste0("git pull"))
    
    setwd(realtime_met_station_location)
    system(paste0("git pull"))
    
    setwd(noaa_location)
    system(paste0("git pull"))
    
    setwd(manual_data_location)
    system(paste0("git pull"))
    
    setwd(realtime_inflow_data_location)
    system(paste0("git pull"))
    
  }
  
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
               lake_name_code,
               code_folder)
  }else{
    file.copy(combined_obs_file, cleaned_observations_file_long)
  }
  
  #### END QAQC CONTAINER ####
  
  ####################################################
  #### STEP 5: PROCESS DRIVER DATA INTO MODEL FORMAT
  ####################################################
  
  #### START DRIVER CONTAINER ####
  
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
  
  forecast_base_name <- paste0(lake_name_code,"_",
                               year(forecast_start_time_GMT),
                               forecast_month_GMT,
                               forecast_day_GMT,"_",
                               "gep_all_",
                               noaa_hour,
                               "z")
  
  forecast_base_name_past <- paste0(lake_name_code,"_",
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
  missing_met <- create_obs_met_input(fname = cleaned_met_file,
                                      outfile = obs_met_outfile,
                                      full_time_local, 
                                      local_tzone,
                                      working_directory,
                                      hist_days)
  }else{
    missing_met <- FALSE
    file.copy(specified_metfile, paste0(working_directory,"/",obs_met_outfile))
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
                                           weather_uncertainty,
                                           obs_met_outfile)
    
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
    
    met_file_names[] <- process_downscale_GEFS(folder = code_folder,
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
                                               weather_uncertainty,
                                               obs_met_outfile
    )
  }
  
  inflow_met_file_names <- met_file_names
  
  if(weather_uncertainty == FALSE){
    #n_enkf_members <- n_enkf_members * n_met_members
    n_met_members <- 1
  }
  
  ##CREATE INFLOW AND OUTFILE FILES
  
  inflow_outflow_files <- create_inflow_outflow_file(full_time_local,
                                                     working_directory, 
                                                     input_file_tz = "EST",
                                                     start_forecast_step,
                                                     inflow_file1 = cleaned_inflow_file,
                                                     inflow_file2,
                                                     outflow_file1,
                                                     chemistry_file = cleaned_inflow_file,
                                                     local_tzone,
                                                     met_file_names,
                                                     forecast_days,
                                                     inflow_process_uncertainty,
                                                     future_inflow_flow_coeff,
                                                     future_inflow_flow_error,
                                                     future_inflow_temp_coeff,
                                                     future_inflow_temp_error,
                                                     states_config,
                                                     include_wq)
  
  
  if(is.na(specified_inflow1)){
  inflow_file_names <- cbind(inflow1 = inflow_outflow_files$inflow_file_names,
                             inflow2 = inflow_outflow_files$wetland_file_names)
  outflow_file_names <- cbind(inflow_outflow_files$spillway_file_names)
  }else{
    inflow_file_names <- cbind(inflow1 = specified_inflow1,
                               inflow2 = specified_inflow2)
    outflow_file_names <- cbind(specified_outflow1)
  }
  
  
  
  #### END DRIVER CONTAINER ####
  
  
  #### START GLM ENKF CONTAINER ####
  
  
  ####################################################
  #### STEP 6: PROCESS OBSERVATIONS DATA FOR ENKF
  ####################################################
  
  obs_methods_temp <- cbind(obs_config$method_1,obs_config$method_2,obs_config$method_3,obs_config$method_4)
  obs_methods <- list()
  for(i in 1:nrow(obs_methods_temp)){
    
    values <- obs_methods_temp[i,which(!is.na(obs_methods_temp[i,]))]
    if(length(values) == 0){
      values <- NA
    }
    obs_methods[[i]] <- values
  }
  obs_config$obs_methods <- obs_methods
  
  
  obs_list <- list()
  for(i in 1:length(obs_config$state_names_obs)){
    print(paste0("Extracting ",obs_config$target_variable[i]))
    obs_list[[i]] <- extract_observations(fname = cleaned_observations_file_long,
                                          full_time_local,
                                          modeled_depths = modeled_depths,
                                          local_tzone,
                                          target_variable = obs_config$target_variable[i],
                                          time_threshold_seconds = obs_config$time_threshold[i],
                                          distance_threshold_meter = obs_config$distance_threshold[i],
                                          methods = obs_config$obs_methods[[i]])
  }
  
  ####################################################
  #### STEP 7: CREATE THE Z ARRAY (OBSERVATIONS x TIME)
  ####################################################
  
  z <- array(NA, dim = c(nsteps, ndepths_modeled, length(obs_config$state_names_obs)))
  for(i in 1:nrow(obs_config)){
    z[ , , i] <-  obs_list[[i]]
  }
  
  z_obs <- z
  if(!use_obs_constraint){
    z[, , ] <- NA
  }
  
  ####################################################
  #### STEP 8: SET UP INITIAL CONDITIONS
  ####################################################
  
  init_depth <- list()
  for(i in 1:nrow(states_config)){
    if(!is.na(states_config$init_obs_name[i])){
      obs_index <- which(obs_config$state_names_obs == states_config$init_obs_name[i])
      #init_obs <- z[1, ,obs_index] * (1/states_config$states_to_obs_mapping[[i]][1]) * states_config$init_obs_mapping[i]
      init_obs <- z[1, ,obs_index] * (1/states_config$states_to_obs_mapping_1[i]) * states_config$init_obs_mapping[i]
      if(length(which(!is.na(init_obs))) == 0){
        init_depth[[i]] <- rep(states_config$initial_conditions[i], ndepths_modeled)
        if(states_config$init_obs_name[i] == "temp"){
          init_obs <- approx(x = default_temp_init_depths, y = default_temp_init, xout = modeled_depths, rule=2)
        }
      }else if(length(which(!is.na(init_obs))) == 1){
        init_depth[[i]] <- rep(init_obs[!is.na(init_obs)], ndepths_modeled)
      }else{
      init_depth[[i]] <- approx(x = modeled_depths[!is.na(init_obs)], y = init_obs[!is.na(init_obs)], xout = modeled_depths, rule=2)$y
      }
    }else{
      init_depth[[i]] <- rep(states_config$initial_conditions[i], ndepths_modeled)
    }
  }
  
  wq_start <- NA
  wq_end <- NA
  if(include_wq){
    temp_start <- 1
    temp_end <- ndepths_modeled
    wq_start <- rep(NA, num_wq_vars)
    wq_end <- rep(NA, num_wq_vars)
    for(wq in 1:num_wq_vars){
      if(wq == 1){
        wq_start[wq] <- temp_end+1
        wq_end[wq] <- temp_end + (ndepths_modeled)
      }else{
        wq_start[wq] <- wq_end[wq-1]+1
        wq_end[wq] <- wq_end[wq-1] + (ndepths_modeled)
      }
    }
  }else{
    temp_start <- 1
    temp_end <- ndepths_modeled
    wq_start <- temp_end+1
    wq_end <- temp_end+1
  }
  

  #######################################################
  #### STEP 9: CREATE THE PSI VECTOR (DATA uncertainty)  
  #######################################################
  
  psi_slope <- rep(NA, length(obs_config$state_names_obs) * ndepths_modeled)
  psi_intercept <- rep(NA, length(obs_config$state_names_obs) * ndepths_modeled)
  
  index <- 0
  for(i in 1:length(obs_config$state_names_obs)){
    for(j in 1:ndepths_modeled){
      index <- index + 1
      psi_intercept[index] <- obs_config$obs_error_intercept[[i]]
      psi_slope[index] <- obs_config$obs_error_slope[[i]]
    }
  }
  
  states_to_obs_temp <- cbind(states_config$states_to_obs_1,states_config$states_to_obs_2, states_config$states_to_obs_3)
  states_to_obs_mapping_temp <- cbind(states_config$states_to_obs_mapping_1,states_config$states_to_obs_mapping_2, states_config$states_to_obs_mapping_3)

  states_to_obs <- list()
  states_to_obs_mapping <- list()
  for(i in 1:nrow(states_to_obs_temp)){

    names_temp <- states_to_obs_temp[i,which(!is.na(states_to_obs_temp[i,]))]
    mapping_temp <- states_to_obs_mapping_temp[i,which(!is.na(states_to_obs_mapping_temp[i,]))]
    if(length(names_temp) == 0){
      values1 <- NA
      values2 <- NA
    }else{
      values1 <- rep(NA,length(names_temp))
      for(j in 1:length(names_temp)){
        values1[j] <- which(obs_config$state_names_obs == names_temp[j])
      }
      values2 <- c(mapping_temp)
    }
    states_to_obs[[i]] <- values1
    states_to_obs_mapping[[i]] <- values2
  }
  
  states_config$states_to_obs <- states_to_obs
  states_config$states_to_obs_mapping <- states_to_obs_mapping

  ####################################################
  #### STEP 10: CREATE THE PROCESS UNCERTAINTY
  ####################################################
  
  combined_error <- states_config$process_error
  combined_init_error <- states_config$initial_error
  
  #OLD STUFF
  qt <- matrix(data = 0, nrow = ndepths_modeled, ncol = ndepths_modeled)
  for(i in 1:num_wq_vars){
    for(j in 1:ndepths_modeled){
      qt <- rbind(qt, rep(0.0, ncol(qt)))
      qt <- cbind(qt, rep(0.0, nrow(qt)))
      qt[ncol(qt),nrow(qt)] <- states_config$process_error[i+1]
    }
  }
  running_residuals <- array(NA, dim =c(30, nrow(qt)))
  qt_pars <- NA
  
  ################################################################
  #### STEP 11: CREATE THE X ARRAY (STATES X TIME);INCLUDES INITIALATION
  ################################################################
  nmembers <- ensemble_size
  
  x <- array(NA, dim=c(nsteps, nmembers, nstates + npars))
  
  #Matrix to store essemble specific surface height
  surface_height <- array(NA, dim=c(nsteps, nmembers))
  snow_ice_thickness <- array(NA, dim=c(nsteps, nmembers, 3))
  avg_surf_temp <- array(NA, dim=c(nsteps, nmembers))
  mixing_vars <- array(NA, dim=c(nmembers, 17))
  glm_depths <- array(NA, dim = c(nsteps, nmembers, 500))
  
  restart_present <- FALSE
  if(!is.na(restart_file)){
    if(file.exists(restart_file)){
      restart_present <- TRUE
    }
  }
  
  #Initial conditions
  if(!restart_present){
    
    alpha_v <- 1 - exp(-vert_decorr_length)
    
    q_v <- rep(NA,ndepths_modeled)
    w <- rep(NA,ndepths_modeled)
    
    combined_initial_conditions <- unlist(init_depth)
    
    
    for(m in 1:nmembers){
      q_v[] <- NA
      w[] <- NA
      for(jj in 1:length(combined_error)){
        w[] <- rnorm(ndepths_modeled, 0, 1)
        q_v[1] <- combined_error[jj] * w[1]
        for(kk in 2:ndepths_modeled){
          q_v[kk] <- alpha_v * q_v[kk-1] + sqrt(1 - alpha_v^2) * combined_init_error[jj] * w[kk]
        }
        
        if(single_run | (initial_condition_uncertainty == FALSE & hist_days == 0)){
          x[1,m,(((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] <- 
            combined_initial_conditions[(((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)]
        }else{
          x[1,m,(((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] <- 
            combined_initial_conditions[(((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] + q_v
        }
      }
    }
    
    for(par in 1:npars){
      x[1, ,(nstates+par)] <- runif(n=nmembers,pars_config$par_init_lowerbound[par], pars_config$par_init_upperbound[par])
      if(single_run){
        x[1, ,(nstates+par)] <-  rep(pars_config$par_init[par], nmembers)
      }
    }
    
    if(include_wq){
      for(m in 1:nmembers){
        index <- which(x[1, m, 1:wq_end[num_wq_vars]] < 0.0)
        index <- index[which(index > wq_start[1])]
        x[1, m, index] <- 0.0
      }
    }
  }
  
  #THIS ALLOWS THE EnKF TO BE RESTARTED FROM YESTERDAY"S RUN
  if(restart_present){
    print("Using restart file")
    nc <- nc_open(restart_file)
    restart_nmembers <- length(ncvar_get(nc, "ens"))
    surface_height_restart <- ncvar_get(nc, "surface_height_restart")
    snow_ice_thickness_restart <- ncvar_get(nc, "snow_ice_restart")
    avg_surf_temp_restart <- ncvar_get(nc, "avg_surf_temp_restart")
    mixing_restart <- ncvar_get(nc, "mixing_restart")
    glm_depths_restart  <- ncvar_get(nc, "depths_restart")
    
    if(restart_nmembers > nmembers){
      #sample restart_nmembers
      sampled_nmembers <- sample(seq(1, restart_nmembers, 1),
                                 nmembers,
                                 replace=FALSE)
      restart_x_previous <- ncvar_get(nc, "x_restart")
      x_previous <- restart_x_previous[sampled_nmembers, ]
      snow_ice_thickness[1, , 1] <- snow_ice_thickness_restart[sampled_nmembers, 1]
      snow_ice_thickness[1, , 2] <- snow_ice_thickness_restart[sampled_nmembers, 2]
      snow_ice_thickness[1, , 3] <- snow_ice_thickness_restart[sampled_nmembers, 3]
      surface_height[1, ] <- surface_height_restart[sampled_nmembers]
      avg_surf_temp[1, ] <- avg_surf_temp_restart[sampled_nmembers]
      mixing_vars <- mixing_restart[sampled_nmembers, ]
      
      for(m in 1:nmembers){
        glm_depths[1,m ,1:dim(glm_depths_restart)[2]] <- glm_depths_restart[sampled_nmembers[m], ]
      }
      
    }else if(restart_nmembers < nmembers){
      sampled_nmembers <- sample(seq(1, restart_nmembers, 1),
                                 nmembers,
                                 replace = TRUE)
      restart_x_previous <- ncvar_get(nc, "x_restart")
      x_previous <- restart_x_previous[sampled_nmembers, ]
      snow_ice_thickness[1, ,1] <- snow_ice_thickness_restart[sampled_nmembers, 1]
      snow_ice_thickness[1, ,2] <- snow_ice_thickness_restart[sampled_nmembers, 2]
      snow_ice_thickness[1, ,3] <- snow_ice_thickness_restart[sampled_nmembers, 3]
      surface_height[1, ] <- surface_height_restart[sampled_nmembers]
      avg_surf_temp[1, ] <- avg_surf_temp_restart[sampled_nmembers]
      mixing_vars <- mixing_restart[sampled_nmembers, ]
      
      for(m in 1:nmembers){
        glm_depths[1,m ,1:dim(glm_depths_restart)[2]] <- glm_depths_restart[sampled_nmembers[m], ]
      }
      
    }else{
      
      restart_x_previous <- ncvar_get(nc, "x_restart")
      x_previous <- restart_x_previous
      snow_ice_thickness[1, ,1] <- snow_ice_thickness_restart[, 1]
      snow_ice_thickness[1, ,2] <- snow_ice_thickness_restart[, 2]
      snow_ice_thickness[1, ,3] <- snow_ice_thickness_restart[, 3]
      surface_height[1, ] <- surface_height_restart
      avg_surf_temp[1, ] <- avg_surf_temp_restart
      mixing_vars <- mixing_restart
      
      for(m in 1:nmembers){
        glm_depths[1,m ,1:dim(glm_depths_restart)[2]] <- glm_depths_restart[m, ]
      }
    }
    nc_close(nc)
  }else{
    
    x_previous <- x[1, , ]
    surface_height[1, ] <- round(lake_depth_init, 3)
    #Matrix to store snow and ice heights
    snow_ice_thickness[1, ,1] <- default_snow_thickness_init
    snow_ice_thickness[1, ,2] <- default_white_ice_thickness_init
    snow_ice_thickness[1, ,3] <- default_blue_ice_thickness_init
    avg_surf_temp[1, ] <- x[1, ,1]
    mixing_vars[,] <- 0.0
    
    for(m in 1:nmembers){
      glm_depths[1, m, 1:ndepths_modeled] <- modeled_depths
    }
    
  }
  
  
  
  #Set initial conditions
  x[1, , ] <- as.matrix(x_previous)
  
  #If hist_days = 0 then the first day of the simulation will be a forecast 
  #therefre the the initial_condition_uncertainty and parameter_uncertainty 
  #need to be dealt with in the x[1, ,] , normally it is dealt with in the 
  #run_EnKF script
  if(hist_days == 0){
    if(initial_condition_uncertainty == FALSE){
      states_mean <- colMeans(x[1, ,1:nstates])
      for(m in 1:nmembers){
        x[1, m, 1:nstates]  <- states_mean
      }
    }
    if(parameter_uncertainty == FALSE){
      mean_pars <- colMeans(x[1, ,(nstates + 1):(nstates + npars)])
      for(m in 1:nmembers){
        x[1, m, (nstates + 1):(nstates +  npars)] <- mean_pars
      }
    }
  }
  
  management_input <- read_sss_files(full_time_local, 
                                     sss_file = sss_fname)
  
  if(length(diagnostics_names) > 0){
    diagnostics <- array(NA, dim=c(nsteps, nmembers, ndepths_modeled, length(diagnostics_names)))
  }else{
    diagnostics <- NA
  }
  


  ####################################################
  #### STEP 12: Run Ensemble Kalman Filter
  ####################################################
  
  enkf_output <- run_EnKF(x,
                          z,
                          qt,
                          qt_pars,
                          psi_slope,
                          psi_intercept,
                          full_time_local,
                          working_directory,
                          pars_config,
                          modeled_depths,
                          surface_height,
                          wq_start,
                          wq_end,
                          met_file_names,
                          include_wq,
                          spin_up_days,
                          glm_output_vars,
                          process_uncertainty,
                          initial_condition_uncertainty,
                          parameter_uncertainty,
                          machine,
                          hist_days,
                          inflow_file_names,
                          outflow_file_names,
                          management_input,
                          forecast_sss_on,
                          snow_ice_thickness,
                          avg_surf_temp,
                          running_residuals,
                          the_sals_init,
                          mixing_vars,
                          glm_depths,
                          diagnostics_names,
                          diagnostics,
                          combined_error,
                          code_folder, 
                          base_GLM_nml, 
                          base_AED_nml,
                          base_AED_phyto_pars_nml,
                          base_AED_zoop_pars_nml,
                          obs_config,
                          states_config
  )
  
  x <- enkf_output$x
  x_restart <- enkf_output$x_restart
  qt_restart <- enkf_output$qt_restart
  x_prior <- enkf_output$x_prior
  surface_height_restart <- enkf_output$surface_height_restart
  snow_ice_restart <- enkf_output$snow_ice_restart
  snow_ice_thickness <- enkf_output$snow_ice_thickness
  surface_height <- enkf_output$surface_height
  data_assimilation_flag <- enkf_output$data_assimilation_flag
  
  running_residuals <- enkf_output$running_residuals
  
  avg_surf_temp_restart <- enkf_output$avg_surf_temp_restart
  mixing_restart <- enkf_output$mixing_restart
  glm_depths_restart <- enkf_output$glm_depths_restart
  
  diagnostics <- enkf_output$diagnostics
  
  ####################################################
  #### STEP 13: PROCESS OUTPUT
  ####################################################
  
  ### CREATE FORECAST NAME
  
  if(day(full_time_local[1]) < 10){
    file_name_H_day <- paste0("0",day(full_time_local[1]))
  }else{
    file_name_H_day <- day(full_time_local[1]) 
  }
  if(day(full_time_local[hist_days+1]) < 10){
    file_name_F_day <- paste0("0",day(full_time_local[hist_days+1]))
  }else{
    file_name_F_day <- day(full_time_local[hist_days+1]) 
  }
  if(month(full_time_local[1]) < 10){
    file_name_H_month <- paste0("0",month(full_time_local[1]))
  }else{
    file_name_H_month <- month(full_time_local[1]) 
  }
  if(month(full_time_local[hist_days+1]) < 10){
    file_name_F_month <- paste0("0",month(full_time_local[hist_days+1]))
  }else{
    file_name_F_month <- month(full_time_local[hist_days+1]) 
  }
  
  save_file_name <- paste0(sim_name, "_H_",
                           (year(full_time_local[1])),"_",
                           file_name_H_month,"_",
                           file_name_H_day,"_",
                           (year(full_time_local[hist_days+1])),"_",
                           file_name_F_month,"_",
                           file_name_F_day,"_F_",
                           forecast_days) 
  
  time_of_forecast <- Sys.time()
  curr_day <- day(time_of_forecast)
  curr_month <- month(time_of_forecast)
  curr_year <- year(time_of_forecast)
  curr_hour <- hour(time_of_forecast)
  curr_minute <- minute(time_of_forecast)
  curr_second <- round(second(time_of_forecast),0)
  if(curr_day < 10){curr_day <- paste0("0",curr_day)}
  if(curr_month < 10){curr_month <- paste0("0",curr_month)}
  if(curr_hour < 10){curr_hour <- paste0("0",curr_hour)}
  if(curr_minute < 10){curr_minute <- paste0("0",curr_minute)}
  if(curr_second < 10){curr_second <- paste0("0",curr_second)}

  forecast_iteration_id <- paste0(curr_year,
                                  curr_month,
                                  curr_day,
                                  "T",
                                  curr_hour,
                                  curr_minute,
                                  curr_second)

  
  
  ###SAVE FORECAST
  write_forecast_netcdf(x,
                        full_time_local,
                        qt,
                        modeled_depths,
                        save_file_name,
                        x_restart,
                        qt_restart,
                        forecast_iteration_id,
                        forecast_project_id,
                        hist_days,
                        x_prior,
                        include_wq,
                        wq_start,
                        wq_end,
                        z,
                        nstates,
                        pars_config,
                        GLMversion,
                        FLAREversion,
                        local_tzone,
                        surface_height_restart,
                        snow_ice_restart,
                        snow_ice_thickness,
                        surface_height,
                        avg_surf_temp_restart,
                        running_residuals,
                        mixing_restart,
                        glm_depths_restart,
                        forecast_location,
                        state_names = states_config$state_names,
                        diagnostics_names,
                        diagnostics,
                        data_assimilation_flag,
                        time_of_forecast
  )
  
  #### END GLM ENKF CONTAINER ####
  
  
  #### START ARCHIVE CONTAINER
  
  restart_file_name <- archive_forecast(working_directory = working_directory,
                                        folder = code_folder, 
                                        forecast_base_name = forecast_base_name, 
                                        forecast_location = forecast_location,
                                        push_to_git = push_to_git,
                                        save_file_name = save_file_name, 
                                        time_of_forecast_string = forecast_iteration_id)

  if(generate_eml){
    create_flare_eml(file_name = restart_file_name[[1]],
                     time_of_forecast,
                     forecast_iteration_id, 
                     forecast_project_id,
                     nstates, 
                     npars, 
                     n_met_members, 
                     n_ds_members,
                     nmembers,
                     process_uncertainty,
                     weather_uncertainty,
                     initial_condition_uncertainty,
                     parameter_uncertainty,
                     met_downscale_uncertainty,
                     inflow_process_uncertainty)
  }
  #### END START ARCHIVE CONTAINER
  
  return(list(restart_file_name <- restart_file_name,
              sim_name <- paste0(save_file_name, "_", forecast_iteration_id)))
}
