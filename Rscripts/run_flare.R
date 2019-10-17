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
                    code_folder, 
                    forecast_location = NA,
                    push_to_git = FALSE,
                    pull_from_git = TRUE, 
                    data_location = NA, 
                    n_enkf_members = NA,
                    n_ds_members = 50,
                    include_wq = FALSE,
                    use_ctd = use_ctd,
                    uncert_mode = 1,
                    #reference_tzone,
                    cov_matrix = NA,
                    downscaling_coeff = NA,
                    GLMversion,
                    DOWNSCALE_MET = TRUE,
                    FLAREversion,
                    met_ds_obs_start,
                    met_ds_obs_end,
                    modeled_depths,
                    forecast_sss_on){
  
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
  
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/create_inflow_outflow_file.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/create_obs_met_input.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/create_sss_input_output.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/extract_chla_chain.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/extract_CTD.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/extract_do_chain.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/extract_fdom_chain.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/extract_temp_chain.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/read_sss_files.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/extract_nutrients.R"))
  
  
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
    spin_up_days <- hist_days + 2
    n_enkf_members <- 3
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
  
  #################################################
  ### STEP 1: GRAB DATA FROM REPO OR SERVER
  #################################################
  
  
  temperature_location <- paste0(data_location, "/", "mia-data") #FCR SPECIFIC
  met_station_location <- paste0(data_location, "/", "carina-data") #FCR SPECIFIC
  noaa_location <- paste0(data_location, "/", "noaa-data") #FCR SPECIFIC
  manual_data_location <- paste0(data_location, "/", "manual-data") #FCR SPECIFIC
  if(pull_from_git){
    
    if(!file.exists(temperature_location)){
      setwd(data_location)
      system("git clone -b mia-data --single-branch https://github.com/CareyLabVT/SCCData.git mia-data")
    }
    if(!file.exists(met_station_location)){
      setwd(data_location)
      system("git clone -b carina-data --single-branch https://github.com/CareyLabVT/SCCData.git carina-data")
    }
    if(!file.exists(noaa_location)){
      setwd(data_location)
      system("git clone -b noaa-data --single-branch https://github.com/CareyLabVT/SCCData.git noaa-data")
    }
    
    if(!file.exists(manual_data_location)){
      setwd(data_location)
      system("git clone -b manual-data --single-branch https://github.com/CareyLabVT/SCCData.git manual-data")
    }
    
    setwd(temperature_location)
    system(paste0("git pull"))
    
    setwd(met_station_location)
    system(paste0("git pull"))
    
    setwd(noaa_location)
    system(paste0("git pull"))
    
    setwd(manual_data_location)
    system(paste0("git pull"))
    
    if(length(met_obs_fname) > 1 & !file.exists(met_obs_fname[2])){
      inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/389/2/63808664f8feda6a703ef42601207d2e" 
      download.file(inUrl1,met_obs_fname[2],method="curl")
    }
    
    if(length(temp_obs_fname) > 1 & !file.exists(temp_obs_fname[2])){
      inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/2/ea8d89aa2804fb4657acb5b2dbdcc944" 
      download.file(inUrl1,temp_obs_fname[2],method="curl")
      
    }
    
    if(!is.na(ctd_fname) & !file.exists(ctd_fname)){
      inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/6/2143055cdfad2e5bd99c2b0b2670cc56" 
      download.file(inUrl1,ctd_fname,method="curl")
    }
    
    if(!is.na(nutrients_fname) & !file.exists(nutrients_fname)){
      inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/5/2b3dc84ae6b12d10bd5485f1c300af13" 
      download.file(inUrl1,nutrients_fname,method="curl")
    }
    
  }
  
  ####################################################
  #### STEP 2: CREATE TIME VECTORS
  ####################################################
  
  total_days <- hist_days + forecast_days
  start_forecast_step <- hist_days + 1
  
  start_datetime_local <- as_datetime(paste0(start_day_local," ",start_time_local), tz = local_tzone)
  end_datetime_local <- start_datetime_local + total_days*24*60*60
  forecast_start_time_local <- start_datetime_local + hist_days*24*60*60
  
  
  start_datetime_GMT <- with_tz(start_datetime_local, tzone = "GMT")
  end_datetime_GMT <- with_tz(end_datetime_local, tzone = "GMT")
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
  
  full_time_GMT <- seq(start_datetime_GMT, end_datetime_GMT, by = "1 day")
  full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")
  full_time_GMT <- strftime(full_time_GMT, 
                            format="%Y-%m-%d %H:%M",
                            tz = "GMT")
  full_time_local <- strftime(full_time_local,
                              format="%Y-%m-%d %H:%M",
                              tz = local_tzone)
  full_time_day_GMT <- strftime(full_time_GMT,
                                format="%Y-%m-%d",
                                tz = "GMT")
  full_time_day_local <- strftime(full_time_local,
                                  format="%Y-%m-%d",
                                  tz = local_tzone)
  full_time_hour_local <- seq(as.POSIXct(full_time_local[1],
                                         tz = local_tzone), 
                              as.POSIXct(full_time_local[length(full_time_local)],
                                         tz = local_tzone),
                              by = "1 hour")
  
  ####################################################
  #### STEP 3: SET ARRAY DIMENSIONS
  ####################################################
  
  nsteps <- length(full_time_local)
  if(spin_up_days > nsteps){
    spin_up_days <- nsteps
  }
  
  ndepths_modeled <- length(modeled_depths)
  num_wq_vars <- length(wq_names)
  if(include_wq){
    glm_output_vars <- c("temp", wq_names)
  }else{
    glm_output_vars <- "temp"
  }
  npars <- length(par_names)
  
  # SET UP NUMBER OF ENSEMBLE MEMBERS
  n_met_members <- 21
  if(single_run){
    n_met_members <- 3
    n_ds_members <- 1
  }
  
  if(include_wq){
    nstates <- ndepths_modeled*(1+num_wq_vars)
  }else{
    nstates <- ndepths_modeled
  }
  
  ###CREATE HISTORICAL MET FILE
  if(met_downscale_uncertainty == FALSE){
    n_enkf_members <- n_enkf_members * n_ds_members
    n_ds_members <- 1
  }
  
  ####################################################
  #### STEP 4: ORGANIZE FILES
  ####################################################
  
  ###CREATE DIRECTORY PATHS AND STRUCTURE
  working_directory <- paste0(forecast_location, "/", "working_directory")
  if(!dir.exists(working_directory)){
    dir.create(working_directory, showWarnings = FALSE)
  }
  ####Clear out temp GLM working directory
  unlink(paste0(working_directory, "/*"), recursive = FALSE)   
  
  forecast_base_name <- paste0(year(forecast_start_time_GMT),
                               forecast_month_GMT,
                               forecast_day_GMT,
                               "gep_all_",
                               noaa_hour,
                               "z")
  
  forecast_base_name_past <- paste0(year(forecast_start_time_GMT_past),
                                    forecast_month_GMT_past,
                                    forecast_day_GMT_past,
                                    "gep_all_",
                                    noaa_hour,
                                    "z")
  
  met_obs_fname_wdir <- met_obs_fname
  
  met_forecast_base_file_name <- paste0("met_hourly_",
                                        forecast_base_name,
                                        "_ens")
  if(is.na(sim_name)){
    sim_name <- paste0(year(full_time_local[1]), "_",
                       month(full_time_local[1]), "_",
                       day(full_time_local[1]))
  }
  
  sim_files_folder <- paste0(code_folder, "/", "sim_files")
  fl <- c(list.files(sim_files_folder, full.names = TRUE))
  tmp <- file.copy(from = fl, to = working_directory, overwrite = TRUE)
  
  if(!is.na(restart_file)){
    tmp <- file.copy(from = restart_file, to = working_directory, overwrite = TRUE)
  }
  
  
  
  ####################################################
  #### STEP 5: PROCESS AND ORGANIZE DRIVER DATA
  ####################################################
  
  met_file_names <- rep(NA, 1+(n_met_members*n_ds_members))
  obs_met_outfile <- "GLM_met.csv"
  missing_met <- create_obs_met_input(fname = met_obs_fname_wdir,
                                      outfile=obs_met_outfile,
                                      full_time_hour_local, 
                                      input_file_tz = "EST5EDT",
                                      local_tzone,
                                      working_directory)
  if(missing_met < missing_met_data_threshold){
    met_file_names[1] <- obs_met_outfile
  }else{
    if(hist_days > 1){
      stop(paste0("Running more than 1 hist_day but met data has ",missing_met," values"))
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
                                              "compare_totals"),
                          "use_covariance" = c(TRUE,
                                               TRUE,
                                               TRUE,
                                               TRUE,
                                               TRUE,
                                               FALSE),
                          stringsAsFactors = FALSE)
    
    replaceObsNames <- c("AirTC_Avg" = "AirTemp",
                         "WS_ms_Avg" = "WindSpeed",
                         "RH" = "RelHum",
                         "SR01Up_Avg" = "ShortWave",
                         "IR01UpCo_Avg" = "LongWave",
                         "Rain_mm_Tot" = "Rain")
    
    temp_met_file<- process_downscale_GEFS(folder = code_folder,
                                           noaa_location,
                                           input_met_file = met_obs_fname_wdir[1],
                                           working_directory,
                                           sim_files_folder = paste0(code_folder, "/", "sim_files"),
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
                                           input_met_file_tz = "EST5EDT")
    
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
                                              "compare_totals"),
                          "use_covariance" = c(TRUE,
                                               TRUE,
                                               TRUE,
                                               TRUE,
                                               TRUE,
                                               FALSE),
                          stringsAsFactors = FALSE)
    
    replaceObsNames <- c("AirTC_Avg" = "AirTemp",
                         "WS_ms_Avg" = "WindSpeed",
                         "RH" = "RelHum",
                         "SR01Up_Avg" = "ShortWave",
                         "IR01UpCo_Avg" = "LongWave",
                         "Rain_mm_Tot" = "Rain")
    
    met_file_names[2:(1+(n_met_members*n_ds_members))] <- process_downscale_GEFS(folder = code_folder,
                                                                                 noaa_location,
                                                                                 input_met_file = met_obs_fname_wdir[1],
                                                                                 working_directory,
                                                                                 sim_files_folder = paste0(code_folder, "/", "sim_files"),
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
                                                                                 input_met_file_tz = "EST5EDT")
    
    if(weather_uncertainty == FALSE & met_downscale_uncertainty == TRUE){
      met_file_names <- met_file_names[1:(1+(1*n_ds_members))]
    }else if(weather_uncertainty == FALSE & met_downscale_uncertainty == FALSE){
      met_file_names <- met_file_names[1:2]
    }
  }
  
  if(weather_uncertainty == FALSE){
    n_enkf_members <- n_enkf_members * n_met_members
    n_met_members <- 1
  }
  
  ##CREATE INFLOW AND OUTFILE FILES
  inflow_outflow_files <- create_inflow_outflow_file(full_time_day_local,
                                                     working_directory, 
                                                     input_file_tz = "EST5EDT",
                                                     start_forecast_step,
                                                     hold_inflow_outflow_constant,
                                                     inflow_file1,
                                                     inflow_file2,
                                                     outflow_file1,
                                                     local_tzone)
  
  inflow_file_names <- cbind(inflow1 = inflow_outflow_files$inflow_file_names,
                             inflow2 = inflow_outflow_files$wetland_file_names)
  outflow_file_names <- cbind(inflow_outflow_files$spillway_file_names)
  
  management_input <- read_sss_files(full_time_day_local,
                                     working_directory,
                                     input_file_tz = 'EST5EDT', 
                                     sss_file = sss_fname,
                                     local_tzone)
  
  
  ####################################################
  #### STEP 6: PROCESS AND ORGANIZE STATE DATA
  ####################################################
  
  #Inputs: temperature_location, temp_obs_fname, full_time_local, modeled_depths,
  # observed_depths_temp, local_tzone, observed_depths_do, exo_2_ctd_chla, use_ctd
  
  #Extract observations, 
  temp_obs_fname_wdir <- temp_obs_fname
  #PROCESS TEMPERATURE OBSERVATIONS
  obs_temp <- extract_temp_chain(fname = temp_obs_fname_wdir,
                                 full_time_local,
                                 modeled_depths = modeled_depths,
                                 observed_depths_temp = observed_depths_temp,
                                 input_file_tz = "EST5EDT",
                                 local_tzone)
  
  if(include_wq){
    #PROCESS DO OBSERVATIONS
    obs_do <- extract_do_chain(fname = temp_obs_fname_wdir,
                               full_time_local,
                               modeled_depths = modeled_depths,
                               observed_depths_do= observed_depths_do,
                               input_file_tz = "EST5EDT", 
                               local_tzone)
    
    obs_chla <- extract_chla_chain(fname = temp_obs_fname_wdir,
                                   full_time_local,
                                   modeled_depths = modeled_depths,
                                   observed_depths_chla_fdom,
                                   input_file_tz = "EST5EDT", 
                                   local_tzone)
    
    obs_fdom <- extract_do_chain(fname = temp_obs_fname_wdir,
                                 full_time_local,
                                 modeled_depths = modeled_depths,
                                 observed_depths_chla_fdom,
                                 input_file_tz = "EST5EDT", 
                                 local_tzone)
    
    #obs_fdom$obs[, ] <- NA
    
    if(use_nutrient_data){
      obs_nutrients <- extract_nutrients(fname = nutrients_fname,
                                         full_time_day_local,
                                         modeled_depths = modeled_depths,
                                         input_file_tz = "EST5EDT", 
                                         local_tzone)

    #Combine fdom and nutrients
    for(i in 1:length(full_time_day_local)){
      if(length(which(!is.na(obs_nutrients$DOC[i,]))) > 0){
        obs_fdom$obs[i,which(is.na(obs_fdom$obs[i,]))] <- obs_nutrients$DOC[i,which(is.na(obs_fdom$obs[i,]))]
      }
    }
  }else{
    obs_nutrients <- NULL
    obs_nutrients$NH4 <- array(NA, dim = dim(obs_fdom$obs))
    obs_nutrients$NO3 <- array(NA, dim = dim(obs_fdom$obs))
    obs_nutrients$SRP <- array(NA, dim = dim(obs_fdom$obs))
  }
}

#Use the CTD observation rather than the sensor string when CTD data is avialable
if(use_ctd){
  
  #NEED TO DOUBLE CHECK TIME ZONE
  obs_ctd <- extract_CTD(fname = ctd_fname,
                         full_time_day_local,
                         modeled_depths = modeled_depths,
                         input_file_tz = "EST5EDT",
                         local_tzone)
  
  #Merge CTD with other sensor data
  for(i in 1:length(full_time_day_local)){
    if(!is.na(obs_ctd$obs_temp[i, 1])){
      obs_temp$obs[i,which(is.na(obs_temp$obs[i,]))] <- obs_ctd$obs_temp[i,which(is.na(obs_temp$obs[i,]))]
      if(include_wq){
        obs_do$obs[i,which(is.na(obs_do$obs[i,]))] <- obs_ctd$obs_do[i,which(is.na(obs_do$obs[i,])) ]
        obs_chla$obs[i,which(is.na(obs_chla$obs[i,]))] <- obs_ctd$obs_chla[i, which(is.na(obs_chla$obs[i,])) ]
      }
    }
  }
}

####################################################
#### STEP 7: CREATE THE Z ARRAY (OBSERVATIONS x TIME)
####################################################

#Observations for each observed state at each time step
#an observation with at least 1 observation but without an 
#observation in a time-step gets assigned an NA

if(include_wq){
  obs_dims <- dim(obs_do$obs)
  
  OXY_oxy_obs <- obs_do$obs
  CAR_pH_obs <- array(NA, dim = obs_dims)
  CAR_dic_obs <- array(NA, dim = obs_dims)
  CAR_ch4_obs <- array(NA, dim = obs_dims)
  SIL_rsi_obs <- array(NA, dim = obs_dims)
  NIT_amm_obs <- obs_nutrients$NH4
  NIT_nit_obs <- obs_nutrients$NO3
  PHS_frp_obs <- obs_nutrients$SRP
  OGM_doc_obs <- obs_fdom$obs
  OGM_poc_obs <- array(NA, dim = obs_dims)
  OGM_don_obs <- array(NA, dim = obs_dims)
  OGM_pon_obs <- array(NA, dim = obs_dims)
  OGM_dop_obs <- array(NA, dim = obs_dims)
  OGM_pop_obs <- array(NA, dim = obs_dims)
  NCS_ss1_obs <- array(NA, dim = obs_dims)
  PHS_frp_ads_obs <- array(NA, dim = obs_dims)
  PHY_AGGREGATE_obs <- obs_chla$obs
  
  z <- cbind(obs_temp$obs,
             OXY_oxy_obs,
             CAR_pH_obs,
             CAR_dic_obs,
             CAR_ch4_obs,
             SIL_rsi_obs,
             NIT_amm_obs,
             NIT_nit_obs,
             PHS_frp_obs,
             OGM_doc_obs,
             OGM_poc_obs,
             OGM_don_obs,
             OGM_pon_obs,
             OGM_dop_obs,
             OGM_pop_obs,
             NCS_ss1_obs,
             PHS_frp_ads_obs,
             PHY_AGGREGATE_obs)
}else{
  z <- cbind(obs_temp$obs) 
}

z_obs <- z
if(!use_obs_constraint){
  z[, ] <- NA
}

obs_index <- seq(1,nstates, 1)

#Matrix for knowing which state the observation corresponds to
z_states <- t(matrix(obs_index, nrow = length(obs_index), ncol = nsteps))

####################################################
#### STEP 8: SET UP INITIAL CONDITIONS
####################################################

init_temps_obs <- obs_temp$obs[1, which(!is.na(obs_temp$obs[1, ]))]
init_obs_temp_depths <- modeled_depths[which(!is.na(obs_temp$obs[1, ]))]

if(include_wq){
  init_do_obs <- obs_do$obs[1, which(!is.na(obs_do$obs[1, ]))]
  init_obs_do_depths <- modeled_depths[which(!is.na(obs_do$obs[1, ]))]
  
  init_chla_obs <- obs_chla$obs[1, which(!is.na(obs_chla$obs[1, ]))]
  init_chla_obs_depths <- modeled_depths[which(!is.na(obs_chla$obs[1, ]))]
  
  init_doc_obs <- obs_fdom$obs[1, which(!is.na(obs_fdom$obs[1, ]))]
  init_doc_obs_depths <- modeled_depths[which(!is.na(obs_fdom$obs[1, ]))]
  
  init_nit_amm_obs <- obs_nutrients$NH4[1, which(!is.na(obs_nutrients$NH4[1, ]))]
  init_nit_amm_obs_depths <- modeled_depths[which(!is.na(obs_nutrients$NH4[1, ]))]
  
  init_nit_nit_obs <- obs_nutrients$NO3[1, which(!is.na(obs_nutrients$NO3[1, ]))]
  init_nit_nit_obs_depths <- modeled_depths[which(!is.na(obs_nutrients$NO3[1, ]))]
  
  init_phs_frp_obs <- obs_nutrients$SRP[1, which(!is.na(obs_nutrients$SRP[1, ]))]
  init_phs_frp_obs_depths <- modeled_depths[which(!is.na(obs_nutrients$SRP[1, ]))]
}


#OGM_doc_init_depth <- NA
#PHY_TCHLA_init_depth <- NA
#OXY_oxy_init_depth <- NA

#NEED AN ERROR CHECK FOR WHETHER THERE ARE OBSERVED DATA
if(is.na(restart_file)){
  if((length(which(init_temps_obs != 0.0)) == 0) | length(which(is.na(init_temps_obs))) > 0){
    print("Using default temperature values to initialize")
    init_temps_obs <- default_temp_init
    init_obs_temp_depths <- default_temp_init_depths
  }
  temp_inter <- approxfun(init_obs_temp_depths, init_temps_obs, rule=2)
  the_temps_init <- temp_inter(modeled_depths)
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

if(include_wq & is.na(restart_file)){
  
  #Initialize Oxygen using data if avialable
  if(length(!is.na(init_do_obs)) == 0){
    OXY_oxy_init_depth <- rep(OXY_oxy_init, ndepths_modeled) 
  }else if(length(!is.na(init_do_obs)) == 1){
    OXY_oxy_init_depth <- rep(init_do_obs, ndepths_modeled)
  }else{
    do_inter <- approxfun(init_obs_do_depths, init_do_obs, rule=2)
    OXY_oxy_init_depth <- do_inter(modeled_depths)
  }
  
  #Initialize Chla usind data if avialable
  if(length(!is.na(init_chla_obs)) == 0){
    PHY_AGGREGATE_init_depth <- rep(PHY_AGGREGATE_init, ndepths_modeled) 
  }else if(length(!is.na(init_chla_obs)) == 1){
    PHY_AGGREGATE_init_depth <- rep(init_chla_obs, ndepths_modeled)
  }else{
    chla_inter <- approxfun(init_chla_obs_depths, init_chla_obs, rule=2)
    PHY_AGGREGATE_init_depth <- chla_inter(modeled_depths)
  }
  
  #Initialize DOC usind data if avialable
  if(length(!is.na(init_doc_obs)) == 0){
    OGM_doc_init_depth <- rep(OGM_doc_init, ndepths_modeled) 
  }else if(length(!is.na(init_doc_obs)) == 1){
    OGM_doc_init_depth <- rep(init_doc_obs, ndepths_modeled)
  }else{
    doc_inter <- approxfun(init_doc_obs_depths, init_doc_obs, rule=2)
    OGM_doc_init_depth <- doc_inter(modeled_depths)
  }
  
  #Initialize DOC usind data if avialable
  if(length(!is.na(init_nit_amm_obs)) == 0){
    NIT_amm_init_depth <- rep(NIT_amm_init, ndepths_modeled) 
  }else if(length(!is.na(init_nit_amm_obs)) == 1){
    NIT_amm_init_depth <- rep(init_nit_amm_obs, ndepths_modeled)
  }else{
    temp_inter <- approxfun(init_nit_amm_obs_depths, init_nit_amm_obs, rule=2)
    NIT_amm_init_depth <- temp_inter(modeled_depths)
  }
  
  #Initialize DOC usind data if avialable
  if(length(!is.na(init_nit_nit_obs)) == 0){
    NIT_nit_init_depth <- rep(NIT_nit_init, ndepths_modeled) 
  }else if(length(!is.na(init_nit_nit_obs)) == 1){
    NIT_nit_init_depth <- rep(init_nit_nit_obs, ndepths_modeled)
  }else{
    temp_inter <- approxfun(init_nit_nit_obs_depths, init_nit_nit_obs, rule=2)
    NIT_nit_init_depth <- temp_inter(modeled_depths)
  }
  
  #Initialize DOC usind data if avialable
  if(length(!is.na(init_phs_frp_obs)) == 0){
    PHS_frp_init_depth <- rep(PHS_frp_init, ndepths_modeled) 
  }else if(length(!is.na(init_phs_frp_obs)) == 1){
    PHS_frp_init_depth <- rep(init_phs_frp_obs, ndepths_modeled)
  }else{
    temp_inter <- approxfun(init_phs_frp_obs_depths, init_phs_frp_obs, rule=2)
    PHS_frp_init_depth <- temp_inter(modeled_depths)
  }
  
  
  #Initilize other water quality variables
  CAR_pH_init_depth  <- rep(CAR_pH_init, ndepths_modeled) 
  CAR_dic_init_depth <- rep(CAR_dic_init, ndepths_modeled)
  CAR_ch4_init_depth <- rep(CAR_ch4_init, ndepths_modeled)
  SIL_rsi_init_depth <- rep(SIL_rsi_init, ndepths_modeled)
  OGM_poc_init_depth <- rep(OGM_poc_init, ndepths_modeled)
  OGM_don_init_depth <- OGM_doc_init_depth * init_donc
  OGM_pon_init_depth <- rep(OGM_pon_init, ndepths_modeled)
  OGM_dop_init_depth <- OGM_doc_init_depth * init_dopc
  OGM_pop_init_depth <- rep(OGM_pop_init, ndepths_modeled)
  NCS_ss1_init_depth <- rep(NCS_ss1_init, ndepths_modeled)
  PHS_frp_ads_init_depth <- rep(PHS_frp_ads_init, ndepths_modeled)
  
  wq_init_vals <- c(OXY_oxy_init_depth,
                    CAR_pH_init_depth,
                    CAR_dic_init_depth,
                    CAR_ch4_init_depth,
                    SIL_rsi_init_depth,
                    NIT_amm_init_depth,
                    NIT_nit_init_depth,
                    PHS_frp_init_depth,
                    OGM_doc_init_depth,
                    OGM_poc_init_depth,
                    OGM_don_init_depth,
                    OGM_pon_init_depth,
                    OGM_dop_init_depth,
                    OGM_pop_init_depth,
                    NCS_ss1_init_depth,
                    PHS_frp_ads_init_depth,
                    PHY_AGGREGATE_init_depth)
  
  #UPDATE NML WITH INITIAL CONDITIONS
  
}else if(!include_wq & is.na(restart_file)){
  #UPDATE NML WITH INITIAL CONDITIONS
  wq_init_vals <- c(" ")
}


########################################
#BEGIN GLM SPECIFIC PART
########################################

GLM_folder <- paste0(code_folder, "/", "glm", "/", machine) 
fl <- c(list.files(GLM_folder, full.names = TRUE))
tmp <- file.copy(from = fl, to = working_directory, overwrite = TRUE)

file.copy(from = paste0(working_directory, "/", base_GLM_nml), 
          to = paste0(working_directory, "/", "glm3.nml"), overwrite = TRUE)

#update_var(wq_init_vals, "wq_init_vals", working_directory, "glm3.nml") #GLM SPECIFIC
if(include_wq){
  update_var(num_wq_vars, "num_wq_vars", working_directory, "glm3.nml") #GLM SPECIFIC
}else{
  update_var(0, "num_wq_vars", working_directory, "glm3.nml") #GLM SPECIFIC
}
update_var(ndepths_modeled, "num_depths", working_directory, "glm3.nml") #GLM SPECIFIC
update_var(modeled_depths, "the_depths", working_directory, "glm3.nml") #GLM SPECIFIC
update_var(rep(the_sals_init, ndepths_modeled), "the_sals", working_directory, "glm3.nml") #GLM SPECIFIC

#Create a copy of the NML to record starting initial conditions
file.copy(from = paste0(working_directory, "/", "glm3.nml"), #GLM SPECIFIC
          to = paste0(working_directory, "/", "glm3_initial.nml"), overwrite = TRUE) #GLM SPECIFIC

########################################
#END GLM SPECIFIC PART
########################################

if(include_wq){
  nobs <- length(observed_depths_temp) + 
    length(observed_depths_do) + 
    length(observed_depths_chla_fdom) + 
    length(observed_depths_chla_fdom) 
}else{
  nobs <- length(observed_depths_temp)
}

#######################################################
#### STEP 9: CREATE THE PSI VECTOR (DATA uncertainty)  
#######################################################
psi_slope <- rep(NA, length(obs_index))
psi_intercept <- rep(NA, length(obs_index))

psi_slope[1: ndepths_modeled] <- rep(obs_error_temperature_slope, ndepths_modeled)
psi_intercept[1: ndepths_modeled] <- rep(obs_error_temperature_intercept, ndepths_modeled)
if(include_wq){
  for(wq in 1:num_wq_vars){
    psi_intercept[wq_start[wq]:wq_end[wq]] <- rep(obs_error_wq_intercept[wq], ndepths_modeled)
    psi_slope[wq_start[wq]:wq_end[wq]] <- rep(obs_error_wq_slope[wq], ndepths_modeled)
  }
}

####################################################
#### STEP 10: CREATE THE QT ARRAY (MODEL VARIANCE)
####################################################

restart_present <- FALSE
if(!is.na(restart_file)){
  if(file.exists(restart_file)){
    restart_present <- TRUE
  }
}

if(restart_present){
  nc <- nc_open(restart_file)
  qt <- ncvar_get(nc, "qt_restart")
  qt_pars <- matrix(data = 0, nrow = npars, ncol = npars)
  diag(qt_pars) <- par_init_qt
  nc_close(nc)
  qt_init <- qt
}else{
  qt <- matrix(data = 0, nrow = ndepths_modeled, ncol = ndepths_modeled)
  diag(qt) <- rep(temp_process_error,ndepths_modeled )
  qt_init <- matrix(data = 0, nrow = ndepths_modeled, ncol = ndepths_modeled)
  diag(qt_init) <- rep(temp_init_error,ndepths_modeled )
  
  if(include_wq){
    wq_var_error <- c(OXY_oxy_process_error,
                      CAR_pH_process_error,
                      CAR_dic_process_error,
                      CAR_ch4_process_error,
                      SIL_rsi_process_error,
                      NIT_amm_process_error,
                      NIT_nit_process_error,
                      PHS_frp_process_error,
                      OGM_doc_process_error,
                      OGM_poc_process_error, 
                      OGM_don_process_error,
                      OGM_pon_process_error,
                      OGM_dop_process_error,
                      OGM_pop_process_error,
                      NCS_ss1_process_error,
                      PHS_frp_ads_process_error,
                      PHY_AGGREGATE_process_error)
    
    wq_var_init_error <- c(OXY_oxy_init_error,
                           CAR_pH_init_error,
                           CAR_dic_init_error,
                           CAR_ch4_init_error,
                           SIL_rsi_init_error,
                           NIT_amm_init_error,
                           NIT_nit_init_error,
                           PHS_frp_init_error,
                           OGM_doc_init_error,
                           OGM_poc_init_error, 
                           OGM_don_init_error,
                           OGM_pon_init_error,
                           OGM_dop_init_error,
                           OGM_pop_init_error,
                           NCS_ss1_init_error,
                           PHS_frp_ads_init_error,
                           PHY_AGGREGATE_init_error) 
    
    for(i in 1:num_wq_vars){
      for(j in 1:ndepths_modeled){
        qt <- rbind(qt, rep(0.0, ncol(qt)))
        qt <- cbind(qt, rep(0.0, nrow(qt)))
        qt[ncol(qt),nrow(qt)] <- wq_var_error[i]
        
        qt_init <- rbind(qt_init, rep(0.0, ncol(qt_init)))
        qt_init <- cbind(qt_init, rep(0.0, nrow(qt_init)))
        qt_init[ncol(qt_init),nrow(qt_init)] <- wq_var_init_error[i]
      }
    }
  }
  
  #Covariance matrix for parameters
  if(npars > 0){
    for(pars in 1:npars){
      qt <- rbind(qt, rep(0.0, ncol(qt)))
      qt <- cbind(qt, rep(0.0, nrow(qt)))
      qt[ncol(qt),nrow(qt)] <- par_init_qt[pars]
      
      qt_init <- rbind(qt_init, rep(0.0, ncol(qt_init)))
      qt_init <- cbind(qt_init, rep(0.0, nrow(qt)))
      qt_init[ncol(qt_init),nrow(qt_init)] <- par_init_qt[pars]
    }
    qt_pars <- matrix(data = 0, nrow = npars, ncol = npars)
    diag(qt_pars) <- par_init_qt
  }else{
    qt_pars <- NA
  }
}

################################################################
#### STEP 11: CREATE THE X ARRAY (STATES X TIME);INCLUDES INITIALATION
################################################################
nmembers <- n_enkf_members*n_met_members*n_ds_members

x <- array(NA, dim=c(nsteps, nmembers, nstates + npars))

#Initial conditions
if(!restart_present){
  if(include_wq){
    if(npars > 0){
      x[1, ,1:nstates] <- rmvnorm(n=nmembers, 
                                  mean=c(the_temps_init, 
                                         wq_init_vals), 
                                  sigma=as.matrix(qt_init[1:nstates,1:nstates]))
      if(single_run){
        for(m in 1:nmembers){
          x[1,m ,1:nstates] <- rep(c(the_temps_init, 
                                     wq_init_vals))
        }
      }
      if(include_wq){
        for(m in 1:nmembers){
          index <- which(x[1,m,] < 0.0)
          x[1, m, index[which(index < wq_end[num_wq_vars])]] <- 0.0
        }
      }
      
      for(par in 1:npars){
        x[1, ,(nstates+par)] <- runif(n=nmembers,par_init_lowerbound[par], par_init_upperbound[par])
        if(single_run){
          x[1, ,(nstates+par)] <-  rep(par_init_mean[par], nmembers)
        }
      }
      
      
      if(initial_condition_uncertainty == FALSE & hist_days == 0){
        state_means <- colMeans(x[1, , 1:nstates])
        for(m in 1:nmembers){
          x[1, m, ] <- state_means
        }
      }
    }else{
      x[1, ,1:nstates] <- rmvnorm(n=nmembers, 
                                  mean=c(the_temps_init,wq_init_vals),
                                  sigma=as.matrix(qt))
      
      if(single_run){
        for(m in 1:nmembers){
          x[1,m ,1:nstates] <- rep(c(the_temps_init, 
                                     wq_init_vals))
        }
      }
      
      if(initial_condition_uncertainty == FALSE & hist_days == 0){
        state_means <- colMeans(x[1, , 1:nstates])
        for(m in 1:nmembers){
          x[1, m, ] <- state_means
        }
      }
    }
  }else{
    if(npars > 0){
      x[1, ,1:nstates] <- rmvnorm(n=nmembers, 
                                  mean=c(the_temps_init), 
                                  sigma=as.matrix(qt_init[1:nstates,1:nstates]))
      
      
      for(par in 1:npars){
        x[1, ,(nstates+par)] <- runif(n=nmembers,par_init_lowerbound[par], par_init_upperbound[par])
        if(single_run){
          x[1, ,(nstates+par)] <- rep(par_init_mean[par], nmembers)
        }
      }
      
      if(initial_condition_uncertainty == FALSE){
        state_means <- colMeans(x[1, , 1:nstates])
        for(m in 1:nmembers){
          x[1, m, ] <- state_means
        }
      }
    }else{
      x[1, , ] <- rmvnorm(n=nmembers, 
                          mean=the_temps_init,
                          sigma=as.matrix(qt))
      
      if(initial_condition_uncertainty == FALSE & hist_days == 0){
        state_means <- colMeans(x[1, , 1:nstates])
        for(m in 1:nmembers){
          x[1, m, ] <- state_means
        }
      }
    }
  }
  if(include_wq){
    for(m in 1:nmembers){
      for(wq in 1:num_wq_vars){
        index <- which(x[1, m, 1:wq_end[num_wq_vars]] < 0.0)
        index <- index[which(index > wq_start[1])]
        x[1, m, index] <- 0.0
      }
    }
  }
  write.csv(x[1, , ],paste0(working_directory, "/", "restart_",
                            year(full_time_local[1]), "_",
                            month(full_time_local[1]), "_",
                            day(full_time_local[1]), "_cold.csv"),
            row.names = FALSE)
}

#THIS ALLOWS THE EnKF TO BE RESTARTED FROM YESTERDAY"S RUN
if(restart_present){
  print("Using restart file")
  nc <- nc_open(restart_file)
  restart_nmembers <- length(ncvar_get(nc, "ens"))
  x_restart_varname <- "x_restart"
  if(restart_nmembers > nmembers){
    #sample restart_nmembers
    sampled_nmembers <- sample(seq(1, restart_nmembers, 1),
                               nmembers,
                               replace=FALSE)
    restart_x_previous <- ncvar_get(nc, x_restart_varname)
    x_previous <- restart_x_previous[sampled_nmembers, ]
    
  }else if(restart_nmembers < nmembers){
    sampled_nmembers <- sample(seq(1, restart_nmembers, 1),
                               nmembers,
                               replace = TRUE)
    restart_x_previous <- ncvar_get(nc, x_restart_varname)
    x_previous <- restart_x_previous[sampled_nmembers, ]
    
  }else{
    restart_x_previous <- ncvar_get(nc, x_restart_varname)
    x_previous <- restart_x_previous
    
  }
  nc_close(nc)
}else{
  x_previous <- read.csv(paste0(working_directory, "/", "restart_",
                                year(full_time_local[1]), "_",
                                month(full_time_local[1]), "_",
                                day(full_time_local[1]), "_cold.csv"))
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
      x[1, m, (nstates + 1):(nstates + npars)] <- mean_pars
    }
  }
}

#Matrix to store essemble specific surface height
surface_height <- array(NA, dim=c(nsteps, nmembers))
surface_height[1, ] <- round(lake_depth_init, 3)

#Matrix to store snow and ice heights
snow_ice_thickness <- array(NA, dim=c(nsteps, nmembers, 3))
snow_ice_thickness[1, ,1] <- default_snow_thickness_init
snow_ice_thickness[1, ,2] <- default_white_ice_thickness_init
snow_ice_thickness[1, ,3] <- default_blue_ice_thickness_init

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
                        npars,
                        modeled_depths,
                        surface_height,
                        wq_start,
                        wq_end,
                        met_file_names,
                        include_wq,
                        spin_up_days,
                        z_states,
                        glm_output_vars,
                        process_uncertainty,
                        initial_condition_uncertainty,
                        parameter_uncertainty,
                        machine,
                        hist_days,
                        print_glm2screen,
                        x_phyto_groups,
                        inflow_file_names,
                        outflow_file_names,
                        management_input,
                        forecast_sss_on,
                        snow_ice_thickness)

x <- enkf_output$x
x_restart <- enkf_output$x_restart
qt_restart <- enkf_output$qt_restart
x_prior <- enkf_output$x_prior
surface_height_restart <- enkf_output$surface_height_restart
snow_ice_restart <- enkf_output$snow_ice_restart
snow_ice_thickness <- enkf_output$snow_ice_thickness
surface_height <- enkf_output$surface_height

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
time_of_forecast_string <- paste0(month(Sys.time()),
                                  day(Sys.time()),
                                  year(Sys.time()-2000),"_",
                                  hour(Sys.time()), "_",
                                  (minute(Sys.time())))


###SAVE FORECAST
write_forecast_netcdf(x,
                      full_time_local,
                      qt,
                      modeled_depths,
                      save_file_name,
                      x_restart,
                      qt_restart,
                      time_of_forecast,
                      hist_days,
                      x_prior,
                      include_wq,
                      wq_start,
                      wq_end,
                      z,
                      nstates,
                      npars,
                      GLMversion,
                      FLAREversion,
                      local_tzone,
                      surface_height_restart,
                      snow_ice_restart,
                      snow_ice_thickness,
                      surface_height)

##ARCHIVE FORECAST
restart_file_name <- archive_forecast(working_directory = working_directory,
                                      folder = code_folder, 
                                      forecast_base_name = forecast_base_name, 
                                      forecast_location = forecast_location,
                                      push_to_git = push_to_git,
                                      save_file_name = save_file_name, 
                                      time_of_forecast_string = time_of_forecast_string)

return(list(restart_file_name <- restart_file_name,
            sim_name <- paste0(save_file_name, "_", time_of_forecast_string)))
}
