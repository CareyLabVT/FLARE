#----------------------------------------------------------#
# Program name: run_enkf_forecast                          #
# Author: R. Quinn Thomas, rqthomas@vt.edu                 #
# Purpose: Sets up and launches the ensemble Kalman Filter #
#          Downloads and process model inputs and sensor   #
#          observations                                    #
# ---------------------------------------------------------#

run_flare<-function(start_day,
                    sim_name = NA, 
                    hist_days = 1,
                    forecast_days = 16,  
                    spin_up_days = 0,
                    restart_file = NA,
                    folder, 
                    forecast_location = NA,
                    push_to_git = FALSE,
                    pull_from_git = TRUE, 
                    data_location = NA, 
                    n_enkf_members = NA,
                    n_ds_members = 50,
                    include_wq = FALSE,
                    use_ctd = use_ctd,
                    uncert_mode = 1,
                    reference_tzone,
                    cov_matrix = NA,
                    downscaling_coeff = NA,
                    GLMversion,
                    DOWNSCALE_MET = TRUE,
                    FLAREversion,
                    met_ds_obs_start,
                    met_ds_obs_end,
                    modeled_depths){
  
  #################################################
  ### LOAD R FUNCTIONS
  #################################################
  
  source(paste0(folder,"/","Rscripts/edit_nml_functions.R"))
  source(paste0(folder,"/","Rscripts/create_obs_met_input.R"))
  source(paste0(folder,"/","Rscripts/extract_temp_chain.R"))
  source(paste0(folder,"/","Rscripts/process_GEFS2GLM.R"))
  source(paste0(folder,"/","Rscripts/extract_temp_CTD.R"))
  source(paste0(folder,"/","Rscripts/create_inflow_outflow_file.R"))
  source(paste0(folder,"/","Rscripts/archive_forecast.R"))
  source(paste0(folder,"/","Rscripts/write_forecast_netcdf.R")) 
  source(paste0(folder,"/","Rscripts/run_EnKF.R")) 
  source(paste0(folder,"/","Rscripts/met_downscale/process_downscale_GEFS.R")) 
  source(paste0(folder,"/","Rscripts/update_qt.R"))
  source(paste0(folder,"/","Rscripts/glmtools.R"))
  
  
  # SET UP NUMBER OF ENSEMBLE MEMBERS
  n_met_members <- 21
  if(single_run){
    n_met_members <- 1
  }
  
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
  ### STEP 1: GRAB DATA FROM REPO OR SERVER
  #################################################
  
  temperature_location <- paste0(data_location, "/", "mia-data")
  met_station_location <- paste0(data_location, "/", "carina-data")
  noaa_location <- paste0(data_location, "/", "noaa-data")
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
    
    setwd(temperature_location)
    system(paste0("git pull"))
    
    setwd(met_station_location)
    system(paste0("git pull"))
    
    setwd(noaa_location)
    system(paste0("git pull"))
  }
  
  
  #################################################
  ### OPTIONS TO ISOLATE COMPONENTS OF uncertainty
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
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- FALSE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    spin_up_days <- hist_days + 1
  }
  
  ####################################################
  #### STEP 2: DETECT PLATFORM  
  ####################################################
  
  switch(Sys.info() [["sysname"]],
         Linux = { machine <- "unix" },
         Darwin = { machine <- "mac" },
         Windows = { machine <- "windows"})
  
  ###INSTALL PREREQUISITES##
  
  #INSTALL libnetcdf
  if(machine == "unix") {
    system("if [ $(dpkg-query -W -f='${Status}' libnetcdf-dev 2>/dev/null | grep -c 'ok installed') -eq 0 ]; then sudo apt update && sudo apt install libnetcdf-dev; fi;")
    Sys.setenv(LD_LIBRARY_PATH=paste("../glm/unix/", Sys.getenv("LD_LIBRARY_PATH"),sep=":"))
  }
  
  ####################################################
  #### STEP 3: CREATE TIME VECTORS
  ####################################################
  
  # The simulations are run from 00:00:00 GMT time 
  # so that they directly interface with the NOAA forecast
  # The output is converted back to local time before being saved
  
  total_days <- hist_days + forecast_days
  start_forecast_step <- hist_days
  
  start_datetime_local <- as_datetime(paste0(start_day_local," ",start_time_local), tz = local_tzone)
  end_datetime_local <- start_datetime_local + total_days*24*60*60
  forecast_start_time_local <- start_datetime_local + start_forecast_step*24*60*60
  
  
  start_datetime_GMT <- with_tz(start_datetime_local, tzone = "GMT")
  end_datetime_GMT <- with_tz(end_datetime_local, tzone = "GMT")
  forecast_start_time_GMT<- with_tz(forecast_start_time_local, tzone = "GMT")
  
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
                              as.POSIXct(full_time_GMT[length(full_time_GMT)],
                                         tz = local_tzone),
                              by = "1 hour")
  

  ####################################################
  #### STEP 4: SET ARRAY LENGTHS
  ####################################################
  
  nsteps <- length(full_time_local)
  ndepths_modeled <- length(modeled_depths)
  num_wq_vars <- length(wq_names) 
  glm_output_vars <- c("temp", wq_names)
  
  ####################################################
  #### STEP 5: ORGANIZE FILES
  ####################################################
  
  ###CREATE DIRECTORY PATHS AND STRUCTURE
  working_glm <- paste0(folder, "/", "GLM_working")  
  ####Clear out temp GLM working directory
  unlink(paste0(working_glm, "/*"), recursive = FALSE)   
  
  forecast_base_name <- paste0(year(forecast_start_time_GMT),
                               forecast_month_GMT,
                               forecast_day_GMT,
                               "gep_all_",
                               noaa_hour,
                               "z")
  temp_obs_fname_wdir <-  paste0(working_glm, "/", temp_obs_fname)
  met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
  met_forecast_base_file_name <- paste0("met_hourly_",
                                        forecast_base_name,
                                        "_ens")
  if(is.na(sim_name)){
    sim_name <- paste0(year(full_time_local[1]), "_",
                       month(full_time_local[1]), "_",
                       day(full_time_local[1]))
  }
  
  ####################################################
  #### STEP 6: PROCESS AND ORGANIZE DATA
  ####################################################
  
  ###CREATE HISTORICAL MET FILE
  if(met_downscale_uncertainty == FALSE){
    n_ds_members <- 1
  }
  
  
  met_file_names <- rep(NA, 1+(n_met_members*n_ds_members))
  obs_met_outfile <- paste0(working_glm, "/", "GLM_met.csv")
  create_obs_met_input(fname = met_obs_fname_wdir,
                       outfile=obs_met_outfile,
                       full_time_hour_local, 
                       input_file_tz = "EST5EDT",
                       local_tzone)
  met_file_names[1] <- obs_met_outfile
  
  ###CREATE FUTURE MET FILES
  if(forecast_days > 0){
    in_directory <- paste0(noaa_location)
    out_directory <- working_glm
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
    
    met_file_names[2:(1+(n_met_members*n_ds_members))] <- process_downscale_GEFS(folder,
                                                                                 noaa_location,
                                                                                 met_station_location,
                                                                                 working_glm,
                                                                                 sim_files_folder = paste0(folder, "/", "sim_files"),
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
    #plot_downscaled_met(met_file_names, VarInfo$VarNames, working_glm)
  }
  
  if(weather_uncertainty == FALSE){
    n_met_members <- 1
  }
  
  ###MOVE DATA FILES AROUND
  sim_files_folder <- paste0(folder, "/", "sim_files")
  GLM_folder <- paste0(folder, "/", "glm", "/", machine) 
  fl <- c(list.files(sim_files_folder, full.names = TRUE))
  tmp <- file.copy(from = fl, to = working_glm, overwrite = TRUE)
  fl <- c(list.files(GLM_folder, full.names = TRUE))
  tmp <- file.copy(from = fl, to = working_glm, overwrite = TRUE)
  if(!is.na(restart_file)){
    tmp <- file.copy(from = restart_file, to = working_glm, overwrite = TRUE)
  }
  if(pre_scc){
    fl <- c(list.files("/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SCC_data/preSCC/",
                       full.names = TRUE))
    tmp <- file.copy(from = fl, to = working_glm, overwrite = TRUE)
  }
  if(include_wq){
    file.copy(from = paste0(working_glm, "/", "glm3_wAED.nml"), 
              to = paste0(working_glm, "/", "glm3.nml"), overwrite = TRUE)
  }else{
    if(!pre_scc){
      file.copy(from = paste0(working_glm, "/", "glm3_woAED.nml"), 
                to = paste0(working_glm, "/", "glm3.nml"), overwrite = TRUE)
    }else{
      file.copy(from = paste0(working_glm, "/", "glm3_woAED_preSCC.nml"), 
                to = paste0(working_glm, "/", "glm3.nml"), overwrite = TRUE)
    }
  }
  
  ##CREATE INFLOW AND OUTFILE FILES
  if(!pre_scc){
    create_inflow_outflow_file(full_time_day_local,
                               working_glm, 
                               input_file_tz = "EST5EDT",
                               start_forecast_step,
                               hold_inflow_outflow_constant,
                               inflow_file1,
                               inflow_file2,
                               outflow_file1,
                               local_tzone)
  }
  
  #Extract observations
  temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname)
  #PROCESS TEMPERATURE OBSERVATIONS
  obs_temp <- extract_temp_chain(fname = temp_obs_fname_wdir,
                                 full_time_local,
                                 modeled_depths = modeled_depths,
                                 observed_depths_temp = observed_depths_temp,
                                 input_file_tz = "EST5EDT",
                                 local_tzone)
  for(i in 1:length(obs_temp$obs[, 1])){
    for(j in 1:length(obs_temp$obs[1, ])){
      if(obs_temp$obs[i, j] == 0 | 
         is.na(obs_temp$obs[i, j]) | 
         is.nan(obs_temp$obs[i, j])){
        obs_temp$obs[i, j] = NA
      } 
    }
  }
  
  init_temps <- obs_temp$obs[1, ]
  
  #PROCESS DO OBSERVATIONS
  obs_do <- extract_do_chain(fname = temp_obs_fname_wdir,
                             full_time_local,
                             modeled_depths = modeled_depths,
                             observed_depths_do= observed_depths_do,
                             input_file_tz = "EST5EDT", 
                             local_tzone)
  obs_do$obs <- obs_do$obs*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  init_do1 <- obs_do$obs[1, ]
  
  obs_chla_fdom <- extract_chla_chain(fname = temp_obs_fname_wdir,
                                      full_time_local,
                                      modeled_depths = modeled_depths,
                                      observed_depths_chla_fdom,
                                      input_file_tz = "EST5EDT", 
                                      local_tzone)
  
  #DIRRRRTY qsu -> mg/L ->  mmol/m3
  #Need to fix
  obs_chla_fdom$fDOM_obs <- obs_chla_fdom$fDOM_obs*1000/(45*12)   

  #Use the CTD observation rather than the sensor string when CTD data is avialable
  if(use_ctd){
    ## LOOK AT CTD DATA
    fl <- c(list.files("/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SCC_data/preSCC/",
                       pattern = "CTD", 
                       full.names = TRUE))
    
    #NEED TO DOUBLE CHECK TIME ZONE
    obs_ctd <- extract_temp_CTD(fname = fl[1],
                                full_time_day_local,
                                modeled_depths = modeled_depths,
                                input_file_tz = "EST5EDT",
                                local_tzone)
    
    obs_ctd$obs_do <- obs_ctd$obs_do*1000/32
    for(i in 1:length(full_time_day_local)){
      if(!is.na(obs_ctd$obs_temp[i, 1])){
        obs_temp$obs[i,] <- obs_ctd$obs_temp[i, ]
        obs_do$obs[i,] <- obs_ctd$obs_do[i, ]
        obs_chla_fdom$Chla_obs[i,] <- obs_ctd$obs_chla[i, ]
      }
    }
    #init_pH_obs <- obs_ctd$obs_pH[1, which(!is.na(obs_ctd$obs_pH[1, ]))]
    #init_obs_pH_depths <- modeled_depths[which(!is.na(obs_ctd$obs_pH[1, ]))]
    
    #init_sal_obs <- obs_ctd$obs_sal[1, which(!is.na(obs_ctd$obs_sal[1, ]))]
    #init_obs_sal_depths <- modeled_depths[which(!is.na(obs_ctd$obs_sal[1, ]))]
  }
  
  init_temps_obs <- obs_temp$obs[1, which(!is.na(obs_temp$obs[1, ]))]
  init_obs_temp_depths <- modeled_depths[which(!is.na(obs_temp$obs[1, ]))]
  
  init_do_obs <- obs_do$obs[1, which(!is.na(obs_do$obs[1, ]))]
  init_obs_do_depths <- modeled_depths[which(!is.na(obs_do$obs[1, ]))]
  
  init_chla_obs <- obs_chla_fdom$Chla_obs[1, which(!is.na(obs_chla_fdom$Chla_obs[1, ]))]
  init_chla_obs_depths <- modeled_depths[which(!is.na(obs_chla_fdom$Chla_obs[1, ]))]
  
  init_doc_obs <- obs_chla_fdom$fDOM_obs[1, which(!is.na(obs_chla_fdom$fDOM_obs[1, ]))]
  init_doc_obs_depths <- modeled_depths[which(!is.na(obs_chla_fdom$fDOM_obs[1, ]))]
  
  
  #NEED AN ERROR CHECK FOR WHETHER THERE ARE OBSERVED DATA
  if(is.na(restart_file)){
    if((length(which(init_temps_obs != 0.0)) == 0) |
       length(which(is.na(init_temps_obs))) > 0){
      print("Pick another start day or provide an initial condition file: 
            observations not avialable for starting day")
      break
    }
    temp_inter <- approxfun(init_obs_temp_depths, init_temps_obs, rule=2)
    the_temps_init <- temp_inter(modeled_depths)
    if(include_wq){
      do_inter <- approxfun(init_obs_do_depths, init_do_obs, rule=2)
      OXY_oxy_init_depth <- do_inter(modeled_depths)
      #if(use_ctd){
      #  if(length(which(!is.na(init_pH_obs))) > 0){
          #pH_inter <- approxfun(init_obs_pH_depths, init_pH_obs, rule=2)
          #CAR_pH_init_depth <- pH_inter(modeled_depths)
      #  }
      #}
      if(length(init_chla_obs) == 1){
        PHY_TCHLA_init_depth <- rep(init_chla_obs, length(modeled_depths))
      }else{
        PHY_TCHLA_init_depth <- init_chla_obs
      }
      if(length(init_doc_obs) == 1){
        OGM_doc_init_depth <- rep(init_doc_obs, length(modeled_depths))
      }
      
      
    }
  }
  
  ####################################################
  #### STEP 7: SET UP INITIAL CONDITIONS
  ####################################################
  
  wq_start <- NA
  wq_end <- NA
  if(include_wq){
    temp_start <- 1
    temp_end <- length(modeled_depths)
    wq_start <- rep(NA, num_wq_vars)
    wq_end <- rep(NA, num_wq_vars)
    for(wq in 1:num_wq_vars){
      if(wq == 1){
        wq_start[wq] <- temp_end+1
        wq_end[wq] <- temp_end + (length(modeled_depths))
      }else{
        wq_start[wq] <- wq_end[wq-1]+1
        wq_end[wq] <- wq_end[wq-1] + (length(modeled_depths))
      }
      
      if(npars > 0){ #NEED TO GENERALIZE
        par1 <- wq_end[num_wq_vars] + 1
        par2 <- par1 + 1
        par3 <-  par2 + 1
        par4 <-  par3 + 1
        par5 <-  par4 + 1
        par6 <-  par5 + 1
      }else{
        par1 <- wq_end[num_wq_vars]
        par2 <- wq_end[num_wq_vars]
        par3 <- wq_end[num_wq_vars]
        par4 <- wq_end[num_wq_vars]
        par5 <- wq_end[num_wq_vars]
        par6 <- wq_end[num_wq_vars]
      }
      
    }
  }else{
    temp_start <- 1
    temp_end <- length(modeled_depths)
    if(npars > 0){
      par1 <- temp_end + 1
      par2 <- par1 + 1
      par3 <-  par2 + 1
      par4 <-  par3 + 1
      par5 <-  par4 + 1
      par6 <-  par5 + 1
    }else{
      par1 <- temp_end
      par2 <- temp_end
      par3 <- temp_end
      par4 <- temp_end
      par5 <- temp_end
      par6 <- temp_end
    }
  }
  
  #UPDATE NML WITH PARAMETERS AND INITIAL CONDITIONS
  init_donc <- 1.3/47.4
  init_dopc <-1.5/47.4
  
  #Initial States
  the_sals_init <- 0.0
  OXY_oxy_init <- 300.62
  CAR_pH_init <- 6.5
  CAR_dic_init <- 59.1
  CAR_ch4_init <- 0.58
  SIL_rsi_init <- 300
  NIT_amm_init <- 0.69
  NIT_nit_init <- 0.05
  PHS_frp_init <- 0.07
  OGM_doc_init <- 47.4
  OGM_poc_init <- 78.5
  OGM_don_init <- 1.3
  OGM_pon_init <- 8.3
  OGM_dop_init <- 1.5
  OGM_pop_init <- 8.3
  PHY_TCHLA_init <- 2.0
  
  OXY_oxy_error <- OXY_oxy_init*0.1
  CAR_pH_error <- CAR_pH_init*0.1
  CAR_dic_error <- CAR_dic_init*0.1
  CAR_ch4_error <- CAR_ch4_init*0.1
  SIL_rsi_error <- SIL_rsi_init*0.1
  NIT_amm_error <- NIT_amm_init*0.1
  NIT_nit_error <- NIT_nit_init*0.1
  PHS_frp_error <- PHS_frp_init*0.1
  OGM_doc_error <- OGM_doc_init*0.1
  OGM_poc_error <- OGM_poc_init*0.1
  OGM_don_error <- OGM_don_init*0.1
  OGM_pon_error <- OGM_pon_init*0.1
  OGM_dop_error <- OGM_dop_init*0.1
  OGM_pop_error <- OGM_pop_init*0.1
  PHY_TCHLA_error <- PHY_TCHLA_init*0.1
  
  wq_var_error <- c(OXY_oxy_error,
                    CAR_pH_error,
                    CAR_dic_error,
                    CAR_ch4_error,
                    SIL_rsi_error,
                    NIT_amm_error,
                    NIT_nit_error,
                    PHS_frp_error,
                    OGM_doc_error,
                    OGM_poc_error, 
                    OGM_don_error,
                    OGM_pon_error,
                    OGM_dop_error,
                    OGM_pop_error,
                    PHY_TCHLA_error)
  
  
  #if(include_wq & use_ctd){
    #OXY_oxy_init_depth <- do_init
  #}else{
  #  OXY_oxy_init_depth <- rep(OXY_oxy_init, ndepths_modeled)  
  #  OXY_oxy_init_depth[1] <- 400
  #}
  #if(include_wq & use_ctd){
  #  CAR_pH_init_depth <- pH_init
  #}else{
    CAR_pH_init_depth <- rep(CAR_pH_init, ndepths_modeled) 
  #}
  
  CAR_dic_init_depth <- rep(CAR_dic_init, ndepths_modeled)
  CAR_ch4_init_depth <- rep(CAR_ch4_init, ndepths_modeled)
  SIL_rsi_init_depth <- rep(SIL_rsi_init, ndepths_modeled)
  NIT_amm_init_depth <- rep(NIT_amm_init, ndepths_modeled)
  NIT_nit_init_depth <- rep(NIT_nit_init, ndepths_modeled)
  PHS_frp_init_depth <- rep(PHS_frp_init, ndepths_modeled)
  #OGM_doc_init_depth <- rep(OGM_doc_init, ndepths_modeled)
  OGM_poc_init_depth <- rep(OGM_poc_init, ndepths_modeled)
  OGM_don_init_depth <- OGM_doc_init_depth * init_donc
  #OGM_don_init_depth <- rep(OGM_don_init, ndepths_modeled)
  OGM_pon_init_depth <- rep(OGM_pon_init, ndepths_modeled)
  OGM_dop_init_depth <- OGM_doc_init_depth * init_dopc
  #OGM_dop_init_depth <- rep(OGM_dop_init, ndepths_modeled)
  OGM_pop_init_depth <- rep(OGM_pop_init, ndepths_modeled)
  #PHY_TCHLA_init_depth <- rep(PHY_TCHLA_init, ndepths_modeled)
  
  #phyto_proportions <- c(0.25, 0.25, 0.25, 0.25)
  
  PHY_CYANOPCH1_init_depth <- PHY_TCHLA_init_depth/biomass_to_chla[1]
  #PHY_CYANONPCH2_init_depth <- PHY_TCHLA_init_depth * phyto_proportions[2]
  #PHY_CHLOROPCH3_init_depth <- PHY_TCHLA_init_depth * phyto_proportions[3]
  #PHY_DIATOMPCH4_init_depth <- PHY_TCHLA_init_depth * phyto_proportions[4] 
  
  if(full_time_day_local[1] == 
     strftime("2018-07-09",format="%Y-%m-%d",tz = "EST5EDT") &
     include_wq){
    curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9,10)
    #mg/L
    curr_values <- c(3.764, 3.781, 3.578, 5.156, 5.2735, 5.5165, 5.222, 5.368)
    curr_values <- (curr_values*1000)/(10*12)
    inter <- approxfun(curr_depths,curr_values,rule=2)
    CAR_dic_init_depth <- inter(modeled_depths)
    
    curr_depths <- c(0.1,1.6,3.8,5,6.2,8,9,9.5)
    #umol CH4/L
    curr_values <- c(3.91E-04,0.370572728,0.107597836,0.126096596,
                     0.088502664,0.086276629,0.07256043,0.07249431)
    curr_values <- curr_values#*1000
    inter <- approxfun(curr_depths,curr_values,rule=2)
    CAR_ch4_init_depth <- inter(modeled_depths)
    
    curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9, 10)
    #ug/L
    curr_values <- c(12.65291714,4.213596723,10.5935375,13.43611258,
                     11.34765394,11.95676704,11.98577285,12.82695814)
    curr_values <- (curr_values*1000)/14
    inter <- approxfun(curr_depths,curr_values,rule=2)
    NIT_amm_init_depth <- inter(modeled_depths)
    
    curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9, 10)
    curr_values <-c(5.68,3.82,4.46,3.71,4.18,5.08,3.01,7.72)
    curr_values <- (curr_values*1000)/14
    #ug/L
    inter <- approxfun(curr_depths,curr_values,rule=2)
    NIT_nit_init_depth <- inter(modeled_depths)
    
    curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9, 10)
    #ug/L
    curr_values <- c(8.96,7.66,6.26,6.22,7.72,9.69,7.95,10.5)
    curr_values <- (curr_values*1000)/18
    inter <- approxfun(curr_depths,curr_values,rule=2)
    PHS_frp_init_depth <- inter(modeled_depths)
    
    curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9, 10)
    ##mg/L
    curr_values <- c(4.2315,4.374, 3.2655,2.9705,2.938,2.922,2.773,2.9525)
    curr_values <- (curr_values*1000)/(10*12)
    inter <- approxfun(curr_depths,curr_values,rule=2)
    OGM_doc_init_depth <- inter(modeled_depths)
    
    curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9, 10)
    ##mg/L
    curr_values <- c(0.2855,0.261,0.218,0.2135,0.2185,0.223,0.2025,0.2065)
    curr_values <- (curr_values*1000)/(10*14)
    inter <- approxfun(curr_depths,curr_values,rule=2)
    DN_init_depth <- inter(modeled_depths)
    OGM_don_init_depth <- DN_init_depth - NIT_amm_init_depth - NIT_nit_init_depth
  }
  
  wq_init_vals_w_tchla <- c(OXY_oxy_init_depth,
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
                            PHY_TCHLA_init_depth)
  
  wq_init_vals_w_phytos <- c(OXY_oxy_init_depth,
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
                             PHY_CYANOPCH1_init_depth)
  
  
  #UPDATE NML WITH PARAMETERS AND INITIAL CONDITIONS
  if(include_wq){
    update_var(wq_init_vals_w_phytos, "wq_init_vals", working_glm, "glm3.nml")
    update_var(length(wq_init_vals_w_phytos)/length(OXY_oxy_init_depth), "num_wq_vars", working_glm, "glm3.nml")
  }else{
    update_var(" ", "wq_init_vals", working_glm, "glm3.nml")
    update_var(0, "num_wq_vars", working_glm, "glm3.nml")
  }
  update_var(ndepths_modeled, "num_depths", working_glm, "glm3.nml")
  update_var(modeled_depths, "the_depths", working_glm, "glm3.nml")
  update_var(rep(the_sals_init, ndepths_modeled), "the_sals", working_glm, "glm3.nml")
  
  #Create a copy of the NML to record starting parameters
  file.copy(from = paste0(working_glm, "/", "glm3.nml"), 
            to = paste0(working_glm, "/", "glm3_initial.nml"), overwrite = TRUE)
  
  #NUMBER OF STATE SIMULATED = SPECIFIED DEPTHS
  if(include_wq){
    nstates <- ndepths_modeled*(1+num_wq_vars)
  }else{
    nstates <- ndepths_modeled
  }
  
  if(include_wq){
    #NEED TO ADD ADDITIONAL OBSERVATION TYPES
    nobs <- length(observed_depths_temp) + 
      length(observed_depths_do) + 
      length(observed_depths_chla_fdom) + 
      length(observed_depths_chla_fdom) 
  }else{
    nobs <- length(observed_depths_temp)
  }
  
  ####################################################
  #### STEP 8: CREATE THE Z ARRAY (OBSERVATIONS x TIME)
  ####################################################
  
  #Observations for each observed state at each time step
  #an observation with at least 1 observation but without an 
  #observation in a time-step gets assigned an NA
  
  if(include_wq){
    OXY_oxy_obs <- obs_do$obs
    CAR_pH_obs <- array(NA, dim = dim(obs_do$obs))
    CAR_dic_obs <- array(NA, dim = dim(obs_do$obs))
    CAR_ch4_obs <- array(NA, dim = dim(obs_do$obs))
    SIL_rsi_obs <- array(NA, dim = dim(obs_do$obs))
    NIT_amm_obs <- array(NA, dim = dim(obs_do$obs))
    NIT_nit_obs <- array(NA, dim = dim(obs_do$obs))
    PHS_frp_obs <- array(NA, dim = dim(obs_do$obs))
    OGM_doc_obs <- obs_chla_fdom$fDOM_obs
    OGM_poc_obs <- array(NA, dim = dim(obs_do$obs))
    OGM_don_obs <- array(NA, dim = dim(obs_do$obs))
    OGM_pon_obs <- array(NA, dim = dim(obs_do$obs))
    OGM_dop_obs <- array(NA, dim = dim(obs_do$obs))
    OGM_pop_obs <- array(NA, dim = dim(obs_do$obs))
    PHY_TCHLA_obs <- obs_chla_fdom$Chla_obs
    
    #z <- cbind(obs_temp$obs, obs_do$obs)
    
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
               PHY_TCHLA_obs)
  }else{
    z <- cbind(obs_temp$obs) 
  }
  
  z_obs <- z
  if(!use_obs_constraint){
    z[, ] <- NA
  }
  
  #FIGURE OUT WHICH DEPTHS HAVE OBSERVATIONS
  if(include_wq){
    obs_index <- rep(NA,length(modeled_depths)*(num_wq_vars+1))
    obs_index[1:length(modeled_depths)] <- seq(1, length(modeled_depths), 1)
    for(wq in 1:num_wq_vars){
      obs_index[wq_start[wq]:wq_end[wq]] <- seq(wq_start[wq], wq_end[wq], 1)
    }
  }else{
    obs_index <- rep(NA,length(modeled_depths))
    obs_index[1:length(modeled_depths)] <- seq(1, length(modeled_depths), 1)
  }
  
  #Matrix for knowing which state the observation corresponds to
  z_states <- t(matrix(obs_index, nrow = length(obs_index), ncol = nsteps))
  
  
  #######################################################
  #### STEP 9: CREATE THE PSI VECTOR (DATA uncertainty)  
  #######################################################
  psi <- rep(NA, length(obs_index))
  
  psi[1: length(modeled_depths)] <- rep(obs_error_temperature, length(modeled_depths))
  if(include_wq){
    for(wq in 1:num_wq_vars){
      psi[wq_start[wq]:wq_end[wq]] <- rep(obs_error_wq[wq], length(modeled_depths))
    }
  }
  
  
  ####################################################
  #### STEP 12: CREATE THE QT ARRAY (MODEL VARIANCE)
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
    resid30day <- ncvar_get(nc, "resid30day")
    nc_close(nc)
  }else{
    #Process error 
    if(is.na(cov_matrix)){
      qt <- read.csv(paste0(working_glm, "/", "qt_cov_matrix.csv"))
    }else{
      qt <- read.csv(paste0(working_glm, "/", cov_matrix))
    }
    
    if(include_wq){
      for(i in 1:num_wq_vars){
        for(j in 1:ndepths_modeled){
          qt <- rbind(qt, rep(0.0, ncol(qt)))
          qt <- cbind(qt, rep(0.0, nrow(qt)))
          qt[ncol(qt),nrow(qt)] <- wq_var_error[i]
        }
      }
    }
    resid30day <- array(NA, dim =c(30, nrow(qt)))
  }
  #Covariance matrix for parameters
  if(npars > 0){
    qt_pars <- matrix(data = 0, nrow = npars, ncol = npars)
    diag(qt_pars) <- c(zone1temp_init_qt, zone2temp_init_qt, swf_init_qt,Fsed_oxy_init_qt,Rdom_minerl_init_qt,Rdom_minerl_init_qt)
  }else{
    qt_pars <- NA
  }
  
  
  ################################################################
  #### STEP 10: CREATE THE X ARRAY (STATES X TIME);INCLUDES INITIALATION
  ################################################################
  nmembers <- n_enkf_members*n_met_members*n_ds_members
  
  
  x <- array(NA, dim=c(nsteps, nmembers, nstates + npars))
  
  
  #Initial conditions
  if(!restart_present){
    if(include_wq){
      if(npars > 0){
        x[1, ,1:nstates] <- rmvnorm(n=nmembers, 
                                    mean=c(the_temps_init, 
                                           wq_init_vals_w_tchla), 
                                    sigma=as.matrix(qt))
        x[1, ,(nstates+1)] <- runif(n=nmembers,zone1_temp_init_lowerbound, zone1_temp_init_upperbound)
        x[1, ,(nstates+2)] <- runif(n=nmembers,zone2_temp_init_lowerbound, zone2_temp_init_upperbound)
        x[1, ,(nstates+3)] <- runif(n=nmembers,swf_init_lowerbound, swf_init_upperbound)
        x[1, ,(nstates+4)] <- runif(n=nmembers,Fsed_oxy_init_lowerbound, Fsed_oxy_init_upperbound)
        x[1, ,(nstates+5)] <- runif(n=nmembers,Rdom_minerl_init_lowerbound, Rdom_minerl_init_upperbound)
        x[1, ,(nstates+6)] <- runif(n=nmembers,R_growth_init_lowerbound, R_growth_init_upperbound)
        if(initial_condition_uncertainty == FALSE){
          for(m in 1:nmembers){
            x[1,m, ] <- c(the_temps_init,
                          wq_init_vals_w_tchla,
                          zone1_temp_init_mean,
                          zone2_temp_init_mean,
                          swf_init_mean,
                          Fsed_oxy_init_mean,
                          Rdom_minerl_init_mean,
                          R_growth_init_mean)
          }
        }
      }else{
        x[1, , ] <- rmvnorm(n=nmembers, 
                            mean=c(the_temps_init,wq_init_vals_w_tchla),
                            sigma=as.matrix(qt))
        if(initial_condition_uncertainty == FALSE){
          for(m in 1:nmembers){
            x[1, m, ] <- c(the_temps_init, do_init, wq_init_vals_w_tchla)
          }
        }
      }
    }else{
      if(npars > 0){
        x[1, ,1:nstates] <- rmvnorm(n=nmembers, 
                                    mean=the_temps_init,
                                    sigma=as.matrix(qt))
        
        
        x[1, ,(nstates+1)] <- runif(n=nmembers,zone1_temp_init_lowerbound, zone1_temp_init_upperbound)
        x[1, ,(nstates+2)] <- runif(n=nmembers,zone2_temp_init_lowerbound, zone2_temp_init_upperbound)
        x[1, ,(nstates+3)] <- runif(n=nmembers,swf_init_lowerbound, swf_init_upperbound)
        if(initial_condition_uncertainty == FALSE){
          for(m in 1:nmembers){
            x[1, m, ] <- c(the_temps_init, zone1_temp_init_mean, zone2_temp_init_mean, swf_init_mean)
          }
        }
      }else{
        x[1, , ] <- rmvnorm(n=nmembers, 
                            mean=the_temps_init,
                            sigma=as.matrix(qt))
        
        if(initial_condition_uncertainty == FALSE){
          for(m in 1:nmembers){
            if(npars > 0){
              x[1, m, ] <- the_temps_init
            }
          }
        }
      }
    }
    if(include_wq){
      for(m in 1:nmembers){
        for(wq in 1:num_wq_vars){
          index <- which(x[1, m, 1:par1] < 0.0)
          index <- index[which(index > wq_start[1])]
          x[1, m, index] <- 0.0
        }
      }
    }
    write.csv(x[1, , ],paste0(working_glm, "/", "restart_",
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
    if(restart_nmembers > nmembers){
      #sample restart_nmembers
      sampled_nmembers <- sample(seq(1, restart_nmembers, 1),
                                 nmembers,
                                 replace=FALSE)
      restart_x_previous <- ncvar_get(nc, "x_restart")
      x_previous <- restart_x_previous[sampled_nmembers, ]
      if(initial_condition_uncertainty == FALSE & hist_days == 0){
        x_previous_1 <- colMeans(x_previous)
        for(m in 1:nmembers){
          x_previous[m, ] <- x_previous_1
        }
      }
    }else if(restart_nmembers < nmembers){
      sampled_nmembers <- sample(seq(1, restart_nmembers, 1),
                                 nmembers,
                                 replace = TRUE)
      restart_x_previous <- ncvar_get(nc, "x_restart")
      x_previous <- restart_x_previous[sampled_nmembers, ]
      if(initial_condition_uncertainty == FALSE & hist_days == 0){
        x_previous_1 <- colMeans(x_previous)
        for(m in 1:nmembers){
          x_previous[m, ] <- x_previous_1
        }
      }
    }else{
      x_previous <- ncvar_get(nc, "x_restart")   
      if(initial_condition_uncertainty == FALSE & hist_days == 0){
        x_previous_1 <- colMeans(x_previous) 
        for(m in 1:nmembers){
          x_previous[m, ] <- x_previous_1
        }
      }
    }
    nc_close(nc)
  }else{
    x_previous <- read.csv(paste0(working_glm, "/", "restart_",
                                  year(full_time_local[1]), "_",
                                  month(full_time_local[1]), "_",
                                  day(full_time_local[1]), "_cold.csv"))
  }
  
  #Set initial conditions
  x[1,,] <- as.matrix(x_previous)
  
  #Matrix to store essemble specific surface height
  surface_height <- array(NA, dim=c(nsteps, nmembers))
  surface_height[1, ] <- lake_depth_init
  
  #
  if(include_wq){
    x_phyto_groups <- array(NA, dim=c(nsteps, nmembers, length(tchla_components_vars)*length(modeled_depths)))
    for(m in 1:nmembers){
      phyto_biomass <- matrix(c(PHY_CYANOPCH1_init_depth), 
                              nrow = length(modeled_depths),
                              ncol = length(tchla_components_vars))
      phyto_proportions <- phyto_biomass
      index <- 0
      for(d in 1:length(modeled_depths)){
        for(pp in 1:length(tchla_components_vars)){
          index <- index + 1
          phyto_proportions[d, pp] <- (phyto_biomass[d, pp]/biomass_to_chla[pp])/sum(phyto_biomass[d,]/biomass_to_chla)
          x_phyto_groups[1, m, index] <- biomass_to_chla[pp] * phyto_proportions[d, pp] * x[1, m, wq_start[num_wq_vars] + (d-1)]
        }
      }
      
    }
  }else{
    x_phyto_groups <- NA
  }
  
  
  
  
  ####################################################
  #### STEP 11: Run Ensemble Kalman Filter
  ####################################################
  
  enkf_output <- run_EnKF(x,
                          z,
                          qt,
                          qt_pars,
                          psi,
                          full_time_local,
                          working_glm,
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
                          resid30day,
                          hist_days,
                          print_glm2screen,
                          x_phyto_groups)
  
  x <- enkf_output$x
  x_restart <- enkf_output$x_restart
  qt_restart <- enkf_output$qt_restart
  x_prior <- enkf_output$x_prior
  resid30day <- enkf_output$resid30day
  
  ####################################################
  #### STEP 12: PROCESS OUTPUT
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
                        par1,
                        par2,
                        par3,
                        par4,
                        par5,
                        par6,
                        z,
                        nstates,
                        npars,
                        GLMversion,
                        FLAREversion,
                        resid30day,
                        local_tzone)
  
  ##ARCHIVE FORECAST
  restart_file_name <- archive_forecast(working_glm = working_glm,
                                        folder = folder, 
                                        forecast_base_name = forecast_base_name, 
                                        forecast_location = forecast_location,
                                        push_to_git = push_to_git,
                                        save_file_name = save_file_name, 
                                        time_of_forecast = time_of_forecast)
  
  return(list(restart_file_name <- restart_file_name,
              sim_name <- paste0(save_file_name, "_", time_of_forecast_string)))
}
