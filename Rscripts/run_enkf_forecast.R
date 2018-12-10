#----------------------------------------------------------#
# Program name: run_enkf_forecast                          #
# Author: R. Quinn Thomas, rqthomas@vt.edu                 #
# Purpose: Sets up and launches the ensemble Kalman Filter #
#          Downloads and process model inputs and sensor   #
#          observations                                    #
# ---------------------------------------------------------#

run_enkf_forecast<-function(start_day= "2018-07-06 00:00:00", 
                       sim_name = NA, 
                       hist_days = 1,
                       forecast_days = 16,  
                       spin_up_days = 0,
                       restart_file = NA,
                       folder, 
                       forecast_location = NA,
                       push_to_git=FALSE,
                       data_location = NA, 
                       n_enkf_members = NA,
                       include_wq = FALSE,
                       use_ctd = use_ctd,
                       uncert_mode = 1,
                       cov_matrix = NA,
                       alpha = c(0.5,0.5,0.5)){
  
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
  source(paste0(folder,"/","Rscripts/GLM_EnKF.R")) 
  
  #################################################
  ### CONFIGURATIONS THAT NEED TO BE GENERALIZED
  #################################################
  
  ###RUN OPTIONS
  npars <- 3
  pre_scc <- FALSE
  
  #Estimated parameters
  lake_depth_init <- 9.4  #not a modeled state
  zone2_temp <- 17
  zone1_temp <- 11
  zone1temp_init_qt <- 0.01 #THIS IS THE VARIANCE, NOT THE SD
  zone2temp_init_qt <- 0.01 #THIS IS THE VARIANCE, NOT THE SD
  swf_lwf_init <- 1.0
  swf_lwf_init_qt <- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD
  
  obs_error <- 0.0001 #NEED TO DOUBLE CHECK

  #Define modeled depths and depths with observations
  modeled_depths <- c(0.1, 0.33, 0.66, 
                       1.00, 1.33, 1.66,
                       2.00, 2.33, 2.66,
                       3.0, 3.33, 3.66,
                       4.0, 4.33, 4.66,
                       5.0, 5.33, 5.66,
                       6.0, 6.33, 6.66,
                       7.00, 7.33, 7.66,
                       8.0, 8.33, 8.66,
                       9.00, 9.33)
  
  observed_depths_temp <- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  observed_depths_do <- c(1, 5, 9)
  observed_depths_chla_fdom <- 1
  
  temp_obs_fname <- "Catwalk.csv"
  met_obs_fname <- "FCRmet.csv"
  
  #define water quality variables modeled.  Not used if include_wq == FALSE
  wq_names <- c("OXY_oxy",
                "CAR_pH",
                "CAR_dic",
                "CAR_ch4",
                "SIL_rsi",
                "NIT_amm",
                "NIT_nit",
                "PHS_frp",
                "OGM_doc",
                "OGM_poc",
                "OGM_don",
                "OGM_pon",
                "OGM_dop",
                "OGM_pop",
                "PHY_CYANOPCH1",
                "PHY_CYANONPCH2",
                "PHY_CHLOROPCH3",
                "PHY_DIATOMPCH4",
                "ZOO_COPEPODS1",
                "ZOO_DAPHNIABIG2",
                "ZOO_DAPHNIASMALL3")
  
  local_tzone <- "EST5EDT"
  
  # SET UP NUMBER OF ENSEMBLE MEMBERS
  n_met_members <- 21
  
  #################################################
  ### STEP 1: GRAB DATA FROM REPO OR SERVER
  #################################################
  
  temperature_location <- paste0(data_location, "/", "mia-data")
  setwd(temperature_location)
  system(paste0("git pull"))
  met_station_location <- paste0(data_location, "/", "carina-data")
  setwd(met_station_location)
  system(paste0("git pull"))
  noaa_location <- paste0(data_location, "/", "noaa-data")
  setwd(noaa_location)
  system(paste0("git pull"))
  
  #################################################
  ### OPTIONS TO ISOLATE COMPONENTS OF UNCERTAINITY
  #################################################
  
  if(uncert_mode == 1){
    #All sources of uncertainity and data used to constrain 
    use_obs_constraint <- TRUE
    #SOURCES OF UNCERTAINITY
    observation_uncertainity <- TRUE
    process_uncertainity <- TRUE
    weather_uncertainity <- TRUE
    initial_condition_uncertainity <- TRUE
    parameter_uncertainity <- TRUE
  }else if(uncert_mode == 2){
    #No sources of uncertainity and no data used to constrain 
    use_obs_constraint <- TRUE
    #SOURCES OF UNCERTAINITY
    observation_uncertainity <- TRUE
    process_uncertainity <- FALSE
    weather_uncertainity <- FALSE
    initial_condition_uncertainity <- FALSE
    parameter_uncertainity <- FALSE
  }else if(uncert_mode == 3){
    #Only process uncertainity
    use_obs_constraint <- TRUE
    #SOURCES OF UNCERTAINITY
    observation_uncertainity <- TRUE
    process_uncertainity <- TRUE
    weather_uncertainity <- FALSE
    initial_condition_uncertainity <- FALSE
    parameter_uncertainity <- FALSE
  }else if(uncert_mode == 4){
    #only weather uncertainity
    use_obs_constraint <- TRUE
    #SOURCES OF UNCERTAINITY
    observation_uncertainity <- TRUE
    process_uncertainity <- FALSE
    weather_uncertainity <- TRUE
    initial_condition_uncertainity <- FALSE
    parameter_uncertainity <- FALSE
  }else if(uncert_mode == 5){
    #only initial condition uncertainity with data constraint
    use_obs_constraint <- TRUE
    #SOURCES OF UNCERTAINITY
    observation_uncertainity <- TRUE
    process_uncertainity <- FALSE
    weather_uncertainity <- FALSE
    initial_condition_uncertainity <- TRUE
    parameter_uncertainity <- FALSE
  }else if(uncert_mode == 6){
    #only initial condition uncertainity without data constraint
    use_obs_constraint <- FALSE
    #SOURCES OF UNCERTAINITY
    observation_uncertainity <- TRUE
    process_uncertainity <- FALSE
    weather_uncertainity <- FALSE
    initial_condition_uncertainity <- TRUE
    parameter_uncertainity <- FALSE
  }else if(uncert_mode == 7){
    #only parameter uncertainity
    use_obs_constraint <- FALSE
    #SOURCES OF UNCERTAINITY
    observation_uncertainity <- TRUE
    process_uncertainity <- FALSE
    weather_uncertainity <- FALSE
    initial_condition_uncertainity <- FALSE
    parameter_uncertainity <- TRUE
  }
  if(observation_uncertainity){
    obs_error <- 0.0001 #NEED TO DOUBLE CHECK
  }
  
  ####################################################
  #### STEP 2: DETECT PLATFORM  
  ####################################################
  
  switch(Sys.info() [["sysname"]],
         Linux = { machine <- "unix" },
         Darwin = { machine <- "mac" })
  
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
  
  begin_sim  <- as.POSIXct(start_day,tz = reference_tzone)
  total_days <- hist_days + forecast_days
  end_sim <- begin_sim + total_days*24*60*60
  start_forecast_step <- hist_days
  forecast_start_time <- begin_sim + (start_forecast_step)*24*60*60
  if(day(forecast_start_time) < 10){
    forecast_day <- paste0("0", day(forecast_start_time))
  }else{
    forecast_day <- paste0(day(forecast_start_time))
  }
  if(month(forecast_start_time) < 10){
    forecast_month <- paste0("0", month(forecast_start_time))
  }else{
    forecast_month <- paste0(month(forecast_start_time))
  }
  full_time <- seq(begin_sim, end_sim, by = "1 day")
  full_time_local <- with_tz(full_time, tzone = local_tzone)
  full_time <- strftime(full_time, 
                        format="%Y-%m-%d %H:%M",
                        tz = reference_tzone)
  full_time_local <- strftime(full_time_local,
                              format="%Y-%m-%d %H:%M",
                              tz = local_tzone)
  full_time_day <- strftime(full_time,
                            format="%Y-%m-%d",
                            tz = reference_tzone)
  full_time_day_local <- strftime(full_time_local,
                                  format="%Y-%m-%d",
                                  tz = local_tzone)
  full_time_hour_obs <- seq(as.POSIXct(full_time[1],
                                       tz = reference_tzone), 
                            as.POSIXct(full_time[length(full_time)],
                                       tz = reference_tzone),
                            by = "1 hour")
  
  
  ####################################################
  #### STEP 4: SET ARRAY LENGTHS
  ####################################################
  
  nsteps <- length(full_time)
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
  forecast_base_name <- paste0(year(forecast_start_time),
                               forecast_month,
                               forecast_day,
                               "gep_all_00z")
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
  met_file_names <- rep(NA, 1+n_met_members)
  obs_met_outfile <- paste0(working_glm, "/", "GLM_met.csv")
  create_obs_met_input(fname = met_obs_fname_wdir,
                       outfile=obs_met_outfile,
                       full_time_hour_obs, 
                       input_tz = "EST5EDT", 
                       output_tz = reference_tzone)
  met_file_names[1] <- obs_met_outfile
  
  ###CREATE FUTURE MET FILES
  if(forecast_days > 0){
    in_directory <- paste0(noaa_location)
    out_directory <- working_glm
    file_name <- forecast_base_name
    #NEED TO DOUBLE CHECK THE INPUT_TZ AND WHY IT IS EST
    met_file_names[2:(1+n_met_members)] <- process_GEFS2GLM(in_directory,
                                                            out_directory,
                                                            file_name, 
                                                            #NEED TO CHANGE TO GMT IF FORECASTING AFTER DEC 8 00:00:00 GMT
                                                            input_tz = "EST5EDT", 
                                                            output_tz = reference_tzone)
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
    create_inflow_outflow_file(full_time = full_time_day,
                               working_glm = working_glm, 
                               input_tz = "EST5EDT",
                               output_tz = reference_tzone )
  }

  #Extract observations
  temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname)
  #PROCESS TEMPERATURE OBSERVATIONS
  obs_temp <- extract_temp_chain(fname = temp_obs_fname_wdir,
                                 full_time,
                                 depths = modeled_depths,
                                 observed_depths_temp = observed_depths_temp,
                                 input_tz = "EST5EDT",
                                 output_tz = reference_tzone)
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
                             full_time,
                             depths = modeled_depths,
                             observed_depths_do= observed_depths_do,
                             input_tz = "EST5EDT", 
                             output_tz = reference_tzone)
  obs_do$obs <- obs_do$obs*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  init_do1 <- obs_do$obs[1, ]
  
  obs_chla_fdom <- extract_chla_chain(fname = temp_obs_fname_wdir,
                                      full_time,
                                      depths = modeled_depths,
                                      observed_depths_chla_fdom = observed_depths_chla_fdom,
                                      input_tz = "EST5EDT", 
                                      output_tz = reference_tzone)
  
  #Use the CTD observation rather than the sensor string when CTD data is avialable
  if(use_ctd){
    ## LOOK AT CTD DATA
    fl <- c(list.files("/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SCC_data/preSCC/",
                       pattern = "CTD", 
                       full.names = TRUE))
    
    #NEED TO DOUBLE CHECK TIME ZONE
    obs_ctd <- extract_temp_CTD(fname = fl[1],
                                full_time_day_local,
                                depths = modeled_depths,
                                input_tz = "EST5EDT",
                                output_tz = reference_tzone)
    
    obs_ctd$obs_do <- obs_ctd$obs_do*1000/32
    for(i in 1:length(full_time_day)){
      if(!is.na(obs_ctd$obs_temp[i, 1])){
        obs_temp$obs[i,] <- obs_ctd$obs_temp[i, ]
        obs_do$obs[i,] <- obs_ctd$obs_do[i, ]
        obs_chla_fdom$Chla_obs[i,] <- obs_ctd$obs_chla[i, ]
      }
    }
    init_pH_obs <- obs_ctd$obs_pH[1, which(!is.na(obs_ctd$obs_pH[1, ]))]
    init_obs_pH_depths <- modeled_depths[which(!is.na(obs_ctd$obs_pH[1, ]))]
    
    init_sal_obs <- obs_ctd$obs_sal[1, which(!is.na(obs_ctd$obs_sal[1, ]))]
    init_obs_sal_depths <- modeled_depths[which(!is.na(obs_ctd$obs_sal[1, ]))]
  }
  
  init_temps_obs <- obs_temp$obs[1, which(!is.na(obs_temp$obs[1, ]))]
  init_obs_temp_depths <- modeled_depths[which(!is.na(obs_temp$obs[1, ]))]
  
  init_do_obs <- obs_do$obs[1, which(!is.na(obs_do$obs[1, ]))]
  init_obs_do_depths <- modeled_depths[which(!is.na(obs_do$obs[1, ]))]
  
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
      do_init <- do_inter(modeled_depths)
      if(length(which(!is.na(init_pH_obs))) > 0){
        pH_inter <- approxfun(init_obs_pH_depths, init_pH_obs, rule=2)
        pH_init <- pH_inter(modeled_depths)
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
      }else{
        par1 <- wq_end[num_wq_vars]
        par2 <- wq_end[num_wq_vars]
        par3 <- wq_end[num_wq_vars]
      }
      
    }
  }else{
    temp_start <- 1
    temp_end <- length(modeled_depths)
    if(npars > 0){
      par1 <- temp_end + 1
      par2 <- par1 + 1
      par3 <-  par2 + 1
    }else{
      par1 <- temp_end
      par2 <- temp_end
      par3 <- temp_end
    }
  }
  
  #UPDATE NML WITH PARAMETERS AND INITIAL CONDITIONS
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
  PHY_CYANOPCH1_init <- 2.0
  PHY_CYANONPCH2_init <-2.0
  PHY_CHLOROPCH3_init <-2.0
  PHY_DIATOMPCH4_init <- 2.0
  ZOO_COPEPODS1_init <- 2.9
  ZOO_DAPHNIABIG2_init <- 4.3
  ZOO_DAPHNIASMALL3_init <- 40
  
  OXY_oxy_error <- OXY_oxy_init*1.0
  CAR_pH_error <- CAR_pH_init*0.001
  CAR_dic_error <- CAR_dic_init*0.001
  CAR_ch4_error <- CAR_ch4_init*0.001
  SIL_rsi_error <- SIL_rsi_init*0.001
  NIT_amm_error <- NIT_amm_init*0.001
  NIT_nit_error <- NIT_nit_init*0.001
  PHS_frp_error <- PHS_frp_init*0.001
  OGM_doc_error <- OGM_doc_init*0.001
  OGM_poc_error <- OGM_poc_init*0.001
  OGM_don_error <- OGM_don_init*0.001
  OGM_pon_error <- OGM_pon_init*0.001
  OGM_dop_error <- OGM_dop_init*0.001
  OGM_pop_error <- OGM_pop_init*0.001
  PHY_CYANOPCH1_error <- PHY_CYANOPCH1_init*0.01
  PHY_CYANONPCH2_error <-PHY_CYANONPCH2_init*0.01
  PHY_CHLOROPCH3_error <-PHY_CHLOROPCH3_init*0.01
  PHY_DIATOMPCH4_error <- PHY_DIATOMPCH4_init*0.01
  ZOO_COPEPODS1_error <- ZOO_COPEPODS1_init*0.001
  ZOO_DAPHNIABIG2_error <- ZOO_DAPHNIABIG2_init*0.001
  ZOO_DAPHNIASMALL3_error <- ZOO_DAPHNIASMALL3_init*0.001
  
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
                    PHY_CYANOPCH1_error,
                    PHY_CYANONPCH2_error,
                    PHY_CHLOROPCH3_error,
                    PHY_DIATOMPCH4_error,
                    ZOO_COPEPODS1_error,
                    ZOO_DAPHNIABIG2_error,
                    ZOO_DAPHNIASMALL3_error)
  
  if(include_wq){
    OXY_oxy_init_depth <- do_init
  }else{
    OXY_oxy_init_depth <- rep(OXY_oxy_init, ndepths_modeled)    
  }
  if(include_wq & use_ctd){
    CAR_pH_init_depth <- pH_init
  }else{
    CAR_pH_init_depth <- rep(CAR_pH_init, ndepths_modeled) 
  }
  
  CAR_dic_init_depth <- rep(CAR_dic_init, ndepths_modeled)
  CAR_ch4_init_depth <- rep(CAR_ch4_init, ndepths_modeled)
  SIL_rsi_init_depth <- rep(SIL_rsi_init, ndepths_modeled)
  NIT_amm_init_depth <- rep(NIT_amm_init, ndepths_modeled)
  NIT_nit_init_depth <- rep(NIT_nit_init, ndepths_modeled)
  PHS_frp_init_depth <- rep(PHS_frp_init, ndepths_modeled)
  OGM_doc_init_depth <- rep(OGM_doc_init, ndepths_modeled)
  OGM_poc_init_depth <- rep(OGM_poc_init, ndepths_modeled)
  OGM_don_init_depth <- rep(OGM_don_init, ndepths_modeled)
  OGM_pon_init_depth <- rep(OGM_pon_init, ndepths_modeled)
  OGM_dop_init_depth <- rep(OGM_dop_init, ndepths_modeled)
  OGM_pop_init_depth <- rep(OGM_pop_init, ndepths_modeled)
  PHY_CYANOPCH1_init_depth <- rep(PHY_CYANOPCH1_init, ndepths_modeled)
  PHY_CYANONPCH2_init_depth <- rep(PHY_CYANONPCH2_init, ndepths_modeled)
  PHY_CHLOROPCH3_init_depth <- rep(PHY_CHLOROPCH3_init, ndepths_modeled)
  PHY_DIATOMPCH4_init_depth <- rep(PHY_DIATOMPCH4_init, ndepths_modeled)
  ZOO_COPEPODS1_init_depth <- rep(ZOO_COPEPODS1_init, ndepths_modeled)
  ZOO_DAPHNIABIG2_init_depth <- rep(ZOO_DAPHNIABIG2_init, ndepths_modeled)
  ZOO_DAPHNIASMALL3_init_depth <- rep(ZOO_DAPHNIASMALL3_init, ndepths_modeled)
  
  #if(full_time_day_local[1] == as.data.frame.POSIXct("201")
  #curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9,10)
  #mg/L
  #curr_values <- c(3.764, 3.781, 3.578, 5.156, 5.2735, 5.5165, 5.222, 5.368)
  #inter <- approxfun(curr_depths,curr_values,rule=2)
  #CAR_dic_init_depth <- inter(modeled_depths)
  
  #curr_depths <- c(0.1,1.6,3.8,5,6.2,8,9,9.5)
  #umol CH4/L
  #curr_values <- c(3.91E-04,0.370572728,0.107597836,0.126096596,
                    #0.088502664,0.086276629,0.07256043,0.07249431)
  #inter <- approxfun(curr_depths,curr_values,rule=2)
  #CAR_ch4_init_depth <- inter(modeled_depths)
  
  #curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9, 10)
  #curr_values <- c(12.65291714,4.213596723,10.5935375,13.43611258,
                    #11.34765394,11.95676704,11.98577285,12.82695814)
  #ug/L
  #inter <- approxfun(curr_depths,curr_values,rule=2)
  #NIT_amm_init_depth <- inter(modeled_depths)
  
  #curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9, 10)
  #curr_values <-c(5.68,3.82,4.46,3.71,4.18,5.08,3.01,7.72)
  #ug/L
  #inter <- approxfun(curr_depths,curr_values,rule=2)
  #NIT_nit_init_depth <- inter(modeled_depths)
  
  #curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9, 10)
  #ug/L
  #curr_values <- c(8.96,7.66,6.26,6.22,7.72,9.69,7.95,10.5)
  #inter <- approxfun(curr_depths,curr_values,rule=2)
  #PHS_frp_init_depth <- inter(modeled_depths)
  
  #curr_depths <- c(0.1,1.6,3.8,5,6.2,8, 9, 10)
  ##mg/L
  #curr_values <- c(4.2315,4.374, 3.2655,2.9705,2.938,2.922,2.773,2.9525)
  #inter <- approxfun(curr_depths,curr_values,rule=2)
  #OGM_poc_init_depth <- inter(modeled_depths)
  
  #}else{
  
  #}
  
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
                    PHY_CYANOPCH1_init_depth,
                    PHY_CYANONPCH2_init_depth,
                    PHY_CHLOROPCH3_init_depth,
                    PHY_DIATOMPCH4_init_depth,
                    ZOO_COPEPODS1_init_depth,
                    ZOO_DAPHNIABIG2_init_depth,
                    ZOO_DAPHNIASMALL3_init_depth)
  
  #UPDATE NML WITH PARAMETERS AND INITIAL CONDITIONS
  if(include_wq){
    update_var(wq_init_vals, "wq_init_vals", working_glm)
    update_var(num_wq_vars, "num_wq_vars", working_glm)
  }else{
    update_var(" ", "wq_init_vals", working_glm)
    update_var(0, "num_wq_vars", working_glm)
  }
  update_var(ndepths_modeled, "num_depths", working_glm)
  update_var(modeled_depths, "the_depths", working_glm)
  update_var(rep(the_sals_init, ndepths_modeled), "the_sals", working_glm)
  
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
    nobs <- length(observed_depths_temp) + length(observed_depths_do) 
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
    z <- cbind(obs_temp$obs, obs_do$obs)
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
  
  ####################################################
  #### STEP 9: CREATE THE QT ARRAY (MODEL VARIANCE)
  ####################################################
  
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
  
  #Covariance matrix for parameters
  qt_pars <- matrix(data = 0, nrow = npars, ncol = npars)
  diag(qt_pars) <- c(zone1temp_init_qt, zone2temp_init_qt, swf_lwf_init_qt)
  
  #######################################################
  #### STEP 10: CREATE THE PSI VECTOR (DATA UNCERTAINITY)  
  #######################################################
  
  psi <- rep(obs_error, length(obs_index))
  
  ################################################################
  #### STEP 11: CREATE THE X ARRAY (STATES X TIME);INCLUDES INITIALATION
  ################################################################
  nmembers <- n_enkf_members*n_met_members
  
  restart_present <- FALSE
  if(!is.na(restart_file)){
    if(file.exists(restart_file)){
      restart_present <- TRUE
    }
  }
  
  x <- array(NA, dim=c(nsteps, nmembers, nstates + npars))

  
  #Initial conditions
  if(!restart_present){
    if(include_wq){
      if(npars > 0){
        x[1, ,1:nstates] <- rmvnorm(n=nmembers, 
                                   mean=c(the_temps_init, 
                                          wq_init_vals), 
                                   sigma=as.matrix(qt))
        x[1, ,(nstates+1):(nstates+npars)] <- rmvnorm(n=nmembers, 
                                                     mean=c(zone1_temp,
                                                            zone2_temp,
                                                            swf_lwf_init),
                                                     sigma = as.matrix(qt_pars))
        if(initial_condition_uncertainity == FALSE){
          for(m in 1:nmembers){
            x[1,m, ] <- c(the_temps_init,
                         wq_init_vals,
                         zone1_temp,
                         zone2_temp,
                         swf_lwf_init)
          }
        }
      }else{
        x[1, , ] <- rmvnorm(n=nmembers, 
                          mean=c(the_temps_init,wq_init_vals),
                          sigma=as.matrix(qt))
        if(initial_condition_uncertainity == FALSE){
          for(m in 1:nmembers){
            x[1, m, ] <- c(the_temps_init, do_init, wq_init_vals)
          }
        }
      }
    }else{
      if(npars > 0){
        x[1, ,1:nstates] <- rmvnorm(n=nmembers, 
                                   mean=the_temps_init,
                                   sigma=as.matrix(qt))
        x[1, ,(nstates+1):(nstates+npars)] <- rmvnorm(n=nmembers, 
                                                     mean=c(zone1_temp,
                                                            zone2_temp,
                                                            swf_lwf_init),
                                                     sigma = as.matrix(qt_pars))
        if(initial_condition_uncertainity == FALSE){
          for(m in 1:nmembers){
            x[1, m, ] <- c(the_temps_init, zone1_temp, zone2_temp, swf_lwf_init)
          }
        }
      }else{
        x[1, , ] <- rmvnorm(n=nmembers, 
                          mean=the_temps_init,
                          sigma=as.matrix(qt))
        
        if(initial_condition_uncertainity == FALSE){
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
          index <- which(x[1, m, ] < 0.0)
          index <- index[which(index > wq_start[1])]
          x[1, m, index] <- 0.0
        }
      }
    }
    write.csv(x[1, , ],paste0(working_glm, "/", "restart_",
                            year(full_time[1]), "_",
                            month(full_time[1]), "_",
                            day(full_time[1]), "_cold.csv"),
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
      if(initial_condition_uncertainity == FALSE & hist_days == 0){
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
      if(initial_condition_uncertainity == FALSE & hist_days == 0){
        x_previous_1 <- colMeans(x_previous)
        for(m in 1:nmembers){
          x_previous[m, ] <- x_previous_1
        }
      }
    }else{
      x_previous <- ncvar_get(nc, "x_restart")   
      if(initial_condition_uncertainity == FALSE & hist_days == 0){
        x_previous_1 <- colMeans(x_previous) 
        for(m in 1:nmembers){
          x_previous[m, ] <- x_previous_1
        }
      }
    }
    nc_close(nc)
  }else{
    x_previous <- read.csv(paste0(working_glm, "/", "restart_",
                                  year(full_time[1]), "_",
                                  month(full_time[1]), "_",
                                  day(full_time[1]), "_cold.csv"))
  }
  
  #Set initial conditions
  x[1,,] <- as.matrix(x_previous)
  
  #Matrix to store essemble specific surface height
  surface_height <- array(NA, dim=c(nsteps, nmembers))
  surface_height[1, ] <- lake_depth_init
  
  ####################################################
  #### STEP 11: Run Ensemble Kalman Filter
  ####################################################
  
  enkf_output <- GLM_EnKF(x,
                          z,
                          qt,
                          qt_pars,
                          psi,
                          full_time,
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
                          alpha,
                          glm_output_vars,
                          weather_uncertainity,
                          process_uncertainity,
                          initial_condition_uncertainity,
                          parameter_uncertainity)
  
  x <- enkf_output$x
  x_restart <- enkf_output$x_restart
  qt_restart <- enkf_output$qt_restart
  x_prior <- enkf_output$x_prior
  
  ####################################################
  #### STEP 12: PROCESS OUTPUT
  ####################################################
  
  ### CREATE FORECAST NAME
  
  if(forecast_days >0){
    save_file_name <- paste0(sim_name, "_hist_",
                             year(full_time[1]), "_",
                             month(full_time[1]), "_",
                             day(full_time[1]), "_forecast_",
                             year(full_time[hist_days+1]), "_",
                             month(full_time[hist_days+1]), "_",
                             day(full_time[hist_days+1]))
  }else{
    save_file_name <- paste0(sim_name, "_hist_",
                             year(full_time[1]), "_",
                             month(full_time[1]), "_",
                             day(full_time[1]))    
  }
  
  time_of_forecast <- Sys.time()
  time_of_forecast_string <- paste0(year(Sys.time()),
                                    month(Sys.time()),
                                    day(Sys.time()), "_",
                                    hour(Sys.time()), "_",
                                    (minute(Sys.time())))
  
  ###SAVE FORECAST
  write_forecast_netcdf(x = x,
                        full_time = full_time_local,
                        qt = qt,
                        modeled_depths = modeled_depths,
                        save_file_name = save_file_name,
                        x_restart = x_restart,
                        qt_restart = qt_restart,
                        time_of_forecast = time_of_forecast,
                        hist_days = hist_days,
                        x_prior,
                        include_wq,
                        wq_start,
                        wq_end,
                        par1,
                        par2,
                        par3,
                        z,
                        nstates,
                        npars)
  
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
