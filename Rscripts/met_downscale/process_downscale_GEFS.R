# --------------------------------------
# purpose: run downscaling processes
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: this calls all other functions required to downscale future GEFS forecasts to a specific site using the relationship between saved forecasts and site observations
# --------------------------------------
process_downscale_GEFS <- function(folder,
                                   noaa_location,
                                   met_station_location,
                                   working_glm,
                                   sim_files_folder,
                                   n_ds_members,
                                   n_met_members,
                                   file_name,
                                   output_tz,
                                   FIT_PARAMETERS,
                                   DOWNSCALE_MET,
                                   met_downscale_uncertainty,
                                   compare_output_to_obs,
                                   VarInfo,
                                   replaceObsNames,
                                   downscaling_coeff,
                                   full_time_local,
                                   first_obs_date,
                                   last_obs_date){
  # -----------------------------------
  # 0. Source necessary files
  # -----------------------------------
  
  path.met.ds.folder <- paste0(folder,"/","Rscripts/met_downscale/")
  
  for(f in list.files(path = path.met.ds.folder, pattern="*.R")){
    if(f != "process_downscale_GEFS.R" & f != "run_met_downscaling.R"& f != "main_downscaling.R"){
    source(paste0(path.met.ds.folder, f))
    }
  }
  
  # -----------------------------------
  # 1. Load & reformat observational data
  # -----------------------------------
  
  obs.file.path = paste(met_station_location, "/FCRmet.csv", sep = "")
  for.file.path = noaa_location
  
  obs.data <- read.csv(obs.file.path, skip = 4, header = F)
  d_names <- read.csv(obs.file.path, skip = 1, header = T, nrows = 1)
  names(obs.data) <- names(d_names)
  
  VarNames = as.vector(VarInfo$VarNames)
  
  maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
  minTempC = -24 # an lower bound of realistic temperature for the study site in deg C
  observations <- obs.data %>% 
    plyr::rename(replaceObsNames) %>%
    dplyr::mutate(TIMESTAMP = as.character(TIMESTAMP)) %>%
    dplyr::mutate_at(VarNames, as.numeric) %>%
    dplyr::mutate(timestamp = as_datetime(TIMESTAMP,
                                          tz = 'EST5EDT')) %>%
    dplyr::mutate(AirTemp = AirTemp + 273.15,# convert from C to Kelvin
                  Rain = Rain* 60 * 24/1000) %>% # convert from mm to m
    dplyr::select(timestamp, VarNames)
  observations$timestamp <- with_tz(observations$timestamp, output_tz)
  observations <- observations %>%
    dplyr::mutate(ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
                  RelHum = ifelse(RelHum <0, 0, RelHum),
                  RelHum = ifelse(RelHum > 100, 100, RelHum),
                  AirTemp = ifelse(AirTemp> 273.15 + maxTempC, NA, AirTemp),
                  AirTemp = ifelse(AirTemp < 273.15 + minTempC, NA, AirTemp),
                  LongWave = ifelse(LongWave < 0, NA, LongWave),
                  WindSpeed = ifelse(WindSpeed <0, 0, WindSpeed)) %>%
                  filter(is.na(timestamp) == FALSE)

  observations$RelHum <- na.interpolation(observations$RelHum)
  observations$AirTemp <- na.interpolation(observations$AirTemp)
  observations$LongWave <- na.interpolation(observations$LongWave)
  observations$WindSpeed <- na.interpolation(observations$WindSpeed)
  
  rm(obs.data)
  hrly.obs <- observations %>% aggregate_obs_to_hrly()
  
  # -----------------------------------
  # 1. Fit Parameters
  # -----------------------------------
  
  if(FIT_PARAMETERS){
    print("Fit Parameters")
    fit_downscaling_parameters(observations,
                               for.file.path = noaa_location,
                               working_glm,
                               VarNames = VarNames,
                               VarNamesStates,
                               replaceObsNames = replaceObsNames,
                               PLOT = FALSE,
                               output_tz = output_tz,
                               VarInfo,
                               first_obs_date,
                               last_obs_date)
  }
  # -----------------------------------
  # 2. Process GEFS
  # -----------------------------------
  met_forecast_output = process_GEFS(file_name = file_name,
                       n_ds_members = n_ds_members,
                       n_met_members = n_met_members,
                       sim_files_folder = sim_files_folder,
                       in_directory = noaa_location,
                       out_directory = working_glm,
                       output_tz = output_tz,
                       VarInfo = VarInfo,
                       replaceObsNames = replaceObsNames,
                       hrly.observations = hrly.obs,
                       DOWNSCALE_MET = DOWNSCALE_MET,
                       FIT_PARAMETERS = FIT_PARAMETERS,
                       met_downscale_uncertainty,
                       WRITE_FILES = TRUE,
                       downscaling_coeff)
  files = met_forecast_output[[1]]
  output = met_forecast_output[[2]]
  if(compare_output_to_obs == TRUE){
    "comparing forecast output to obs"
    compare_output_to_obs(output, hrly.obs)
  }
  return(files)
}



