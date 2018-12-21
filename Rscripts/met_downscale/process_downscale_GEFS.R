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
                                   ADD_NOISE){
  # -----------------------------------
  # 0. Source necessary files
  # -----------------------------------
  path.working <- paste0(folder,"/","Rscripts/met_downscale/")
  setwd(path.working)
  # used for all scenarios
  source('process_GEFS.R')
  source('repeat_6hr_to_hrly.R')
  source("prep_for.R")
  # only used if FIT_PARAMETERS is TRUE
  source('process_saved_forecasts.R') 
  source('get_daily_debias_coeff.R')
  source('fit_downscaling_parameters.R')
  source('prep_obs.R')
  # only used if DOWNSCALE_MET is TRUE
  source('downscale_met.R') 
  source('daily_debias_from_coeff.R')
  source('spline_to_hourly.R')
  source('solar_geom.R')
  source('ShortWave_to_hrly.R')
  source('aggregate_to_daily.R')
  source('daily_to_6hr.R')
  # only used if DOWNSCALE_MET is TRUE and ADD_NOISE is TRUE
  source('add_noise.R')
  # only used if DOWNSCALE_MET is FALSE
  source('out_of_box.R')
  
  source('compare_output_to_obs.R')
  source('check_CI.R')
  source('aggregate_obs_to_hrly.R')
  
  # library(imputeTS) # for out-of-box
  # library(stringr) # for out-of-box
  # library(lubridate)
  library(tidyr)
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  
  # -----------------------------------
  # 1. Setup
  # -----------------------------------
  
  in_directory = noaa_location
  out_directory = working_glm
  obs.file.path = paste(met_station_location, "/FCRmet.csv", sep = "")
  for.file.path = noaa_location
  start_date = "2018-11-18"
  VarNames = c("AirTemp",
               "WindSpeed",
               "RelHum",
               "ShortWave",
               "LongWave")
  VarNamesStates = c("AirTemp",
                     "WindSpeed",
                     "RelHum")
  
  if(FIT_PARAMETERS){
    replaceObsNames = c("AirTC_Avg" = "AirTemp",
                        "WS_ms_Avg" = "WindSpeed",
                        "RH" = "RelHum",
                        "SR01Up_Avg" = "ShortWave",
                        "IR01UpCo_Avg" = "LongWave")
  }
  
  output_tz = "US/Eastern" 
  nmembers = n_ds_members # members for downscaled ensembles
  
  # -----------------------------------
  # 1. Fit Parameters
  # -----------------------------------
  
  if(FIT_PARAMETERS){
    fit_downscaling_parameters(obs.file.path = obs.file.path,
                               for.file.path = for.file.path,
                               VarNames,
                               VarNamesStates,
                               USE_ENSEMBLE_MEAN = FALSE,
                               PLOT = TRUE)
  }
  
  # -----------------------------------
  # 2. Process GEFS
  # -----------------------------------
  files = process_GEFS(file_name, n_ds_members, n_met_members, sim_files_folder, in_directory, out_directory, output_tz, VarNames, VarNamesStates, DOWNSCALE_MET, FIT_PARAMETERS, ADD_NOISE, WRITE_FILES = TRUE)[[1]]
  return(files)
}



