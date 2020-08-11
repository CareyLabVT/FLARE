# --------------------------------------
# purpose: run downscaling processes
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: this calls all other functions required to downscale future GEFS forecasts to a specific site using the relationship between saved forecasts and site observations
# --------------------------------------
process_downscale_GEFS <- function(folder,
                                   noaa_location,
                                   input_met_file,
                                   working_directory,
                                   n_ds_members,
                                   n_met_members,
                                   file_name,
                                   local_tzone,
                                   FIT_PARAMETERS,
                                   DOWNSCALE_MET,
                                   met_downscale_uncertainty,
                                   compare_output_to_obs,
                                   VarInfo,
                                   replaceObsNames,
                                   downscaling_coeff,
                                   full_time_local,
                                   first_obs_date,
                                   last_obs_date,
                                   input_met_file_tz,
                                   weather_uncertainty){
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
  
  obs.file.path = input_met_file
  for.file.path = noaa_location
  
  obs.data <- read_csv(obs.file.path, col_types = cols())
  
  obs.data$time <- with_tz(obs.data$time, tz = local_tzone)
  
  VarNames = as.vector(VarInfo$VarNames)
  
  observations <- obs.data %>% 
    dplyr::mutate(AirTemp = AirTemp + 273.15,# convert from C to Kelvin
                  Rain = Rain* 60 * 24/1000)

  rm(obs.data)
  hrly.obs <- observations #%>% aggregate_obs_to_hrly()
  
  # -----------------------------------
  # 1. Fit Parameters
  # -----------------------------------
  
  if(FIT_PARAMETERS){
    print("Fit Parameters")
    fit_downscaling_parameters(observations,
                               for.file.path = noaa_location,
                               working_directory,
                               VarNames,
                               VarNamesStates,
                               replaceObsNames,
                               PLOT = FALSE,
                               local_tzone,
                               VarInfo,
                               first_obs_date,
                               last_obs_date)
  }
  # -----------------------------------
  # 2. Process GEFS
  # -----------------------------------
  forecasted_met <- process_GEFS(file_name,
                       n_ds_members,
                       n_met_members,
                       in_directory = noaa_location,
                       out_directory = working_directory,
                       local_tzone,
                       VarInfo,
                       replaceObsNames,
                       hrly.observations = hrly.obs,
                       DOWNSCALE_MET,
                       FIT_PARAMETERS,
                       met_downscale_uncertainty,
                       WRITE_FILES = TRUE,
                       downscaling_coeff,
                       full_time_local,
                       weather_uncertainty)

  #if(compare_output_to_obs == TRUE){
  #  "comparing forecast output to obs"
  #  compare_output_to_obs(output, hrly.obs)
  #}
  return(forecasted_met)
}



