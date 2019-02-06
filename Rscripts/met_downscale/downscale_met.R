# --------------------------------------
# purpose: downscale GEFS forecast to specific site & hr-resolution
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: this function spatially downscaled forecasts from GEFS cell size to the specific site location and temporally downscaled from 6-hr resolution to hr-resolution using saved parameters from earlier fitting process (fit_downscaling_parameters.R)
# --------------------------------------

downscale_met <- function(forecasts, debiased.coefficients, VarInfo, PLOT, output_tz){
  # -----------------------------------
  # 0. summarize forecasts to ensemble mean if USE_ENSEMBLE_MEAN is TRUE
  # -----------------------------------
  
  time0 = min(forecasts$timestamp)
  tzone = attributes(forecasts$timestamp)$tzone
  forecasts <- forecasts %>%
    dplyr::mutate(fday = as.numeric(difftime(timestamp, time0))/(24*60*60),
                  fday.group = as.integer(fday + 0.75),
                  fday.group = ifelse(timestamp == time0, 0, fday.group))
  # -----------------------------------
  # 1. aggregate forecasts and observations to daily resolution
  # -----------------------------------
  
  daily.forecast = aggregate_to_daily(forecasts) %>%
    select(-date) # %>% filter(fday.group > 0)) 
  
  # -----------------------------------
  # 2. load saved parameters and spatially debias at daily scale
  # -----------------------------------
  
  debiased <- daily_debias_from_coeff(daily.forecast, debiased.coefficients, VarInfo)
  
  # -----------------------------------
  # 3.a. temporal downscaling step (a): redistribute to 6-hourly resolution
  # -----------------------------------
  redistributed = daily_to_6hr(forecasts, daily.forecast, debiased, VarNames = VarInfo$VarNames)
  
  # -----------------------------------
  # 3.b. temporal downscaling step (b): temporally downscale from 6-hourly to hourly
  # -----------------------------------
  
  ## downscale states to hourly resolution (air temperature, relative humidity, average wind speed) 
  VarNamesStates = VarInfo %>%
    filter(VarType == "State")
  VarNamesStates = VarNamesStates$VarNames
  states.ds.hrly = spline_to_hourly(redistributed,
                                    VarNamesStates = VarNamesStates)
  # if filtering out incomplete days, that would need to happen here
  
  VarNames_6hr = VarInfo %>%
    filter(ds_res == "6hr")
  VarNames_6hr = VarNames_6hr$VarNames
  
  ## convert longwave to hourly (just copy 6 hourly values over past 6-hour time period)
  nonSW.flux.hrly <- redistributed %>%
    select(timestamp, NOAA.member, VarNames_6hr) %>%
    repeat_6hr_to_hrly()
  
  ## downscale shortwave to hourly
  ShortWave.ds = ShortWave_to_hrly(debiased, time0, lat = 37.307, lon = 360 - 79.837, output_tz)
  
  # -----------------------------------
  # 4. join debiased forecasts of different variables into one dataframe
  # -----------------------------------
  
  joined.ds <- full_join(states.ds.hrly, ShortWave.ds, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
    full_join(nonSW.flux.hrly, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
    filter(timestamp >= min(forecasts$timestamp) & timestamp <= max(forecasts$timestamp))
  
  return(joined.ds)
  
}

