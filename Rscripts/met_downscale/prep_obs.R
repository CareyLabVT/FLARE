prep_obs <- function(obs.data, output_tz){
  # --------------------------------------
  # purpose: convert observations dataframe to format/units for comparison with forecasts
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  
  obs.units.match <- obs.data %>%
    plyr::rename(replaceObsNames) %>%
    dplyr::mutate(TIMESTAMP = as.character(TIMESTAMP)) %>%
    dplyr::mutate_at(VarNames, as.numeric) %>%
    dplyr::mutate(timestamp = as_datetime(TIMESTAMP,
                                          tz = output_tz)) %>%                                      
    dplyr::mutate(AirTemp = AirTemp + 273.15) %>% # convert from C to Kelvin
  select(timestamp, VarNames)

return(obs.units.match)
}
