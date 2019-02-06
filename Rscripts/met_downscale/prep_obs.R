prep_obs <- function(obs.data, output_tz, replaceObsNames, VarNames){
  # --------------------------------------
  # purpose: convert observations dataframe to format/units for comparison with forecasts
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  
  observations <- obs.data %>%
    plyr::rename(replaceObsNames) %>%
    dplyr::mutate(TIMESTAMP = as.character(TIMESTAMP)) %>%
    dplyr::mutate_at(VarNames, as.numeric) %>%
    dplyr::mutate(timestamp = as_datetime(TIMESTAMP,
                                          tz = 'EST5EDT')) %>%
    dplyr::mutate(AirTemp = AirTemp + 273.15,# convert from C to Kelvin
                  Rain = Rain* 60 * 24/1000) %>% # convert from mm to m
  select(timestamp, VarNames)
  observations$timestamp <- with_tz(observations$timestamp, output_tz)

return(observations)
}
