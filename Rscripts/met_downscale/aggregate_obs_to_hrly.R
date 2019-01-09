aggregate_obs_to_hrly <- function(observations){
  obs.tz = attributes(observations$timestamp)$tzone
  hrly.flux.obs <- observations %>%
    dplyr::mutate(date = date(timestamp)) %>%
    dplyr::mutate(hour = hour(timestamp)) %>%
    dplyr::group_by(date, hour) %>%
    dplyr::summarize(ShortWave = mean(ShortWave, na.rm = FALSE),
                     LongWave = mean(LongWave, na.rm = FALSE)) %>%
    ungroup() %>%
    dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = obs.tz) + 60*60) %>% # add one hour so that timestamp represents average over past hour
    select(timestamp, ShortWave, LongWave)
  
  
  hrly.state.obs <- observations %>% group_by(timestamp) %>%
    dplyr::summarize(AirTemp = mean(AirTemp, na.rm = FALSE),
                     RelHum = mean(RelHum, na.rm = FALSE),
                     WindSpeed = mean(WindSpeed, na.rm = FALSE)) %>%
    ungroup()
  hrly.obs = inner_join(hrly.flux.obs, hrly.state.obs, by = "timestamp")
  return(hrly.obs)
}