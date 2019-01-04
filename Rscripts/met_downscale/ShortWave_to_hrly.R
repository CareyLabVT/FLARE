ShortWave_to_hrly <- function(debiased, lat, lon, output_tz){
  ## downscale shortwave to hourly
  
  ShortWave.hours <- debiased %>%
    dplyr::group_by(NOAA.member, date) %>%
    tidyr::expand(hour = 0:23)
  
  ShortWave.ds <- debiased %>% 
    select(ShortWave, NOAA.member, date) %>%
    dplyr::group_by(NOAA.member, date) %>%
    full_join(ShortWave.hours, by = c("NOAA.member","date")) %>%
    ungroup() %>%
    dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = output_tz)) %>%
    dplyr::mutate(doy = yday(date) + hour/24) %>%
    dplyr::mutate(rpot = solar_geom(doy, lon, lat)) %>% # hourly sw flux calculated using solar geometry
    dplyr::group_by(date) %>%
    dplyr::mutate(avg.rpot = mean(rpot)) %>% # daily sw mean from solar geometry
    ungroup() %>%
    dplyr::mutate(ShortWave = ifelse(avg.rpot > 0, ShortWave * (rpot/avg.rpot),0)) %>%
    select(timestamp, NOAA.member, ShortWave)
}