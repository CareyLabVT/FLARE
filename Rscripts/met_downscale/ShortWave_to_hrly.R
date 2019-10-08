ShortWave_to_hrly <- function(debiased, time0, lat, lon, local_tzone){
  ## downscale shortwave to hourly
  
  grouping = "NOAA.member"
  if("fday.group" %in% colnames(debiased)){
    grouping = append(grouping, "fday.group")
    
    ShortWave.hours <- debiased %>%
      dplyr::mutate(fday = fday.group) %>%
      unique() %>%
      group_by_at(grouping) %>%
      expand(fday = seq((fday.group - 23/24), fday.group, by = 1/24)) %>%# the days since start of forecast 
      ungroup() %>%
      filter(fday > -1/24) %>% # (rounding error causes the first "fday" to be -1e-16 instead of 0)
      dplyr::mutate(timestamp = as_datetime(time0 + fday*24*60*60, tz = local_tzone)) %>%
      # filter(timestamp >= time0 + 6*60*60) %>%
      dplyr::mutate(date = as_date(timestamp),
                    hour = hour(timestamp))
    
  }else{
    grouping = append(grouping, "date")
    ShortWave.hours <- debiased %>%
      dplyr::group_by_at(grouping) %>%
      tidyr::expand(hour = 0:23)
  }
  
  ShortWave.ds <- debiased %>% 
    select(ShortWave, grouping) %>%
    full_join(ShortWave.hours, by = grouping) %>%
    dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = local_tzone) - 1*60*60) %>% # subtract one hour to convert times from representing pervious hour to representing the next hour
    dplyr::mutate(doy = yday(date) + hour/24) %>%
    dplyr::mutate(rpot = solar_geom(doy, lon, lat)) %>% # hourly sw flux calculated using solar geometry
    dplyr::group_by_at(grouping) %>%
    dplyr::mutate(avg.rpot = mean(rpot, na.rm = TRUE)) %>% # daily sw mean from solar geometry
    ungroup() %>%
    dplyr::mutate(ShortWave = ifelse(avg.rpot > 0, ShortWave * (rpot/avg.rpot),0)) %>%
    select(timestamp, NOAA.member, ShortWave)
}