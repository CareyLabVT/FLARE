extract_observations <- function(fname,
                               full_time_local,
                               modeled_depths = modeled_depths,
                               local_tzone,
                               target_variable,
                               distance_threshold_meter,
                               time_threshold_seconds,
                               methods){
  
  col_types <- cols(
    timestamp = col_datetime(format = ""),
    depth = col_double(),
    value = col_double(),
    variable = col_character(),
    method = col_character())
  
  d <- read_csv(fname,
                col_types = col_types)
  
  d$timestamp <- force_tz(d$timestamp, tzone = local_tzone)
  
  obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))
  
  d <- d %>% 
    filter(variable == target_variable,
           method %in% methods,
           (timestamp >= first(full_time_local) - time_threshold_seconds),
           (timestamp <= last(full_time_local) + time_threshold_seconds))
  
  for(i in 1:length(full_time_local)){
    for(j in 1:length(modeled_depths)){
      d1 <- d %>% 
        filter(abs(difftime(timestamp, full_time_local[i], units = "secs")) < time_threshold_seconds &
                 abs(depth-modeled_depths[j]) < distance_threshold_meter) %>% 
        summarize(value = mean(value, na.rm = TRUE)) %>% 
        mutate(value = ifelse(is.na(value),NA, value),
               value = ifelse(is.nan(value),NA, value))
      obs[i,j] <- d1$value
    }
  }
  
  return(obs)
}