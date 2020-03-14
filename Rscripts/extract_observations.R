extract_observations <- function(fname,
                               full_time_local,
                               modeled_depths = modeled_depths,
                               local_tzone,
                               target_variable,
                               distance_threshold_meter,
                               time_threshold_seconds,
                               methods){
  
  
  d <- read_csv(fname)
  d$timestamp <- force_tz(d$timestamp, tzone = local_tzone)
  
  obs <- array(NA,dim=c(length(full_time_local),length(modeled_depths)))

  for(i in 1:length(full_time_local)){
    for(j in 1:length(modeled_depths)){
      d1 <- d %>% 
        filter(abs(difftime(timestamp, full_time_local[i], units = "secs")) < time_threshold_seconds &
                 abs(depth-modeled_depths[j]) < distance_threshold_meter &
               variable == target_variable,
               method %in% methods) %>% 
        summarize(value = mean(value)) %>% 
        mutate(value = ifelse(is.na(value),NA, value),
               value = ifelse(is.nan(value),NA, value))
      obs[i,j] <- d1$value
    }
  }
  
  return(obs)
}