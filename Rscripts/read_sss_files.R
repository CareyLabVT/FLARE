read_sss_files <-  function(full_time_local,
                            sss_file){
  
  full_time_day_local <- as_date(full_time_local)
  
  d <- read_csv(sss_file, col_type = cols(
    time = col_date(format = ""),
    FLOW = col_double(),
    OXY_oxy = col_double())
  )
  
  sss_flow <- rep(0, length(full_time_day_local))
  sss_OXY_oxy <- rep(0, length(full_time_day_local))
  
  if(length(which(d$time == full_time_day_local[1])) > 0){
    
    for(i in 1:(length(full_time_day_local))){
      index <- which(d$time == full_time_day_local[i])
      if(length(index) > 0){
        sss_flow[i] <- unlist(d[index, "FLOW"])
        sss_OXY_oxy[i] <- unlist(d[index, "OXY_oxy"])
      }
    }
  }
  
  management_input <- data.frame(sss_flow = sss_flow, sss_OXY_oxy = sss_OXY_oxy)

  return(management_input)
  
}
