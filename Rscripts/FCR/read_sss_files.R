read_sss_files <-  function(full_time_day_local,
                            working_directory,
                            input_file_tz = 'EST5EDT', 
                            sss_file,
                            local_tzone){
  
  d <- read.csv(sss_file)
  
  TIMESTAMP_in <- as.POSIXct(d$time, 
                             format= "%Y-%m-%d",
                             tz = input_file_tz)
  
  d$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
  
  
  sss_flow <- rep(0, length(full_time_day_local))
  sss_OXY_oxy <- rep(0, length(full_time_day_local))
  
  if(length(which(d$TIMESTAMP == full_time_day_local[1])) > 0){
    
    for(i in 1:(length(full_time_day_local))){
      index <- which(d$TIMESTAMP == full_time_day_local[i])
      if(length(index) > 0){
      sss_flow[i] <- d[index, "FLOW"]
      sss_OXY_oxy[i] <- d[index, "OXY_oxy"]
      }
    }
  }
  
  management_input <- data.frame(sss_flow = sss_flow, sss_OXY_oxy = sss_OXY_oxy)
  
  return(management_input)
  
}
