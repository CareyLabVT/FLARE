extract_temp_CTD <- function(fname,full_time_day = full_time_day_local,depths,input_tz = 'EST5EDT', output_tz = reference_tzone){
  
  d <- read.csv(fname)
  if(length(d[which(d$Reservoir == 'FCR' & d$Site == '50'),1])>0){
  d_fcr <- d[which(d$Reservoir == 'FCR' & d$Site == '50'),]
  }else{
    d_fcr <- d
  }
  d_fcr_day <- as.POSIXct(strftime(d_fcr$Date, format="%Y-%m-%d"))
  obs_temp <- array(NA,dim=c(length(full_time_day),length(depths)))
  obs_chla <- array(NA,dim=c(length(full_time_day),length(depths)))
  obs_do <- array(NA,dim=c(length(full_time_day),length(depths)))
  obs_pH <- array(NA,dim=c(length(full_time_day),length(depths)))
  obs_sal <- array(NA,dim=c(length(full_time_day),length(depths)))
    
  TIMESTAMP_in <- as.POSIXct(paste0(d_fcr$Date, '12:00:00)'),origin = '1970-01-01 00:00.00 UTC',tz = input_tz)
  TIMESTAMP_out <- as.POSIXct(TIMESTAMP_in,tz = output_tz)
  d_fcr_day <- as.POSIXct(strftime(TIMESTAMP_out, format="%Y-%m-%d"))

  for(i in 1:length(full_time_day)){
    index1 = which(d_fcr_day==full_time_day[i])
    if(length(index1)>0){
      curr_day <- d_fcr[index1,]
      for(j in 1:length(depths)){
        index2 <- which.min(abs(curr_day$Depth_m -depths[j]))
        obs_temp[i,j] <- curr_day$Temp_C[index2]
        obs_chla[i,j] <- curr_day$Chla_ugL[index2]
        obs_do[i,j] <- curr_day$DO_mgL[index2]
        obs_pH[i,j] <- curr_day$pH[index2]
        obs_sal[i,j] <- curr_day$Salinity[index2]
      }
    }
  }
  
  return(list(obs_temp = obs_temp,obs_chla = obs_chla, obs_do= obs_do, obs_pH = obs_pH,obs_sal= obs_sal, depths = depths))
}