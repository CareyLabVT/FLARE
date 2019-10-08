extract_CTD <- function(fname,
                        full_time_day_local,
                        modeled_depths,
                        input_file_tz,
                        local_tzone){
  
  d <- read.csv(fname)
  if(length(d[which(d$Reservoir == 'FCR' & d$Site == '50'),1])>0){
    d_fcr <- d[which(d$Reservoir == 'FCR' & d$Site == '50'),]
  }else{
    d_fcr <- d
  }
  d_fcr_day <- as_date(d_fcr$Date)
  obs_temp <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_chla <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_do <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_pH <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_sal <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  
  TIMESTAMP_in <- as_datetime(paste0(d_fcr_day, '12:00:00'),tz = input_file_tz)
  TIMESTAMP_out <- with_tz(TIMESTAMP_in,tz = local_tzone)
  d_fcr_day <- as_date(TIMESTAMP_out)
  
  for(i in 1:length(full_time_day_local)){
    index1 = which(d_fcr_day==full_time_day_local[i])
    if(length(index1)>0){
      curr_day <- d_fcr[index1,]
      for(j in 1:length(modeled_depths)){
        index2 <- which.min(abs(curr_day$Depth_m -modeled_depths[j]))
        obs_temp[i,j] <- curr_day$Temp_C[index2]
        obs_chla[i,j] <- curr_day$Chla_ugL[index2]
        obs_do[i,j] <- curr_day$DO_mgL[index2]
        obs_pH[i,j] <- curr_day$pH[index2]
        #obs_sal[i,j] <- curr_day$Salinity[index2]
      }
    }
  }
  
  obs_do <- obs_do*1000/32
  
  obs_chla <- ctd_2_exo_chla[1] + ctd_2_exo_chla[2] * obs_chla
  
  obs_chla <- obs_chla * biomass_to_chla
  
  return(list(obs_temp = obs_temp,obs_chla = obs_chla, obs_do= obs_do, obs_pH = obs_pH,obs_sal= obs_sal, depths = modeled_depths))
}
