extract_nutrients <- function(fname,
                        full_time_day_local,
                        modeled_depths,
                        input_file_tz,
                        local_tzone){
  
  d <- read.csv(fname)
  d_fcr <- d[which(d$Reservoir == 'FCR' & d$Site == '50'),]
 
  obs_TN <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_TP <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_NH4 <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_NO3NO2 <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_SRP <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  obs_DOC <- array(NA,dim=c(length(full_time_day_local),length(modeled_depths)))
  
  TIMESTAMP_in <- as_datetime(d_fcr$DateTime,tz = input_file_tz)
  TIMESTAMP_out <- with_tz(TIMESTAMP_in,tz = local_tzone)
  d_fcr_day <- as_date(TIMESTAMP_out)
  
  for(i in 1:length(full_time_day_local)){
    index1 = which(d_fcr_day==full_time_day_local[i])
    if(length(index1)>0){
      curr_day <- d_fcr[index1,]
      for(j in 1:length(modeled_depths)){
        
        depth_diff <- abs(curr_day$Depth_m -modeled_depths[j])
        if(j == length(modeled_depths)){
        model_depth_diff <- abs(modeled_depths[j] - modeled_depths[j-1])
        }else if(j == 1){
          model_depth_diff <- abs(modeled_depths[j] - modeled_depths[j+1])
        }else{
          model_depth_diff <- min(c(abs(modeled_depths[j] - modeled_depths[j+1]),
                                    abs(modeled_depths[j] - modeled_depths[j-1])))
        }
        
        index2 <- which(depth_diff < 0.3 & depth_diff < model_depth_diff)
        if(length(index2) == 1){
        obs_TN[i,j] <- curr_day$TN_ugL[index2]
        obs_TP[i,j] <- curr_day$TP_ugL[index2]
        obs_NH4[i,j] <- curr_day$NH4_ugL[index2]
        obs_NO3NO2[i,j] <- curr_day$NO3NO2_ugL[index2]
        obs_SRP[i,j] <- curr_day$SRP_ugL[index2]
        obs_DOC[i,j] <- curr_day$DOC_mgL[index2]
        }
      }
    }
  }
  
  #Unit Conversion and other calculations
  obs_TN <- obs_TN * 1000 * 0.001 * (1/14)
  obs_TP <- obs_TP * 1000 * 0.001 * (1/30.97)
  obs_NH4 <- obs_NH4 * 1000 * 0.001 * (1 / 18.04)
  obs_NO3 <- obs_NO3NO2 * 1000 * 0.001 * (1/62.00)
  obs_SRP <- obs_SRP * 1000 * 0.001 * (1/94.9714)
  obs_DOC <- obs_DOC * 1000 * (1/12.01)
    
  return(list(NH4 = obs_NH4, NO3 = obs_NO3, SRP = obs_SRP, DOC = obs_DOC))
}
