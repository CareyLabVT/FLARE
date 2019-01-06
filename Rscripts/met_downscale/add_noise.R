# --------------------------------------
# purpose: create ensemble members of downscaling noise
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: creates ensemble members with noise addition (random sample from normal distribution with standard deviation equal to saved standard deviation of residuals from downscaling process.) For all variables except Shortwave, this is the standard deviaiton of the residuals after downscaling to the hourly resolution. For Shortwave, is is the value after downscaling to the daily resolution to artificually high values that result from noise in observational data from hour to hour.
# --------------------------------------

add_noise <- function(debiased, cov, n_ds_members, n_met_members, VarNames){
  
  if("timestamp" %in% colnames(debiased)){
    with.noise <- debiased %>%
      group_by(timestamp, NOAA.member, VarNames) %>%
      expand(dscale.member = 1:n_ds_members) %>%
      dplyr::mutate(ShortWaveOld = ShortWave) %>%
      ungroup()
  }else{
    with.noise <- debiased %>%
      group_by(date, NOAA.member, VarNames) %>%
      expand(dscale.member = 1:n_ds_members) %>%
      dplyr::mutate(ShortWaveOld = ShortWave) %>%
      ungroup()
    
  }
  
  # add option for covariance vs non covariance
  
  for(NOAA.ens in 1:n_met_members){
    for(dscale.ens in 1:n_ds_members){
      noise = rmvnorm(1, mean = c(0,0,0,0,0), sigma = cov)
      for(Var in 1:length(VarNames)){
        with.noise[which(debiased$NOAA.member == NOAA.ens & with.noise$dscale.member == dscale.ens),VarNames[Var]] =
          with.noise[which(debiased$NOAA.member == NOAA.ens & with.noise$dscale.member == dscale.ens),VarNames[Var]] + noise[Var]
      }
    }
  }
  return(with.noise)
}