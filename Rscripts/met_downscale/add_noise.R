add_noise <- function(debiased, cov, n_ds_members, n_met_members){
  with.noise <- debiased %>%
    group_by(timestamp, NOAA.member, AirTemp, RelHum, WindSpeed, ShortWave, LongWave) %>%
    expand(dscale.member = 1:n_ds_members) %>%
    mutate(ShortWaveOld = ShortWave) %>%
    ungroup()
  
  for(NOAA.ens in 1:n_met_members){
    for(dscale.ens in 1:n_ds_members){
      noise = rmvnorm(1, mean = c(0,0,0,0,0), sigma = cov)
        with.noise[which(debiased$NOAA.member == NOAA.ens & with.noise$dscale.member == dscale.ens),3:7] =
          with.noise[which(debiased$NOAA.member == NOAA.ens & with.noise$dscale.member == dscale.ens),3:7] + noise
      }
  }
  return(with.noise)
}