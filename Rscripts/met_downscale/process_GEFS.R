# --------------------------------------
# purpose: process GEFS forecasts and save as input for lake downscaling
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: processes GEFS forecasts for three scenarios:
# (1) downscaled without noise addition
# (2) downscaled with noise addition
# (3) not downscaled ("out of box")
# Then, the output for each ensemble member is saved as a .csv file
# The function returns: (1) a list of the names of the .csv files and (2) a datframe of the processed output for all ensembles 
# --------------------------------------

process_GEFS <- function(file_name,
                         n_ds_members,
                         n_met_members,
                         sim_files_folder,
                         in_directory,
                         out_directory,
                         output_tz,
                         VarNames,
                         VarNamesStates,
                         DOWNSCALE_MET,
                         FIT_PARAMETERS,
                         ADD_NOISE,
                         WRITE_FILES){
  # -----------------------------------
  # 1. read in and reformat forecast data
  # -----------------------------------
  f <- paste0(in_directory,'/',file_name,'.csv')
  if(!file.exists(f)){
    print('Missing forecast file!')
    print(f)
    stop()
  }else{
    
    d <- read.csv(paste0(in_directory,'/',file_name,'.csv')) 
    full_time <- rep(NA,length(d$forecast.date)*6)
    begin_step <- as_datetime(head(d$forecast.date,1), tz = output_tz)
    end_step <- as_datetime(tail(d$forecast.date,1), tz = output_tz)
    full_time <- seq(begin_step, end_step, by = "1 hour", tz = output_tz) # grid
    forecasts <- prep_for(d)
    REPLACE_START_WITH_OBS = FALSE
    if(REPLACE_START_WITH_OBS == TRUE){
      # change this to read obs_met_outfile 
      last_obs_time = begin_step + 8*60*60
      recent.obs <- read.csv(met_obs_fname_wdir) %>% 
        prep_obs() %>%
        dplyr::mutate(ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
               RelHum = ifelse(RelHum <0, 0, RelHum),
               RelHum = ifelse(RelHum > 100, 100, RelHum),
               # AirTemp = AirTemp - 273.15,
               WindSpeed = ifelse(WindSpeed <0, 0, WindSpeed)) %>%
        aggregate_obs_to_hrly() %>%
        filter(timestamp <= last_obs_time & timestamp >= begin_step - 1)
      for(i in 1:length(recent.obs)){
        time_i = recent.obs$timestamp[i]
        if(time_i %in% forecasts$timestamp){
          forecasts[which(forecasts$timestamp == time_i),"AirTemp"] = recent.obs[which(recent.obs$timestamp == time_i),"AirTemp"]
          forecasts[which(forecasts$timestamp == time_i),"LongWave"] = recent.obs[which(recent.obs$timestamp == time_i),"LongWave"]
          forecasts[which(forecasts$timestamp == time_i),"ShortWave"] = recent.obs[which(recent.obs$timestamp == time_i),"ShortWave"]
          forecasts[which(forecasts$timestamp == time_i),"RelHum"] = recent.obs[which(recent.obs$timestamp == time_i),"RelHum"]
          forecasts[which(forecasts$timestamp == time_i),"WindSpeed"] = recent.obs[which(recent.obs$timestamp == time_i),"WindSpeed"]
        }
      }
    }
    forecasts[which(forecasts$timestamp == min(forecasts$timestamp)),]$ShortWave = forecasts[which(forecasts$timestamp == min(forecasts$timestamp) + 24*60*60),]$ShortWave
    # hack to give sw values for 1st measurement (that are in fact the values for the second day). This is to avoid having NAs for the first few hours of forecast
    forecasts[which(forecasts$timestamp == min(forecasts$timestamp)),]$LongWave = forecasts[which(forecasts$timestamp == min(forecasts$timestamp) + 6*60*60),]$LongWave
    forecasts[which(forecasts$timestamp == min(forecasts$timestamp)),]$Rain = forecasts[which(forecasts$timestamp == min(forecasts$timestamp) + 6*60*60),]$Rain
    # hack to give lw values for 1st measurement (that are in fact the values of the next measurement, 6 hours later). This is to avoid having NAs for the first few hours of forecast
    
  }
  
  # -----------------------------------
  # 2. process forecast according to desired method
  # -----------------------------------

  
  if(DOWNSCALE_MET == TRUE){
    ## Downscaling option
    print("Downscaling option")
    load(file = paste(sim_files_folder,"/debiased.coefficients.RData", sep = ""))
    ds = downscale_met(forecasts,
                       debiased.coefficients,
                       VarNames,
                       VarNamesStates,
                       USE_ENSEMBLE_MEAN = FALSE,
                       PLOT = FALSE)
    if(ADD_NOISE == TRUE){
      ## Downscaling + noise addition option
      print("with noise")
      ds.noise = add_noise(debiased = ds,
                           cov = debiased.covar,
                           n_ds_members,
                           n_met_members) %>%
          mutate(ShortWave = ifelse(ShortWaveOld == 0, 0, ShortWave),
                 ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
                 RelHum = ifelse(RelHum <0, 0, RelHum),
                 RelHum = ifelse(RelHum > 100, 100, RelHum),
                 AirTemp = AirTemp - 273.15,
                 WindSpeed = ifelse(WindSpeed <0, 0, WindSpeed))
      output = ds.noise
    }else{
      print("without noise")
      ds = ds %>% mutate(dscale.member = 0) %>%
        mutate(ShortWave = ifelse(ShortWave <0, 0, ShortWave),
               AirTemp = AirTemp - 273.15)
      output = ds
    }
    
  }else{
    ## "out of box" option
    print("out of box option")
    out.of.box = out_of_box(forecast, VarNames) %>%
      dplyr::mutate(AirTemp = AirTemp - 273.15,
                    RelHum = ifelse(RelHum <0, 0, RelHum),
                    RelHum = ifelse(RelHum > 100, 100, RelHum))
    output = out.of.box
  }
  
  # -----------------------------------
  # 3. Produce output files
  # -----------------------------------
  met_file_list = NULL
  if(WRITE_FILES){
    # Rain and Snow are currently not downscaled, so they are calculated here
    hrly.Rain.Snow = forecasts %>% dplyr::mutate(Snow = 0) %>%
      select(timestamp, NOAA.member, Rain, Snow) %>%
      repeat_6hr_to_hrly()
    
    write_file <- function(df){
      # formats GLM_climate, writes it as a .csv file, and returns the filename
      GLM_climate <- df %>% plyr::rename(c("timestamp" = "full_time")) %>%
        select(full_time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain, Snow)
      GLM_climate[,"full_time"] = strftime(GLM_climate$full_time, format="%Y-%m-%d %H:%M")
      colnames(GLM_climate) =  noquote(c("time", 
                                         "ShortWave",
                                         "LongWave",
                                         "AirTemp",
                                         "RelHum",
                                         "WindSpeed",
                                         "Rain",
                                         "Snow"))
      current_filename = paste0(out_directory,'/','met_hourly_',file_name,'_NOAA',NOAA.ens,'_ds',dscale.ens,'.csv')
      write.csv(GLM_climate,file = current_filename, row.names = FALSE, quote = FALSE)
      return(current_filename)
    }
    
    for(NOAA.ens in 1:21){
      Rain.Snow = hrly.Rain.Snow %>% filter(NOAA.member == NOAA.ens) %>%
        select(timestamp, Rain, Snow)
      if(DOWNSCALE_MET){
        if(ADD_NOISE){ # downscale met with noise addition
          for(dscale.ens in 1:n_ds_members){
            GLM_climate_ds = ds.noise %>%
              filter(NOAA.member == NOAA.ens & dscale.member == dscale.ens) %>%
              arrange(timestamp) %>%
              select(timestamp, VarNames)
            GLM_climate_ds = full_join(Rain.Snow, GLM_climate_ds, by = "timestamp")
            current_filename = write_file(GLM_climate_ds)
            met_file_list = append(met_file_list, current_filename)
          }
        }else{ # downscale met, no noise addition
          dscale.ens = 0
          GLM_climate_ds = ds %>%
            filter(NOAA.member == NOAA.ens) %>%
            arrange(timestamp) %>%
            select(timestamp, VarNames)
          GLM_climate_ds = full_join(Rain.Snow, GLM_climate_ds, by = "timestamp")
          current_filename = write_file(GLM_climate_ds)
          met_file_list = append(met_file_list, current_filename)
        }
      }else{ # out of box
        dscale.ens = 0
        GLM_climate_no_ds = out.of.box %>%
          filter(NOAA.member == NOAA.ens) %>%
          arrange(timestamp) %>%
          select(timestamp, Rain, Snow, VarNames)
        current_filename = write_file(GLM_climate_no_ds)
        met_file_list = append(met_file_list, current_filename)
      }
    }
  }
  return(list(met_file_list, output))
}










