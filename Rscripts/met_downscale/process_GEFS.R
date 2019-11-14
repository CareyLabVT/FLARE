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
                         local_tzone,
                         VarInfo,
                         replaceObsNames,
                         hrly.observations,
                         DOWNSCALE_MET,
                         FIT_PARAMETERS,
                         met_downscale_uncertainty,
                         WRITE_FILES,
                         downscaling_coeff,
                         full_time_local,
                         weather_uncertainty){
  # -----------------------------------
  # 1. read in and reformat forecast data
  # -----------------------------------
  f <- paste0(in_directory,'/',file_name,'.csv')
  if(!file.exists(f)){
    print('Missing forecast file!')
    print(f)
    stop()
  }
  
  d <- read.csv(paste0(in_directory,'/',file_name,'.csv')) 
  #full_time <- rep(NA,length(d$forecast.date)*6)
  #begin_step <- as_datetime(head(d$forecast.date,1), tz = local_tzone)
  #end_step <- as_datetime(tail(d$forecast.date,1), tz = local_tzone)
  
  # adjust for different timezones in saved GEFS forecasts 
  if(date(full_time_local[1])>as_datetime("2018-12-07 02:00:00", tz = "EST")){ 
    for.input_tz = "GMT"
  }else{
    for.input_tz = "US/Eastern"
  }
  
  #for.input_tz = "GMT"
  
  #full_time_local <- seq(begin_step, end_step, by = "1 hour", tz = local_tzone) # grid
  
  forecasts <- prep_for(d, input_tz = for.input_tz, local_tzone, weather_uncertainty)
  
  time0 = min(forecasts$timestamp)
  time_end = max(forecasts$timestamp)
  
  # -----------------------------------
  # 2. process forecast according to desired method
  # -----------------------------------
  
  if(DOWNSCALE_MET == TRUE){
    ## Downscaling option
    print("Downscaling option")
    if(is.na(downscaling_coeff)){
      load(file = paste(out_directory,"/debiased.coefficients.RData", sep = ""))
    }else{
      load(file = downscaling_coeff)
    }
    output <- downscale_met(forecasts,
                        debiased.coefficients,
                        VarInfo,
                        PLOT = FALSE,
                        local_tzone = local_tzone,
                        debiased.covar,
                        n_ds_members,
                        n_met_members,
                        met_downscale_uncertainty)
    output <- output %>% mutate(AirTemp = AirTemp - 273.15) # from Kelvin to Celsius 
    
  }else{
    ## "out of box" option
    print("out of box option")
    out.of.box = out_of_box(forecasts, VarInfo$VarNames) %>%
      dplyr::mutate(AirTemp = AirTemp - 273.15,
                    RelHum = ifelse(RelHum <0, 0, RelHum),
                    RelHum = ifelse(RelHum > 100, 100, RelHum),
                    ShortWave = ifelse(ShortWave < 0, 0, ShortWave)) %>%
      arrange(timestamp)
    output = out.of.box %>%
      dplyr::mutate(dscale.member = 0)
  }
  
  hrly.observations <- hrly.observations %>%
    mutate(AirTemp = AirTemp - 273.15)
  
  obs.time0 <- hrly.observations %>% filter(timestamp == time0)
  
  VarNamesStates = VarInfo %>%
    filter(VarType == "State")
  VarNamesStates = VarNamesStates$VarNames
  
  # replace the first measurements of the downscaled output with observations so that the model has a smooth transition from past observations to future forecast
  # if missing observation then it skips this step
  for(i in 1:length(VarNamesStates)){
    if(nrow(obs.time0[VarNamesStates[i]]) == 1){
      output[which(output$timestamp == time0),VarNamesStates[i]] = obs.time0[VarNamesStates[i]]
    }
  }
  
  output.time0.6.hrs <- output %>% 
    filter(timestamp == time0 | timestamp == time0 + 6*60*60)
  states.output0.6.hrs <- spline_to_hourly(output.time0.6.hrs,VarNamesStates)
  output <- output %>% 
    full_join(states.output0.6.hrs, by = c("NOAA.member","dscale.member","timestamp"), suffix = c("",".splined")) %>%
    mutate(AirTemp = ifelse(is.na(AirTemp.splined), AirTemp, AirTemp.splined),
           WindSpeed = ifelse(is.na(WindSpeed.splined), WindSpeed, WindSpeed.splined),
           RelHum = ifelse(is.na(RelHum.splined), RelHum, RelHum.splined)) %>%
    select(-AirTemp.splined, WindSpeed.splined, RelHum.splined)
  output <- output %>% filter(timestamp < time_end)
  
  output$timestamp <- with_tz(output$timestamp, local_tzone)
  
  # -----------------------------------
  # 3. Produce output files
  # -----------------------------------
  met_file_list = NULL
  if(WRITE_FILES){
    print("Write Output Files")
    # Rain and Snow are currently not downscaled, so they are calculated here
    # hrly.Rain.Snow = forecasts %>% dplyr::mutate(Snow = 0) %>%
    #   select(timestamp, NOAA.member, Rain, Snow) %>%
    #   repeat_6hr_to_hrly()
    
    
    write_file <- function(df){
      # formats GLM_climate, writes it as a .csv file, and returns the filename
      GLM_climate <- df %>% plyr::rename(c("timestamp" = "full_time_local")) %>%
        select(full_time_local, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain, Snow)
      GLM_climate[,"full_time_local"] = strftime(GLM_climate$full_time_local, format="%Y-%m-%d %H:%M", tz = attributes(GLM_climate$full_time_local)$tzone)
      colnames(GLM_climate) =  noquote(c("time", 
                                         "ShortWave",
                                         "LongWave",
                                         "AirTemp",
                                         "RelHum",
                                         "WindSpeed",
                                         "Rain",
                                         "Snow"))
      current_filename = paste0('met_hourly_',file_name,'_NOAA',NOAA.ens,'_ds',dscale.ens,'.csv')
      write.csv(GLM_climate,file = paste0(out_directory, "/", current_filename), row.names = FALSE, quote = FALSE)
      return(current_filename)
    }
    
    for(NOAA.ens in 1:21){
      # Rain.Snow = hrly.Rain.Snow %>% filter(NOAA.member == NOAA.ens) %>%
      #   select(timestamp, Rain, Snow)
      if(DOWNSCALE_MET){
        if(met_downscale_uncertainty){ # downscale met with noise addition
          for(dscale.ens in 1:n_ds_members){
            GLM_climate_ds <- output %>%
              filter(NOAA.member == NOAA.ens & dscale.member == dscale.ens) %>%
              arrange(timestamp) %>%
              select(timestamp, VarInfo$VarNames)
            # GLM_climate_ds = full_join(Rain.Snow, GLM_climate_ds, by = "timestamp")
            GLM_climate_ds = GLM_climate_ds %>% 
              mutate(Snow = 0)
            current_filename = write_file(GLM_climate_ds)
            met_file_list = append(met_file_list, current_filename)
          }
        }else{ # downscale met, no noise addition
          dscale.ens = 0
          GLM_climate_ds = output %>%
            filter(NOAA.member == NOAA.ens) %>%
            arrange(timestamp) %>%
            select(timestamp, VarInfo$VarNames)
          # GLM_climate_ds = full_join(Rain.Snow, GLM_climate_ds, by = "timestamp")
          GLM_climate_ds = GLM_climate_ds %>% 
            mutate(Snow = 0)
          current_filename = write_file(GLM_climate_ds)
          met_file_list = append(met_file_list, current_filename)
        }
      }else{ # out of box
        dscale.ens = 0
        GLM_climate_no_ds = output %>%
          filter(NOAA.member == NOAA.ens) %>%
          arrange(timestamp) %>%
          select(timestamp, Snow, VarInfo$VarNames)
        current_filename = write_file(GLM_climate_no_ds)
        met_file_list = append(met_file_list, current_filename)
      }
    }
  }
  return(list(met_file_list, output))
}