if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")

library(mvtnorm)
library(ncdf4)
library(lubridate)
library(RCurl)
library(testit)
library(imputeTS)
library(tidyverse)
library(tools)

data_location <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/"
code_folder <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE/"
forecast_location <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/test/"
execute_location <<- "/Users/quinn/Desktop/FLARE_AED_test/"

source(paste0(forecast_location,"/","configure_FLARE.R"))
source(paste0(code_folder, "/", "Rscripts/run_flare.R"))
source(paste0(code_folder, "/", "Rscripts/plot_forecast.R"))

start_day_local <- "2019-09-20" 
restart_file <- NA #"

sim_name <- "testing_sim"
start_time_local <- "07:00:00"
forecast_start_day_local <- "2019-09-22" 
spin_up_days <- 0
days_between_forecasts <- 1
forecast_days <- 16
num_forecast_periods <- 1000
wait_time <- 60*60

start_day_local <- as.POSIXct(start_day_local, format = "%Y-%m-%d")
forecast_start_day_local <- as.POSIXct(forecast_start_day_local, format = "%Y-%m-%d")

if(is.na(restart_file)){
  
  hist_days <- as.numeric(difftime(as_date(forecast_start_day_local),as_date(start_day_local)))
  
  out <- run_flare(start_day_local,
                   start_time_local,
                   forecast_start_day_local,
                   sim_name = sim_name,
                   hist_days = hist_days,
                   forecast_days = 0,
                   spin_up_days = spin_up_days,
                   restart_file = restart_file,
                   code_folder = code_folder,
                   forecast_location = forecast_location,
                   execute_location = execute_location,
                   push_to_git = push_to_git,
                   pull_from_git = pull_from_git,
                   data_location = data_location,
                   n_enkf_members = n_enkf_members,
                   n_ds_members = n_ds_members,
                   include_wq = include_wq,
                   use_ctd = use_ctd,
                   uncert_mode = uncert_mode,
                   cov_matrix = cov_matrix,
                   downscaling_coeff = downscaling_coeff,
                   GLMversion = GLMversion,
                   DOWNSCALE_MET = DOWNSCALE_MET,
                   FLAREversion = FLAREversion,
                   met_ds_obs_start = met_ds_obs_start,
                   met_ds_obs_end = met_ds_obs_end,
                   modeled_depths = modeled_depths,
                   forecast_sss_on = FALSE)
  
  
  plot_forecast(pdf_file_name = unlist(out)[2],
                output_file = unlist(out)[1],
                include_wq = include_wq,
                forecast_days = forecast_days,
                code_folder = code_folder,
                save_location = forecast_location,
                data_location = data_location,
                plot_summaries = TRUE,
                push_to_git = push_to_git,
                pull_from_git = pull_from_git,
                use_ctd = use_ctd,
                modeled_depths = modeled_depths)
  
  #ADVANCE TO NEXT DAY
  start_day_local <- start_day_local + days(hist_days)
  restart_file <- unlist(out)[1]
}

forecast_day_count <- 1
#ALL SUBSEQUENT DAYS
if(num_forecast_periods > 0){
  repeat{
    
    startTime <- Sys.time()
    if(forecast_day_count == 1){
      hist_days <- 1
    }else{
      hist_days <- days_between_forecasts
    }
    
    if(use_future_met){
      #LOOP TO KEEP CHECKING FOR A NOAA FORECAST
      forecast_avialable = FALSE
      while(forecast_avialable == FALSE){
        forecast_start_day_local <- start_day_local + days(hist_days)
        if(day(forecast_start_day_local) < 10){
          forecast_day <- paste0('0',day(forecast_start_day_local))
        }else{
          forecast_day <- paste0(day(forecast_start_day_local))
        }
        if(month(forecast_start_day_local) < 10){
          forecast_month <- paste0('0',month(forecast_start_day_local))
        }else{
          forecast_month <- paste0(month(forecast_start_day_local))
        }
        forecast_base_name <- paste0(year(forecast_start_day_local),forecast_month,forecast_day,'gep_all_12z.csv')
        
        noaa_location <- paste0(data_location,'/','noaa-data')
        setwd(noaa_location)
        system(paste0('git pull'))
        
        if(!file.exists(paste0(noaa_location,'/',forecast_base_name))){
          print('Waiting for NOAA forecast')
          Sys.sleep(wait_time)
        }else{
          forecast_avialable = TRUE
        }
      }
    }
    
    spin_up_days <- 0
    
    if(forecast_no_SSS){
      
      out1 <- run_flare(start_day_local,
                        start_time_local,
                        forecast_start_day_local,
                        sim_name = sim_name,
                        hist_days = hist_days,
                        forecast_days = forecast_days,
                        spin_up_days = spin_up_days,
                        restart_file = restart_file,
                        code_folder = code_folder,
                        forecast_location = forecast_location,
                        execute_location = execute_location,
                        push_to_git = push_to_git,
                        pull_from_git = pull_from_git,
                        data_location = data_location,
                        n_enkf_members = n_enkf_members,
                        n_ds_members = n_ds_members,
                        include_wq = include_wq,
                        use_ctd = use_ctd,
                        uncert_mode = uncert_mode,
                        cov_matrix = cov_matrix,
                        downscaling_coeff = downscaling_coeff,
                        GLMversion = GLMversion,
                        DOWNSCALE_MET = DOWNSCALE_MET,
                        FLAREversion = FLAREversion,
                        met_ds_obs_start = met_ds_obs_start,
                        met_ds_obs_end = met_ds_obs_end,
                        modeled_depths = modeled_depths,
                        forecast_sss_on = FALSE)
      
      plot_forecast(pdf_file_name = unlist(out1)[2],
                    output_file = unlist(out1)[1],
                    include_wq = include_wq,
                    forecast_days = forecast_days,
                    code_folder = code_folder,
                    save_location = forecast_location,
                    data_location = data_location,
                    plot_summaries = TRUE,
                    push_to_git = push_to_git,
                    pull_from_git = pull_from_git,
                    use_ctd = use_ctd,
                    modeled_depths = modeled_depths)
    }
    
    if(forecast_SSS){
      out2 <- run_flare(start_day_local,
                        start_time_local,
                        forecast_start_day_local,
                        sim_name = paste0(sim_name, "_SSS"),
                        hist_days = hist_days,
                        forecast_days = forecast_days,
                        spin_up_days = spin_up_days,
                        restart_file = restart_file,
                        code_folder = code_folder,
                        forecast_location = forecast_location,
                        execute_location = execute_location,
                        push_to_git = push_to_git,
                        pull_from_git = pull_from_git,
                        data_location = data_location,
                        n_enkf_members = n_enkf_members,
                        n_ds_members = n_ds_members,
                        include_wq = include_wq,
                        use_ctd = use_ctd,
                        uncert_mode = uncert_mode,
                        cov_matrix = cov_matrix,
                        downscaling_coeff = downscaling_coeff,
                        GLMversion = GLMversion,
                        DOWNSCALE_MET = DOWNSCALE_MET,
                        FLAREversion = FLAREversion,
                        met_ds_obs_start = met_ds_obs_start,
                        met_ds_obs_end = met_ds_obs_end,
                        modeled_depths = modeled_depths,
                        forecast_sss_on = TRUE)
      
      plot_forecast(pdf_file_name = unlist(out2)[2],
                    output_file = unlist(out2)[1],
                    include_wq = include_wq,
                    forecast_days = forecast_days,
                    code_folder = code_folder,
                    save_location = forecast_location,
                    data_location = data_location,
                    plot_summaries = TRUE,
                    push_to_git = push_to_git,
                    pull_from_git = pull_from_git,
                    use_ctd = use_ctd,
                    modeled_depths = modeled_depths)
    }
    
    
    restart_file <- unlist(out1)[1]
    
    #ADVANCE TO NEXT DAY
    start_day_local <- start_day_local + days(hist_days)
    forecast_day_count <- forecast_day_count + 1
    if(!is.na(num_forecast_periods)){
      if(forecast_day_count > num_forecast_periods){
        break
      }
    }
  }
}
