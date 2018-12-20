if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"glmtools" %in% installed.packages()) install.packages("glmtools",
                                                            repos=c("http://cran.rstudio.com",
                                                                    "http://owi.usgs.gov/R"))
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")

library(mvtnorm)
library(glmtools)
library(ncdf4)
library(lubridate)
library(RCurl)
library(testit)
library(imputeTS)

folder <- "/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE/"
forecast_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/GLEON_AGU_2018/" 
data_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/" 
restart_file <- "/Users/quinn/Dropbox/Research/SSC_forecasting/GLEON_AGU_2018/test_hist_2018_7_10_20181219_23_46.nc"
spin_up_days <- 0
push_to_git <- FALSE
pull_from_git <- FALSE
reference_tzone <- "GMT"
n_enkf_members <- 50
forecast_days <- 16
include_wq <- FALSE
use_ctd <- FALSE
num_forecast_periods <- NA
wait_time <- 60*10


sim_name <- "test" 
start_day <- "2018-12-19 00:00:00" #GMT
forecast_start_day <-"2018-12-20 00:00:00" #GMT 

source(paste0(folder, "/", "Rscripts/run_enkf_forecast.R"))
source(paste0(folder, "/", "Rscripts/evaluate_forecast.R"))
source(paste0(folder, "/", "Rscripts/plot_forecast.R"))

if(is.na(restart_file)){
  hist_days <- as.numeric(difftime(as.POSIXct(forecast_start_day, tz = reference_tzone), as.POSIXct(start_day,tz = reference_tzone)))
  
  out <- run_enkf_forecast(start_day= start_day,
                           sim_name = sim_name,
                           hist_days = hist_days,
                           forecast_days = 0,
                           spin_up_days = spin_up_days,
                           restart_file = NA,
                           folder = folder,
                           forecast_location = forecast_location,
                           push_to_git = push_to_git,
                           pull_from_git = pull_from_git,
                           data_location = data_location,
                           n_enkf_members = n_enkf_members,
                           include_wq = include_wq,
                           use_ctd = use_ctd,
                           uncert_mode = 1,
                           cov_matrix = "Qt_cov_matrix_11June_14Aug2_18.csv",
                           alpha = c(0.5, 0.5, 0.9))
  
  plot_forecast(pdf_file_name = unlist(out)[2],
                output_file = unlist(out)[1],
                include_wq = include_wq,
                forecast_days = 0,
                code_location = paste0(folder, "/Rscripts/"),
                save_location = forecast_location,
                data_location = data_location,
                plot_summaries = FALSE,
                pre_scc = FALSE,
                push_to_git = push_to_git,
                pull_from_git = pull_from_git,
                use_ctd = use_ctd)
  
  #ADVANCE TO NEXT DAY
  start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S") + days(hist_days)
  restart_file <- unlist(out)[1]
}else{
  start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S")
  restart_file <- restart_file
}

forecast_day_count <- 1
#ALL SUBSEQUENT DAYS
repeat{
  
  startTime <- Sys.time()
  
  
  #LOOP TO KEEP CHECKING FOR A NOAA FORECAST
  forecast_avialable = FALSE
  while(forecast_avialable == FALSE){
    forecast_start_time <- start_day + days(1)
    if(day(forecast_start_time) < 10){
      forecast_day <- paste0('0',day(forecast_start_time))
    }else{
      forecast_day <- paste0(day(forecast_start_time))
    }
    if(month(forecast_start_time) < 10){
      forecast_month <- paste0('0',month(forecast_start_time))
    }else{
      forecast_month <- paste0(month(forecast_start_time))
    }
    forecast_base_name <- paste0(year(forecast_start_time),forecast_month,forecast_day,'gep_all_00z.csv')
    
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
  
  start_day <- paste0(strftime(start_day,format = "%Y-%m-%d",usetz = FALSE)," 00:00:00")
  
  hist_days <- 1
  spin_up_days <- 0
  out <- run_enkf_forecast(start_day= start_day,
                           sim_name = sim_name,
                           hist_days = hist_days,
                           forecast_days = forecast_days,
                           spin_up_days = spin_up_days,
                           restart_file = restart_file,
                           folder = folder,
                           forecast_location = forecast_location,
                           push_to_git = push_to_git,
                           pull_from_git = pull_from_git,
                           data_location = data_location,
                           n_enkf_members = n_enkf_members,
                           include_wq = include_wq,
                           use_ctd = use_ctd,
                           uncert_mode = 1,
                           cov_matrix = "Qt_cov_matrix_11June_14Aug2_18.csv",
                           alpha = c(0.5, 0.5, 0.9))
  
  forecast_day_count <- forecast_day_count + 1
  
  restart_file <- unlist(out)[1]
  
  plot_forecast(pdf_file_name = unlist(out)[2],
                output_file = unlist(out)[1],
                include_wq = include_wq,
                forecast_days = forecast_days,
                code_location = paste0(folder, "/Rscripts/"),
                save_location = forecast_location,
                data_location = data_location,
                plot_summaries = FALSE,
                pre_scc = FALSE,
                push_to_git = push_to_git,
                pull_from_git = pull_from_git,
                use_ctd = use_ctd)
  
  #ADVANCE TO NEXT DAY
  start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S") + days(1)
  if(!is.na(num_forecast_periods)){
    if(forecast_day_count > num_forecast_periods){
      break
    }
  }
  
}