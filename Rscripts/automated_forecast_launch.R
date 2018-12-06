if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"glmtools" %in% installed.packages()) install.packages('glmtools', repos=c('http://cran.rstudio.com', 'http://owi.usgs.gov/R'))
if (!"RCurl" %in% installed.packages()) install.packages('RCurl')
if (!"testit" %in% installed.packages()) install.packages('testit')
library(mvtnorm)
library(glmtools)
library(ncdf4)
library(lubridate)
library(RCurl)
library(testit)

sim_name <- 'AGU_GLEON_INIT_UNCERT' #FCR_betaV2'
Folder <- '/Users/quinn/Dropbox/Research/SSC_forecasting/SSC_forecasting/'
forecast_location <- '/Users/quinn/Dropbox/Research/SSC_forecasting/test_forecast/' 
data_location <- '/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/' 
start_day <- '2018-10-07 00:00:00'
forecast_start_day <- '2018-10-08 00:00:00'
spin_up_days <- 0
num_forecast_days <- 1  #Set to NA if running into future
init_restart_file <- '/Users/quinn/Dropbox/Research/SSC_forecasting/test_forecast/FCR_betaV2_hist_2018_10_6_forecast_2018_10_7_2018107_5_50.nc'
wait_time <- 60*10
push_to_git <- FALSE
reference_tzone <- 'GMT'
nEnKFmembers <- 50
include_wq <- FALSE
USE_CTD <- FALSE

source(paste0(Folder,'/','Rscripts/EnKF_GLM_wNOAAens_V2.R'))
source(paste0(Folder,'/','Rscripts/evaluate_forecast.R'))
source(paste0(Folder,'/','Rscripts/plot_forecast_management.R'))
source(paste0(Folder,'/','Rscripts/plot_forecast_netcdf.R'))

if(is.na(init_restart_file)){
  hist_days <- as.numeric(difftime(as.POSIXct(forecast_start_day, tz = reference_tzone), as.POSIXct(start_day,tz = reference_tzone)))
  
  #FIRST DAY
  out <- run_forecast(
    start_day = start_day,
    sim_name = sim_name, 
    hist_days = hist_days-1,
    forecast_days = 0,
    spin_up_days = spin_up_days,
    restart_file = NA,
    Folder = Folder,
    forecast_location = forecast_location,
    push_to_git=push_to_git,
    data_location = data_location,
    nEnKFmembers = nEnKFmembers,
    include_wq = include_wq,
    USE_CTD = USE_CTD,
    uncert_mode = 1
  )
  
  plot_forecast_netcdf(pdf_file_name = paste0(unlist(out)[2],'.pdf'),
                       output_file = unlist(out)[1],
                       include_wq = include_wq,
                       code_location = paste0(Folder,'/Rscripts/'),
                       save_location = forecast_location,
                       data_location = data_location,
                       plot_summaries = FALSE,
                       USE_CTD = USE_CTD)
  
  #ADVANCE TO NEXT DAY
  start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S") + days(hist_days) - days(1)
  restart_file <- unlist(out)[1]
}else{
  start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S")
  restart_file <- init_restart_file
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
  
  out <- run_forecast(
    start_day= start_day,
    sim_name = sim_name, 
    hist_days = 1,
    forecast_days = 15,
    spin_up_days = 0,
    restart_file = restart_file,
    Folder = Folder,
    forecast_location = forecast_location,
    push_to_git=push_to_git,
    data_location = data_location,
    nEnKFmembers = nEnKFmembers,
    include_wq = include_wq,
    USE_CTD = USE_CTD
  )
  forecast_day_count <- forecast_day_count + 1
  
  restart_file <- unlist(out)[1]
  
  plot_forecast_management(pdf_file_name = paste0(unlist(out)[2],'_management.png'),
                           output_file = unlist(out)[1],
                           include_wq = FALSE,
                           code_location = paste0(Folder,'/Rscripts/'),
                           save_location = forecast_location,
                           data_location = data_location,
                           plot_summaries = TRUE,
                           PRE_SCC = FALSE,
                           push_to_git=push_to_git,
                           USE_CTD = USE_CTD,
                           uncert_mode = 1)
  
  #ADVANCE TO NEXT DAY
  start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S") + days(1)
  if(!is.na(num_forecast_days)){
    if(forecast_day_count > num_forecast_days){
      break
    }
  }
  
}