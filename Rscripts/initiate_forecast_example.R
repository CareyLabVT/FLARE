if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"glmtools" %in% installed.packages()) install.packages("glmtools",
                                                            repos=c("http://cran.rstudio.com",
                                                                    "http://owi.usgs.gov/R"))
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"testit" %in% installed.packages()) install.packages("testit")
library(mvtnorm)
library(glmtools)
library(ncdf4)
library(lubridate)
library(RCurl)
library(testit)

folder <- "/Users/quinn/Dropbox/Research/SSC_forecasting/SSC_forecasting/"
forecast_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/GLEON_AGU_2018/" 
data_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/" 
start_day <- "2018-10-02 00:00:00"
forecast_start_day <- "2018-10-08 00:00:00"
restart_file <- "/Users/quinn/Dropbox/Research/SSC_forecasting/GLEON_AGU_2018/FCR_betaV2_hist_2018_10_1_forecast_2018_10_2_2018102_5_53.nc"
spin_up_days <- 0
num_forecast_days <- 0  #Set to NA if running into future
push_to_git <- FALSE
reference_tzone <- "GMT"
n_enkf_members <- 50
include_wq <- FALSE
use_ctd <- FALSE
source(paste0(folder,"/","Rscripts/run_enkf_forecast.R"))
source(paste0(folder,"/","Rscripts/evaluate_forecast.R"))
source(paste0(folder,"/","Rscripts/plot_forecast_management.R"))
source(paste0(folder,"/","Rscripts/plot_forecast_netcdf.R"))

sim_name <- "test" 
start_day <- "2018-07-12 00:00:00"
forecast_start_day <-"2018-07-16 00:00:00" 
hist_days <- as.numeric(difftime(as.POSIXct(forecast_start_day, tz = reference_tzone),
                                 as.POSIXct(start_day, tz = reference_tzone)))

out <- run_enkf_forecast(
  start_day= start_day,
  sim_name = sim_name, 
  hist_days = hist_days,
  forecast_days = 0,
  spin_up_days = 0,
  restart_file = NA,
  folder = folder,
  forecast_location = forecast_location,
  push_to_git=push_to_git,
  data_location = data_location,
  n_enkf_members = 1,
  include_wq = include_wq,
  use_ctd = use_ctd,
  uncert_mode = 1,
  cov_matrix = "Qt_cov_matrix_11June_14Aug2_18.csv",
  alpha = c(0.5, 0.5, 0.9))


plot_forecast_netcdf(pdf_file_name = paste0(unlist(out)[2], ".pdf"),
                     output_file = unlist(out)[1],
                     include_wq = include_wq,
                     code_location = paste0(folder, "/Rscripts/"),
                     save_location = forecast_location,
                     data_location = data_location,
                     plot_summaries = FALSE,
                     use_ctd = use_ctd)
