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
forecast_location <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/glm_testcase/"
execute_location <<- execute_location #"/Users/quinn/Desktop/FLARE_AED_test/"

source(paste0(forecast_location,"/","configure_FLARE.R"))
source(paste0(code_folder, "/", "Rscripts/run_flare.R"))
source(paste0(code_folder, "/", "Rscripts/plot_forecast.R"))

restart_file <- NA

sim_name <- "test" 
forecast_days <- 0
spin_up_days <- 0

start_day_local <- "2018-09-20"  #Note: 2018-07-16 is the first day with CTD observations for initial conditions
start_time_local <- "07:00:00"
forecast_start_day_local <- "2018-10-19" 

start_day_local <- as.POSIXct(start_day_local, format = "%Y-%m-%d")
forecast_start_day_local <- as.POSIXct(forecast_start_day_local, format = "%Y-%m-%d")
hist_days <- as.numeric(difftime(as_date(forecast_start_day_local),as_date(start_day_local)))

forecast_sss_on = FALSE

out <- run_flare(start_day_local,
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
                 forecast_sss_on = forecast_sss_on)


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