if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
#if (!"glmtools" %in% installed.packages()) install.packages("glmtools",
#                                                            repos=c("http://cran.rstudio.com",
#                                                                    "http://owi.usgs.gov/R"))
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
#if (!"GLMr" %in% installed.packages()) install.packages("GLMr")

library(mvtnorm)
#library(glmtools)
library(ncdf4)
library(lubridate)
library(RCurl)
library(testit)
library(imputeTS)
library(tidyverse)
library(tools)



data_location = "/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/"
folder <- "/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE/"
forecast_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/testing_AED/"

source(paste0(folder,"/","configure_FLARE.R"))
source(paste0(folder, "/", "Rscripts/run_flare.R"))
source(paste0(folder, "/", "Rscripts/plot_forecast.R"))

#restart_file <- "/Users/quinn/Dropbox/Research/SSC_forecasting/GLEON_AGU_2018/FCR_betaV2_hist_2018_10_1_forecast_2018_10_2_2018102_5_53.nc"
restart_file <- "/Users/quinn/Dropbox/Research/SSC_forecasting/testing_AED/test1_H_2018_10_12_2018_10_22_F_0_2019623_16_22.nc"
restart_file <- NA


forecast_days <- 0
spin_up_days <- 26
sim_name <- "test1" 
start_day_local <- "2018-10-05"
start_time_local <- "00:00:00"
forecast_start_day_local <-"2018-10-30" 
hist_days <- as.numeric(difftime(as.POSIXct(forecast_start_day_local, tz = local_tzone),
                                 as.POSIXct(start_day_local, tz = local_tzone)))

out <- run_flare(start_day= start_day,
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
                         n_ds_members = n_ds_members,
                         include_wq = include_wq,
                         use_ctd = use_ctd,
                         uncert_mode = uncert_mode,
                         reference_tzone = reference_tzone,
                         cov_matrix = cov_matrix,
                         downscaling_coeff = downscaling_coeff,
                         GLMversion = GLMversion,
                         DOWNSCALE_MET = DOWNSCALE_MET,
                         FLAREversion = FLAREversion,
                         met_ds_obs_start = met_ds_obs_start,
                         met_ds_obs_end = met_ds_obs_end,
                         modeled_depths = modeled_depths)


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
              use_ctd = use_ctd,
              modeled_depths = modeled_depths)
