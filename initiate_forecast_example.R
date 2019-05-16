if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"glmtools" %in% installed.packages()) install.packages("glmtools",
                                                            repos=c("http://cran.rstudio.com",
                                                                    "http://owi.usgs.gov/R"))
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")


library(mvtnorm)
library(glmtools)
library(ncdf4)
library(lubridate)
library(RCurl)
library(testit)
library(imputeTS)
library(tidyverse)

data_location = "/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/"
folder <- "/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE/"
forecast_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/GLEON_AGU_2018/"

restart_file <- "/Users/quinn/Dropbox/Research/SSC_forecasting/GLEON_AGU_2018/FCR_betaV2_hist_2018_10_1_forecast_2018_10_2_2018102_5_53.nc"
spin_up_days <- 0
push_to_git <- FALSE
pull_from_git <- TRUE
reference_tzone <- "GMT"
forecast_days <- 0
include_wq <- TRUE
use_ctd <- FALSE
DOWNSCALE_MET <- FALSE
GLMversion <- "GLM 3.0.0beta10"
FLAREversion <- "v1.0_beta.1"

#Note: this number is multiplied by 
# 1) the number of NOAA ensembles (21)
# 2) the number of downscaling essembles (50 is current)
# get to the total number of essembles
n_enkf_members <- 1
n_ds_members <- 1

source(paste0(folder, "/", "Rscripts/run_enkf_forecast.R"))
source(paste0(folder, "/", "Rscripts/evaluate_forecast.R"))
source(paste0(folder, "/", "Rscripts/plot_forecast.R"))

sim_name <- "test1" 
start_day <- "2018-07-12 00:00:00" #GMT
forecast_start_day <-"2019-05-01 00:00:00" #GMT 
hist_days <- as.numeric(difftime(as.POSIXct(forecast_start_day, tz = reference_tzone),
                                 as.POSIXct(start_day, tz = reference_tzone)))

out <- run_enkf_forecast(start_day= start_day,
                         sim_name = sim_name,
                         hist_days = hist_days,
                         forecast_days = forecast_days,
                         spin_up_days = 0,
                         restart_file = NA,
                         folder = folder,
                         forecast_location = forecast_location,
                         push_to_git = push_to_git,
                         pull_from_git = pull_from_git,
                         data_location = data_location,
                         n_enkf_members = n_enkf_members,
                         n_ds_members = n_ds_members,
                         include_wq = include_wq,
                         use_ctd = use_ctd,
                         uncert_mode = 9,
                         reference_tzone,
                         cov_matrix = "Qt_cov_matrix_init.csv",
                         alpha = c(0, 0, 0),
                         downscaling_coeff = NA,
                         GLMversion,
                         DOWNSCALE_MET,
                         FLAREversion,
                         met_ds_obs_start = as.Date("2018-04-06"),
                         met_ds_obs_end = as.Date(start_day))


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
