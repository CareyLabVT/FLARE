# FLARE
Forecasting Lake And Reservoir Ecosystems

Directions for using FLARE
note: that FLARE has not been fully generalized and only works at Falling Creek Reservoir.  We are working to generalize for use in other lakes


`initiate_forecast_example.R` provides an example for running FLARE once

`automated_forecast_launch_example.R` provides an example for automated running of forecasts.  Move this file to your `forecast_location` (see below) so that you can edit it separate of the FLARE repository.  

1) Set the location of the FLARE directory on your computer

`folder <- "/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE/"`

2) Set the location were you want the forecasts to be saved.  You will create this directory.

`forecast_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/FCR_forecasts/"`

3) Set the directory were the data used by FLARE is located (note: this involves setting a set of github repositories.  We will provide instructions for that in the future

`data_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/"`

4) Move either `initiate_forecast_example.R` or `automated_forecast_launch_example.R` to your `forecast_location` and modify the following variables.

5) Set the following variables 

`restart_file <- NA`: this is directory and name of the restart file if you are using one, otherwise set to NA

`spin_up_days <- 0`: A value greater than zero is the number of days FLARE initially runs without using data to constrain

`push_to_git <- TRUE`: TRUE = forecasts will be push to the GitHub respository that you set up in the  `forecast_location`

`pull_from_git <- TRUE`: TRUE = data in 'data_location' will be pulled from GitHub. Set to FALSE if not connected to the internet

`reference_tzone <- "GMT"`: Time zone that the forecasts will operate in.  GMT is required so that is lines up with NOAA forecast schedule on 00, 06, 12, 18 hr

`n_enkf_members <- 1`: Number of additional ensemble members beyond the number that is acquired by multiplying `n_ds_members` x 21 (the number of NOAA forecast ensembles). 

`n_ds_members <- 50`: Number of ensembles associated with uncertainity in downscaling NOAA forecast per NOAA forecast ensemble

`forecast_days <- 16`: Number of days in the future for each forecast (16 is the max from NOAA)

`include_wq <- FALSE`: Set to TRUE if forecasting water quality variables using AED

`use_ctd <- FALSE`: Set to TRUE if using data from CTD sampling (tells the code to look for it and expect a different input format

`num_forecast_periods <- NA`: NA = run forecats to present and keep checking for new NOAA forecast, set to a number if only running a specific number of forecasts

`wait_time <- 60*10`: number of seconds to wait before checking if a new NOAA forecast is avialable.

`GLMversion <- "GLM_3.0.0beta10"`: Version of GLM that is used.  The user needs to set this so that the output file has correct documentation

`DOWNSCALE_MET <- TRUE`: Set to TRUE if the NOAA 1x1 resolution gridded forecast is downscaled to local site.

`FLAREversion <- "v1.0_beta.1.01"`: Version of FLARE that is used. The user needs to set this so that the output file has correct documentation

`sim_name <- "FCRv1.beta2"`: Name of simulations that is used in the output files and plots 

`start_day <- "2018-07-10 00:00:00"` Date-time in GMT that FLARE starts

`forecast_start_day <-"2019-01-24 00:00:00"`:  Date-time in GMT that switch from historical data assimilation to a forecast occurs.  
