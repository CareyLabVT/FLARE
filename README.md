# FLARE
Forecasting Lake And Reservoir Ecosystems

Directions for using FLARE
note: that FLARE has not been fully generalized and only works at Falling Creek Reservoir.  We are working to generalize for use in other lakes and look forward to collaborating with others to get FLARE working at your site!

Users guide is pending

`initiate_forecast_example.R` provides an example for running FLARE once. Move this file to your `forecast_location`

`automated_forecast_launch_example.R` provides an example for automated running of forecasts.  Move this file to your `forecast_location` (see below) so that you can edit it separate of the FLARE repository.  

`configure_FLARE.R` are the variables that you need to set to run FLARE that are not in `initiate_forecast_example.R` or `automated_forecast_launch_example.R`

# Setting up data repositories for FCR

In your `data_location` directory run the following three commands at the command line:

`git clone -b carina-data --single-branch https://github.com/CareyLabVT/SCCData.git carina-data`

`git clone -b mia-data --single-branch https://github.com/CareyLabVT/SCCData.git mia-data`

`git clone -b noaa-data --single-branch https://github.com/CareyLabVT/SCCData.git noaa-data`

`git clone -b manual-data --single-branch https://github.com/CareyLabVT/SCCData.git manual-data`


