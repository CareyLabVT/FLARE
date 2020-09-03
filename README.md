# FLARE
Forecasting Lake And Reservoir Ecosystems

Directions for using FLARE

# Setting up data repositories for FCR

In your `data_location` directory run the following five commands at the command line:

`git clone -b carina-data --depth 1 https://github.com/CareyLabVT/SCCData.git carina-data`

`git clone -b mia-data --depth 1 https://github.com/CareyLabVT/SCCData.git mia-data`

`git clone -b diana-data --depth 1 https://github.com/CareyLabVT/SCCData.git diana-data`

`git clone -b fcre --depth 1 https://github.com/CareyLabVT/noaa_gefs_forecasts.git fcre`

`git clone -b manual-data --depth 1 https://github.com/CareyLabVT/SCCData.git manual-data`

You will also need to download the meterology file (`Met_final_2015_2019.csv`)from the Environmental Data Initiative and place it in the `manual-data` directory:  https://portal.edirepository.org/nis/mapbrowse?packageid=edi.389.4

# Initiating a simulation

1) Create a directory (`forecast_location`) where you will move simulation configuration files and where you want FLARE to save the forecast output.

2) Copy the `configure_FLARE.R` from `FLARE/example_configuration_files/` to your `forecast_location`. 

3) Copy the relevant General Lake Model nml files `FLARE/example_configuration_files/` to your `forecast_location`

4) Copy either of the two files below to from the `FLARE/example_configuration_files/` to `forecast_location`

 * `initiate_forecast_example.R` provides an example for running FLARE once. 

 * `automated_forecast_launch_example.R` provides an example for automated running of forecasts.  

5) Modify your `initiate_forecast_example.R` or `automated_forecast_launch_example.R` and `configure_FLARE.R` scripts to customize your simulation.

6) Run `initiate_forecast_example.R` or `automated_forecast_launch_example.R`
