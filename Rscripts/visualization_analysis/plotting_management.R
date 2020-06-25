lake_name <- "fcre"
code_folder <- "/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE/"
data_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/"
manual_data_location <- paste0(data_location, "/manual-data") 
insitu_obs_fname <- c(paste0(realtime_insitu_location,"/Catwalk.csv"),
                      paste0(manual_data_location,"/Catwalk_cleanedEDI.csv"))
#maintenance_file <- maintenance_file
nutrients_fname <- paste0(manual_data_location,"/chemistry.csv")
ctd_fname <- NA #paste0(manual_data_location,"/CTD_final_2013_2019.csv")
local_tzone <- "EST"
do_methods <<- c("do_sensor", "exo_sensor")
chla_methods <<- c("exo_sensor", "ctd")
temp_methods <<- c("thermistor", "do_sensor", "exo_sensor")
fdom_methods <<- c("exo_sensor")
nh4_methods <<- c("grab_sample")
no3_methods <<- c("grab_sample")
srp_methods <<- c("grab_sample")

time_threshold_seconds_temp <<- 60*30
time_threshold_seconds_oxygen <<- 60*60*12
time_threshold_seconds_chla <<- 60*60*12
time_threshold_seconds_fdom <<- 60*60*12
time_threshold_seconds_nh4 <<- 60*60*12
time_threshold_seconds_no3 <<- 60*60*12
time_threshold_seconds_srp <<- 60*60*12
distance_threshold_meter <<- 0.15


par_names_potential <- c("zone1temp",
                         "zone2temp"
                         ,"sw_factor"
                         ,"Fsed_oxy_zone1"
                         ,"Fsed_oxy_zone2"
                         ,"Rdom_minerl"
                         ,"Fsed_frp"
                         ,"Fsed_amm"
                         ,"Fsed_nit"
                         ,"R_resp")

wq_names_potential <- c("OXY_oxy",
                        "CAR_dic",
                        "CAR_ch4",
                        "SIL_rsi",
                        "NIT_amm",
                        "NIT_nit",
                        "PHS_frp",
                        "OGM_doc",
                        "OGM_poc",
                        "OGM_don",
                        "OGM_pon",
                        "OGM_dop",
                        "OGM_pop",
                        "NCS_ss1",
                        "PHS_frp_ads",
                        "PHY_TCHLA")

tchla_components_vars_potential <- c("PHY_cyano","PHY_green","PHY_diatom")



source(paste0(code_folder,"/","Rscripts/extract_observations.R"))
source(paste0(code_folder,"/","Rscripts/",lake_name,"/in_situ_qaqc.R"))  

realtime_insitu_location <- paste0(data_location,"/mia-data")

setwd(realtime_insitu_location)
if(pull_from_git){
  system(paste0("git pull"))
}

###### EVERY THING ABOVE HERE SHOULD BE DEALT WITH IN THE QAQC CONTAINER###

### EVERYTHING AFTER IS FOR THE PDF GENERATION ####

output_file <- "/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/glm_aed_forecasts/model_output/FLARE1_SPIN_UP_H_2018_07_12_2018_08_27_F_0_5252020_12_25.nc"
save_location <- "/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/glm_aed_forecasts/model_output/"

nc <- nc_open(output_file)
t <- ncvar_get(nc,'time')
local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
full_time_local <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = local_tzone)
full_time_day_local <- as_date(full_time_local)
forecasted <- ncvar_get(nc,'forecasted')
depths <- ncvar_get(nc,'z')

state_names <- c("temp")


if(length(which(forecasted == 1))>0){
  forecast_index <- which(forecasted == 1)[1]
}else{
  forecast_index <- 0
}

state_list <- ncvar_get(nc,state_names)



#PROCESS TEMPERATURE OBSERVATIONS

cleaned_observations_file_long <- paste0(save_location, 
                                         "/observations_postQAQC_long.csv")

in_situ_qaqc(insitu_obs_fname = insitu_obs_fname, 
             data_location = data_location, 
             maintenance_file = maintenance_file,
             ctd_fname = ctd_fname, 
             nutrients_fname = nutrients_fname,
             cleaned_observations_file_long = cleaned_observations_file_long,
             lake_name,
             code_folder)

#####

print("Extracting temperature observations")
obs_temp <- extract_observations(fname = cleaned_observations_file_long,
                                 full_time_local,
                                 modeled_depths = modeled_depths,
                                 local_tzone,
                                 target_variable = "temperature",
                                 time_threshold_seconds = time_threshold_seconds_temp,
                                 distance_threshold_meter = distance_threshold_meter,
                                 methods = temp_methods)

if(include_wq){
  
  print("Extracting DO observations")
  obs_do <- extract_observations(fname = cleaned_observations_file_long,
                                 full_time_local,
                                 modeled_depths = modeled_depths,
                                 local_tzone,
                                 target_variable = "oxygen",
                                 time_threshold_seconds = time_threshold_seconds_oxygen,
                                 distance_threshold_meter = distance_threshold_meter,
                                 methods = do_methods)
  
  print("Extracting Chl-a observations")
  obs_chla <- extract_observations(fname = cleaned_observations_file_long,
                                   full_time_local,
                                   modeled_depths = modeled_depths,
                                   local_tzone,
                                   target_variable = "chla",
                                   time_threshold_seconds = time_threshold_seconds_chla,
                                   distance_threshold_meter = distance_threshold_meter,
                                   methods = chla_methods)
  
  print("Extracting fdom observations")
  obs_fdom <- extract_observations(fname = cleaned_observations_file_long,
                                   full_time_local,
                                   modeled_depths = modeled_depths,
                                   local_tzone,
                                   target_variable = "fdom",
                                   time_threshold_seconds = time_threshold_seconds_fdom,
                                   distance_threshold_meter = distance_threshold_meter,
                                   methods = fdom_methods)
  
  print("Extracting NH4 observations")
  obs_NH4 <- extract_observations(fname = cleaned_observations_file_long,
                                  full_time_local,
                                  modeled_depths = modeled_depths,
                                  local_tzone,
                                  target_variable = "NH4",
                                  time_threshold_seconds = time_threshold_seconds_nh4,
                                  distance_threshold_meter = distance_threshold_meter,
                                  methods = nh4_methods)
  
  print("Extracting NO3 observations")
  obs_NO3 <- extract_observations(fname = cleaned_observations_file_long,
                                  full_time_local,
                                  modeled_depths = modeled_depths,
                                  local_tzone,
                                  target_variable = "NO3NO2",
                                  time_threshold_seconds = time_threshold_seconds_no3,
                                  distance_threshold_meter = distance_threshold_meter,
                                  methods = no3_methods)
  
  print("Extracting SRP observations")
  obs_SRP <- extract_observations(fname = cleaned_observations_file_long,
                                  full_time_local,
                                  modeled_depths = modeled_depths,
                                  local_tzone,
                                  target_variable = "SRP",
                                  time_threshold_seconds = time_threshold_seconds_srp,
                                  distance_threshold_meter = distance_threshold_meter,
                                  methods = srp_methods)
}

if(length(which(forecasted == 1)) == 16 & forecasted_index[1] == 3){
  
  full_time_local_past <- seq(full_time_local[1] - days(5), full_time_local[2], by = "1 day") # grid
  full_time_local_combined <- seq(full_time_local_past[1], full_time_local[length(full_time_local)], by = "1 day")
  full_time_local_plotting <- seq(full_time_local_past[1]-days(3), full_time_local[length(full_time_local)]+days(5), by = "1 day")
  
  print("Extracting temperature observations")
  obs_temp <- extract_observations(fname = cleaned_observations_file_long,
                                   full_time_local_past,
                                   modeled_depths = modeled_depths,
                                   local_tzone,
                                   target_variable = "temperature",
                                   time_threshold_seconds = time_threshold_seconds_temp,
                                   distance_threshold_meter = distance_threshold_meter,
                                   methods = temp_methods)
  
  
  
  nlayers <- length(depths)
  
  focal_depths <- focal_depths_manager
  png( paste0(save_location,'/',pdf_file_name, "_management.png"),width = 12, height = 6,units = 'in',res=300)
  par(mfrow=c(1,2))
  
  #PLOT OF TURNOVER PROBABILITY
  
  prob_zero <- rep(NA,length(seq(3,18,1)))
  for(i in forecasted_index){
    prob_zero[i-2] = 100*length(which(temp[i,,turnover_index_1] - temp[i,,turnover_index_2] < 1))/length((temp[i,,obs_index[1]]))
  }
  
  plot(full_time_local_plotting,rep(-99,length(full_time_local_plotting)),ylim=c(0,100),xlab = 'date',ylab = '% chance')
  title('Turnover forecast',cex.main=0.9)
  
  points(full_time_local[3:18],prob_zero,type='o',ylim=c(0,100),xlab = 'date',ylab = 'Probablity of turnover')
  axis(1, at=full_time_local_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
  abline(v = full_time_local_past[length(full_time_local_past)])
  text(full_time_local_past[length(full_time_local_past)-2],80,'past')
  text(full_time_local[4],80,'future')
  #HISTORICAL AND FUTURE TEMPERATURE
  if(length(depths) == 10){
    depth_colors <- c("firebrick4",
                      "firebrick1",
                      "DarkOrange1",
                      "gold",
                      "greenyellow",
                      "medium sea green",
                      "sea green",
                      "DeepSkyBlue4",
                      "blue2",
                      "blue4")
    
  }else if(length(depths) == 19){
    depth_colors <- c("firebrick4",
                      NA,
                      "firebrick1",
                      NA,
                      "DarkOrange1",
                      NA,
                      "gold",
                      NA,
                      "greenyellow",
                      NA,
                      "medium sea green",
                      NA,
                      "sea green",
                      NA,
                      "DeepSkyBlue4",
                      NA,
                      "blue2",
                      NA,
                      "blue4")
  }else{
    depth_colors <- c("firebrick4",
                      NA,
                      NA,
                      "firebrick1",
                      NA,
                      NA,
                      "DarkOrange1",
                      NA,
                      NA,
                      "gold",
                      NA,
                      NA,
                      "greenyellow",
                      NA,
                      NA,
                      "medium sea green",
                      NA,
                      NA,
                      "sea green",
                      NA,
                      NA,
                      "DeepSkyBlue4",
                      NA,
                      NA,
                      "blue2",
                      NA,
                      NA,
                      "blue4",
                      NA)
  }
  
  plot(full_time_local_plotting,rep(-99,length(full_time_local_plotting)),ylim=c(-5,35),xlab = 'date',ylab = expression(~degree~C))
  title(paste0('Water temperature forecast'),cex.main=0.9)
  tmp_day <- full_time_local[-1][1]
  axis(1, at=full_time_local_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
  depth_colors_index = 0
  for(i in 1:length(depths)){
    if(length(which(depths[i]  == modeled_depths)) >= 1 ){
      depth_colors_index <- i
      points(full_time_local_past, obs_temp[1:length(full_time_local_past),i],type='l',col=depth_colors[depth_colors_index],lwd=1.5)
      index <- which(focal_depths == obs_index[i])
      if(length(index) == 1){
        points(full_time_local[-1], temp_mean[-1,i],type='l',lty='dashed',col=depth_colors[depth_colors_index],lwd=1.5)
        points(full_time_local[-1], temp_upper[-1,i],type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=1.5)
        points(full_time_local[-1], temp_lower[-1,i],type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=1.5)
      }
    }
  }
  
  abline(v = full_time_local_past[length(full_time_local_past)])
  text(full_time_local_past[length(full_time_local_past)-2],30,'past')
  text(full_time_local[4],30.1,'future')
  if(temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]] == temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]] |
     temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]] == temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]] |
     temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]] == temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]]){
    text(full_time_local_plotting[length(full_time_local_plotting)-3], temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]], '1m:1660', col='firebrick1')
    text(full_time_local_plotting[length(full_time_local_plotting)-2], temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]], '5m:1647', col='medium sea green')
    text(full_time_local_plotting[length(full_time_local_plotting)-1], temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]], '8m:1637', col='blue2')
  }else{
    text(full_time_local_plotting[length(full_time_local_plotting)-2], temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]], '1m:1660', col='firebrick1')
    text(full_time_local_plotting[length(full_time_local_plotting)-2], temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]], '5m:1647', col='medium sea green')
    text(full_time_local_plotting[length(full_time_local_plotting)-2], temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]], '8m:1637', col='blue2')
  }
  
  legend("left",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m","8m", "9m"),
         text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                    "DeepSkyBlue4", "blue2", "blue4"), cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
  legend('topright', c('mean','confidence bounds'), lwd=1.5, lty=c('dashed','dotted'),bty='n',cex = 1)
  
  mtext(paste0('Falling Creek Reservoir\n',month(tmp_day),'/',day(tmp_day),'/',year(tmp_day)), side = 3, line = -2, outer = TRUE, font = 2)
  dev.off()