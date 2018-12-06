library(mvtnorm)
library(ncdf4)
library(lubridate)
library(testit)
if (!"glmtools" %in% installed.packages()) install.packages('glmtools', repos=c('http://cran.rstudio.com', 'http://owi.usgs.gov/R'))
library(glmtools)

first_day <- '2013-05-14 00:00:00'
last_day <- '2014-11-13 00:00:00'
reference_tzone <- 'GMT'
sim_name <- 'prediction'
hist_days <- as.numeric(as.POSIXct(last_day)-as.POSIXct(first_day))
forecast_days <- 0
restart_file <- NA
Folder <- '/Users/quinn/Dropbox/Research/SSC_forecasting/SSC_forecasting/'
machine <- 'mac'
data_location <- '/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_Data/'
PRE_SCC <- TRUE

###RUN OPTIONS
#Folder <- '/Users/quinn/Dropbox/Research/SSC_forecasting/SSC_forecasting/'
nEnKFmembers <- 1
nMETmembers <- 1
nmembers = nEnKFmembers*nMETmembers

use_CTD <- FALSE
include_wq <- TRUE
NO_UNCERT <- FALSE
ADD_NOISE_TO_OBS <- FALSE
USE_OBS_DEPTHS <- FALSE
USE_OBS_CONTRAINT <- TRUE
UPDATE_STATES_W_OBS <- FALSE

###CREATE TIME VECTOR
begin_sim  <- as.POSIXct(first_day,tz = reference_tzone)
total_days <- hist_days + forecast_days
end_sim <- begin_sim + total_days*24*60*60
start_forecast_step <- hist_days
full_time <- seq(begin_sim, end_sim, by = "1 day") # grid
full_time_local <- with_tz(full_time,tzone = 'EST5EDT')
full_time <- strftime(full_time, format="%Y-%m-%d %H:%M",tz = reference_tzone)
full_time_local <- strftime(full_time_local, format="%Y-%m-%d %H:%M")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
full_time_day_local <- strftime(full_time_local, format="%Y-%m-%d")
full_time_hour_obs <- seq(as.POSIXct(full_time[1],tz = reference_tzone), as.POSIXct(full_time[length(full_time)],tz = reference_tzone), by = "1 hour") # grid
nsteps <- length(full_time)


###CREATE DIRECTORY PATHS AND STRUCTURE
workingGLM <- paste0(Folder,'GLM_working/')  
print(workingGLM)
unlink(paste0(workingGLM,'*'),recursive = FALSE)    #Clear out temp GLM working directory

###LOAD SHARE R FUNCTIONS
source(paste0(Folder,'Rscripts/mcmc_enkf_shared_functions.R'))
source(paste0(Folder,'Rscripts/create_obs_met_input.R'))
source(paste0(Folder,'Rscripts/extract_temp_chain.R'))
source(paste0(Folder,'Rscripts/process_GEFS2GLM_v2.R'))
source(paste0(Folder,'Rscripts/extract_temp_CTD.R'))
source(paste0(Folder,'Rscripts/create_inflow_outflow_file.R'))
source(paste0(Folder,'Rscripts/plot_forecast.R'))
source(paste0(Folder,'Rscripts/archive_forecast.R'))

###SHARED GLM LIBRARIES
#Sys.setenv(DYLD_FALLBACK_LIBRARY_PATH= paste(pathGLM,'/glm_lib_files/',sep=''))
#Sys.setenv(PATH='/opt/local/bin:/opt/local/sbin:/Users/quinn/anaconda2/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/local/bin')
#system(paste('export DYLD_FALLBACK_LIBRARY_PATH=~',pathGLM,'/glm_lib_files:$DYLD_FALLBACK_LIBRARY_PATH',sep=''))

###SET FILE NAMES
catwalk_fname <-  paste0(workingGLM,'Catwalk.csv')
met_obs_fname <-paste0(workingGLM,'FCRmet.csv')
#ctd_fname <- '/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/test_data/070218_fcr50.csv' 

if(is.na(sim_name)){
  sim_name <- paste0(year(full_time[1]),'_',month(full_time[1]),'_',day(full_time[1]))
  #paste0('historical_start_',year(first_day),'_',month(first_day),'_',day(first_day),'_forecast_start_',paste0(year(forecast_start_time),forecast_month,forecast_day))
}

###DOWNLOAD FILES TO WORKING DIRECTORY
mia_location <- paste0(data_location,'/','mia-data')
setwd(mia_location)
system(paste0('git pull'))
carina_location <- paste0(data_location,'/','carina-data')
setwd(carina_location)
system(paste0('git pull'))
noaa_location <- paste0(data_location,'/','noaa-data')
setwd(noaa_location)
system(paste0('git pull'))

#download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv',paste0(workingGLM,'/','FCRmet.csv'))
#download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv',paste0(workingGLM,'/','Catwalk.csv'))
#download.file(paste0('https://github.com/CareyLabVT/SCCData/raw/noaa-data/',forecast_base_name,'.csv'),paste0(workingGLM,'/',forecast_base_name,'.csv'))

met_obs_fname <- paste0(carina_location,'/FCRmet.csv')
###CREATE HISTORICAL MET FILE
obs_met_outfile <- paste0(workingGLM,'/','GLM_met.csv')
create_obs_met_input(fname = met_obs_fname,outfile=obs_met_outfile,full_time_hour_obs, input_tz = 'EST5EDT', output_tz = reference_tzone)
if(PRE_SCC){
  fl <- c(list.files(paste0(data_location,'/PreSCC/'), full.names = TRUE))
  file.copy(from = fl, to = workingGLM,overwrite = TRUE)
  obs_met_outfile = paste0(workingGLM,'/','FCR_GLM_met_NLDAS2_010113_010118_GMTadjusted.csv')
}

###MOVE FILES AROUND
SimFilesFolder <- paste0(Folder,'sim_files/')
if(machine == 'mac'){
  GLM_folder <-  file.path(Folder,'glm/mac/') 
}else if(machine == 'unix'){
  GLM_folder <- file.path(Folder,'glm/unix/') 
}
fl <- c(list.files(SimFilesFolder, full.names = TRUE))
tmp <- file.copy(from = fl, to = workingGLM,overwrite = TRUE)
fl <- c(list.files(GLM_folder, full.names = TRUE))
tmp <- file.copy(from = fl, to = workingGLM,overwrite = TRUE)
if(!is.na(restart_file)){
  tmp <- file.copy(from = restart_file, to = workingGLM,overwrite = TRUE)
}

create_inflow_outflow_file(full_time,workingGLM)

if(include_wq){
  if(PRE_SCC){
    file.copy(from = paste0(workingGLM,'glm3_wAED_preSCC.nml'), to = paste0(workingGLM,'glm3.nml'),overwrite = TRUE)
  }else{
  file.copy(from = paste0(workingGLM,'glm3_wAED.nml'), to = paste0(workingGLM,'glm3.nml'),overwrite = TRUE)
  }
}else{
  file.copy(from = paste0(workingGLM,'glm3_woAED.nml'), to = paste0(workingGLM,'glm3.nml'),overwrite = TRUE)
}

###SET UP RUN
OXY_oxy_init <- 300.62
CAR_pH_init <- 6.5
CAR_dic_init <- 59.1
CAR_ch4_init <- 0.58
SIL_rsi_init <- 300
NIT_amm_init <- 0.69
NIT_nit_init <- 0.05
PHS_frp_init <- 0.07
OGM_doc_init <- 47.4
OGM_poc_init <- 78.5
OGM_don_init <- 1.3
OGM_pon_init <- 8.3
OGM_dop_init <- 1.5
OGM_pop_init <- 8.3
OGM_docr_init <- 350.00
OGM_donr_init <- 13.0
OGM_dopr_init <- 3.0
OGM_cpom_init <- 100.00
PHY_CYANOPCH1_init <- 2.0
PHY_CYANONPCH2_init <-2.0
PHY_CHLOROPCH3_init <-2.0
PHY_DIATOMPCH4_init <- 2.0
ZOO_COPEPODS1_init <- 2.9
ZOO_DAPHNIABIG2_init <- 4.3
ZOO_DAPHNIASMALL3_init <- 40


wq_names <- c('OXY_oxy',
              'CAR_pH','CAR_dic','CAR_ch4',
              'SIL_rsi',
              'NIT_amm', 'NIT_nit',
              'PHS_frp',
              'OGM_doc','OGM_poc','OGM_don','OGM_pon','OGM_dop','OGM_pop',
              'PHY_CYANOPCH1','PHY_CYANONPCH2','PHY_CHLOROPCH3','PHY_DIATOMPCH4',
              'ZOO_COPEPODS1','ZOO_DAPHNIABIG2','ZOO_DAPHNIASMALL3')

#wq_names <- c('OXY_oxy',
#              'CAR_pH','CAR_dic','CAR_ch4', 
#              'SIL_rsi',
#              'NIT_amm', 'NIT_nit',
#              'PHS_frp',
#              'OGM_doc','OGM_poc','OGM_don','OGM_pon','OGM_dop','OGM_pop',  #'OGM_docr', 'OGM_donr', 'OGM_dopr','OGM_cpom', 
#              'PHY_','PHY_CYANONPCH2','PHY_CHLOROPCH3','PHY_DIATOMPCH4')
num_wq_vars <- length(wq_names) 

if(include_wq){
  glm_output_vars <- c('temp',wq_names)
}else{
  glm_output_vars <- c('temp')
}

the_sals_init <- 0.0


#Parameters
Kw <- 0.86
#coef_mix_conv <- 0.2
#coef_wind_stir <- 0.23
#coef_mix_shear <- 0.2
#coef_mix_turb <- 0.51
#coef_mix_KH <- 0.3
#coef_mix_hyp <- 0.5
#wind_factor <- 0.5
sw_factor <- 0.95
lw_factor <- 0.95
#at_factor <- 1
#rh_factor <- 1
#rain_factor <- 1
#cd <- 0.0013
#ce <- 0.0013
#ch <- 0.0013

lake_depth_init <- 9.4

the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)


if(!PRE_SCC){
  catwalk_fname <- paste0(mia_location,'/','Catwalk.csv')
  #PROCESS TEMPERATURE OBSERVATIONS
  obs_temp <- extract_temp_chain(fname = catwalk_fname,full_time,input_tz = 'EST5EDT', output_tz = reference_tzone)
  for(i in 1:length(obs_temp$obs[,1])){
    for(j in 1:length(obs_temp$obs[1,])){
      if(obs_temp$obs[i,j] == 0 | is.na(obs_temp$obs[i,j]) | is.nan(obs_temp$obs[i,j])){
        obs_temp$obs[i,j] = NA
      } 
    }
  }
  TempObservedDepths <- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8,9)
  init_temps1 <- obs_temp$obs[1,]
  
  #PROCESS DO OBSERVATIONS
  DoObservedDepths <- c(1,5,9)
  obs_do <- extract_do_chain(fname = catwalk_fname,full_time,input_tz = 'EST5EDT', output_tz = reference_tzone)
  #mg/L (obs) -> mol/m3 * 31.25
  
}else{
  fname <- paste0('/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SCC_data/preSCC/CTD_Meta_13_17.csv')
  obs_temp <- extract_temp_CTD(fname,full_time_day,depths = the_depths_init,input_tz = 'EST5EDT', output_tz = reference_tzone)
  TempObservedDepths <- the_depths_init
  init_temps1 <- obs_temp$obs[1,]
  DoObservedDepths <- c(1,5,9)
}


temp_inter <- approxfun(TempObservedDepths,init_temps1,rule=2)

#SET UP INITIAL CONDITIONS
if(USE_OBS_DEPTHS){
  nlayers_init <- length(TempObservedDepths)
  the_depths_init <- TempObservedDepths
  the_temps_init <- init_temps1
}else{
  the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)
  nlayers_init <- length(the_depths_init)
  the_temps_init <- temp_inter(the_depths_init)
  do_init <- rep(NA,length(the_depths_init))
  #do_init[1:13] <- obs_do$obs[1,1]
  #do_init[14:23] <- obs_do$obs[1,2]
  #do_init[24:29] <- obs_do$obs[1,3]
}


the_temps_init <- temp_inter(the_depths_init)

temp_start <- 1
temp_end <- length(the_depths_init)
wq_start <- rep(NA,num_wq_vars)
wq_end <- rep(NA,num_wq_vars)
for(wq in 1:num_wq_vars){
  if(wq == 1){
    wq_start[wq] <- temp_end+1
    wq_end[wq] <- temp_end + (length(the_depths_init))
  }else{
    wq_start[wq] <- wq_end[wq-1]+1
    wq_end[wq] <- wq_end[wq-1] + (length(the_depths_init))
  }
}



#UPDATE NML WITH PARAMETERS AND INITIAL CONDITIONS
OXY_oxy_init_depth <- rep(OXY_oxy_init,nlayers_init)
CAR_pH_init_depth <- rep(CAR_pH_init,nlayers_init)
CAR_dic_init_depth <- rep(CAR_dic_init,nlayers_init)
CAR_ch4_init_depth <- rep(CAR_ch4_init,nlayers_init)
SIL_rsi_init_depth <- rep(SIL_rsi_init,nlayers_init)
NIT_amm_init_depth <- rep(NIT_amm_init,nlayers_init)
NIT_nit_init_depth <- rep(NIT_nit_init,nlayers_init)
PHS_frp_init_depth <- rep(PHS_frp_init,nlayers_init)
OGM_doc_init_depth <- rep(OGM_doc_init,nlayers_init)
OGM_poc_init_depth <- rep(OGM_poc_init,nlayers_init)
OGM_don_init_depth <- rep(OGM_don_init,nlayers_init)
OGM_pon_init_depth <- rep(OGM_pon_init,nlayers_init)
OGM_dop_init_depth <- rep(OGM_dop_init,nlayers_init)
OGM_pop_init_depth <- rep(OGM_pop_init,nlayers_init)
#OGM_docr_init_depth <- rep(OGM_docr_init,nlayers_init)
#OGM_donr_init_depth <- rep(OGM_donr_init,nlayers_init)
#OGM_dopr_init_depth <- rep(OGM_dopr_init,nlayers_init)
#OGM_cpom_init_depth <- rep(OGM_cpom_init,nlayers_init)
PHY_CYANOPCH1_init_depth <- rep(PHY_CYANOPCH1_init,nlayers_init)
PHY_CYANONPCH2_init_depth <- rep(PHY_CYANONPCH2_init,nlayers_init)
PHY_CHLOROPCH3_init_depth <- rep(PHY_CHLOROPCH3_init,nlayers_init)
PHY_DIATOMPCH4_init_depth <- rep(PHY_DIATOMPCH4_init,nlayers_init)
ZOO_COPEPODS1_init_depth <- rep(ZOO_COPEPODS1_init,nlayers_init)
ZOO_DAPHNIABIG2_init_depth <- rep(ZOO_DAPHNIABIG2_init,nlayers_init)
ZOO_DAPHNIASMALL3_init_depth <- rep(ZOO_DAPHNIASMALL3_init,nlayers_init)

#PHY_CYANOPCH1_init_depth[2:nlayers_init] <- 0
#PHY_CYANOPCH1_init_depth[1] <- 100 

wq_init_vals <- c(OXY_oxy_init_depth,
                  CAR_pH_init_depth,
                  CAR_dic_init_depth,
                  CAR_ch4_init_depth,
                  SIL_rsi_init_depth,
                  NIT_amm_init_depth,
                  NIT_nit_init_depth,
                  PHS_frp_init_depth,
                  OGM_doc_init_depth,
                  OGM_poc_init_depth,
                  OGM_don_init_depth,
                  OGM_pon_init_depth,
                  OGM_dop_init_depth,
                  OGM_pop_init_depth,
                  #OGM_docr_init_depth,
                  #OGM_donr_init_depth,
                  #OGM_dopr_init_depth,
                  #OGM_cpom_init_depth,
                  PHY_CYANOPCH1_init_depth,
                  PHY_CYANONPCH2_init_depth,
                  PHY_CHLOROPCH3_init_depth,
                  PHY_DIATOMPCH4_init_depth,
                  ZOO_COPEPODS1_init_depth,
                  ZOO_DAPHNIABIG2_init_depth,
                  ZOO_DAPHNIASMALL3_init_depth
)

#UPDATE NML WITH PARAMETERS AND INITIAL CONDITIONS
update_var(wq_init_vals,'wq_init_vals',workingGLM)
update_var(num_wq_vars,'num_wq_vars',workingGLM)
update_var(rep(the_sals_init,nlayers_init),'the_sals',workingGLM)
update_var(lake_depth_init,'lake_depth',workingGLM)
update_var(nlayers_init,'num_depths',workingGLM)
update_var(the_temps_init,'the_temps',workingGLM)
update_var(the_depths_init,'the_depths',workingGLM)

update_var(Kw,'Kw',workingGLM)
#update_var(coef_mix_conv,'coef_mix_conv',workingGLM)
#update_var(coef_wind_stir,'coef_wind_stir',workingGLM)
#update_var(coef_mix_shear,'coef_mix_shear',workingGLM)
#update_var(coef_mix_turb,'coef_mix_turb',workingGLM)
#update_var(coef_mix_KH,'coef_mix_KH',workingGLM)
#update_var(coef_mix_hyp,'coef_mix_hyp',workingGLM)
#update_var(wind_factor,'wind_factor',workingGLM)
update_var(sw_factor,'sw_factor',workingGLM)
update_var(lw_factor,'lw_factor',workingGLM)
#update_var(at_factor,'at_factor',workingGLM)
#update_var(rh_factor,'rh_factor',workingGLM)
#update_var(rain_factor,'rain_factor',workingGLM)
#update_var(cd,'cd',workingGLM)
#update_var(ce,'ce',workingGLM)
#update_var(ch,'ch',workingGLM)

#NUMBER OF STATE SIMULATED = SPECIFIED DEPTHS
if(include_wq){
  nstates <- nlayers_init*(1+num_wq_vars)
}else{
  nstates <- nlayers_init 
}

if(include_wq){
  nobs <- length(TempObservedDepths) + length(DoObservedDepths)
}else{
  nobs <- length(TempObservedDepths)
}

#Observations for each observed state at each time step
#an observation with at least 1 observation but without an observation in a time-step gets assigned an NA
z <- t(matrix(rep(NA,nobs), nrow = nobs, ncol = nsteps))
if(include_wq){
  #z <- cbind(obs_temp$obs,obs_do$obs)
  z <- cbind(obs_temp$obs) 
}else{
  z <- cbind(obs_temp$obs) 
}

z_obs <- z
if(!USE_OBS_CONTRAINT){
  z[,] <- NA
}


#FIGURE OUT WHICH DEPTHS HAVE OBSERVATIONS
if(include_wq){
  obs_index <- rep(NA,length(TempObservedDepths)+length(DoObservedDepths))
  for(i in 1:length(TempObservedDepths)){
    obs_index[i] <- which.min(abs(the_depths_init - TempObservedDepths[i]))
  }
  for(i in 1:length(DoObservedDepths)){
    obs_index[length(TempObservedDepths)+i] <- length(the_depths_init) + which.min(abs(the_depths_init - DoObservedDepths[i]))
  }
  
  obs_index <- rep(NA,length(TempObservedDepths))
  for(i in 1:length(TempObservedDepths)){
    obs_index[i] <- which.min(abs(the_depths_init - TempObservedDepths[i]))
  }

}else{
  obs_index <- rep(NA,length(TempObservedDepths))
  for(i in 1:length(TempObservedDepths)){
    obs_index[i] <- which.min(abs(the_depths_init - TempObservedDepths[i]))
  } 
}

#Matrix for knowing which state the observation corresponds to
z_states <- t(matrix(obs_index, nrow = length(obs_index), ncol = nsteps))

### INITILIZE FIRST TIME STEP
restart_present <- FALSE
if(!is.na(restart_file)){
  if(file.exists(restart_file)){
    restart_present <- TRUE
  }
}
x <- array(NA,dim=c(nsteps,nstates))


#Set initial conditions
if(include_wq){
  x[1,] <- c(the_temps_init,wq_init_vals)
}else{
  x[1,] <- c(the_temps_init)
}

surface_height <- array(NA,dim=c(nsteps))
surface_height[1] <- lake_depth_init

file.copy(from = paste0(workingGLM,'glm3.nml'), to = paste0(workingGLM,'glm3_initial.nml'),overwrite = TRUE)

met_index <- 1
model_obs_array = array(NA,c(3,nsteps,nstates))  
ndays = 0
update_var(obs_met_outfile,'meteo_fl',workingGLM)


if(!PRE_SCC){
  update_var(paste0('FCR_inflow.csv'),'inflow_fl',workingGLM)
  update_var(paste0('FCR_spillway_outflow.csv'),'outflow_fl',workingGLM)
}else{
  #update_var(paste0('FCR_inflow.csv'),'inflow_fl',workingGLM)
  #update_var(paste0('FCR_spillway_outflow.csv'),'outflow_fl',workingGLM)
}

model_obs_array[1,1,] = x[1,]
model_obs_array[2,1,obs_index] = z[1,]
model_obs_array[3,1,] = 0
for(i in 2:nsteps){
  
  #1) Update GLM NML files to match the current day of the simulation
  curr_start <- (full_time[i-1])
  curr_stop <- (full_time[i])
  update_time(start_value  = curr_start, stop_value = curr_stop,workingGLM)
  setwd(workingGLM)
  
  #Create array to hold GLM predictions for each ensemble
  x_star <- array(NA, dim = c(nstates))
  
  #2) Use x[i-1,m,] to update GLM NML files for initial temperature at each depth
  tmp <- update_temps(curr_temps = round(x[i-1,temp_start:temp_end],3),the_depths_init,workingGLM)
  update_var(round(surface_height[i-1],3),'lake_depth',workingGLM)
  #print(x[i-1,temp_start:temp_end])
  
  if(include_wq){
    wq_init_vals <- round(c(x[i-1,wq_start[1]:wq_end[num_wq_vars]]),3)
    update_var(wq_init_vals,'wq_init_vals',workingGLM)
  }else{
    
    
  }
  
  #3) Use GLM NML files to run GLM for a day
  #if(machine == 'mac'){
  #  system(paste0(workingGLM,"glm"))
  #}else if(machine == 'unix'){
  #  system(paste0(workingGLM,"glm"))
  #}
  pass <- FALSE
  num_reruns <- 0
  while(!pass){
    
    unlink(paste0(workingGLM,'/output.nc')) 
    system(paste0(workingGLM,"glm"))
    #GLM_temp_wq_out <- get_glm_nc_var_all_wq(ncFile = 'output.nc',z_out = the_depths_init,vars = glm_output_vars)
    
    #4) Fill x_star with temperatures from GLM
    #GLM_sals = get_glm_nc_var(ncFile = 'output.nc',z_out = the_depths_init, var = 'temp')
    
    
    
    if(file.exists(paste0(workingGLM,'/output.nc')) & !has_error(nc_open('output.nc'))){
      if(length(ncvar_get(nc_open('output.nc'),'time')) > 1){
      if(include_wq){
        GLM_temp_wq_out <- get_glm_nc_var_all_wq(ncFile = 'output.nc',z_out = the_depths_init,vars = glm_output_vars)
        x_star <- c(GLM_temp_wq_out$output)
        surface_height[i] <- GLM_temp_wq_out$surface_height 
      }else{
        GLMtemps <- get_glm_nc_var(ncFile = 'output.nc',z_out = the_depths_init, var = 'temp')
        x_star[temp_start:temp_end] <- GLMtemps 
        surface_height[i] <- GLM_temp_wq_out$surface_height 
      }
      if(length(which(is.na(x_star)))==0){
        pass = TRUE
      }else{
        num_reruns <- num_reruns + 1
      }
      }
    }
    if(num_reruns > 1000){
      print(paste0('Too many re-runs (> 1000) due to NaN values in output'))
    }
  }
  
  met_index <- 1
  
  #Obs for time step
  z_index = which(!is.na(z[i,]))
  
  #if no observations at a time step then just propogate model uncertainity
  if(length(z_index) == 0){
    x[i,] = x_star
    if(UPDATE_STATES_W_OBS){
      ndays = ndays+1
    }else{
      ndays = 1
    }
    model_obs_array[1,i,] = x_star
  }else{
    zt = z[i,z_index]
    z_states_t = z_states[i,z_index]
    temp1 = zt
    model = x_star[z_states_t]
    obs = zt
    
    model_obs_array[1,i,] = x_star
    model_obs_array[2,i,z_states_t] = z[i,z_index]
    model_obs_array[3,i,] = ndays
    
    if(UPDATE_STATES_W_OBS & !include_wq){
      temp_inter <- approxfun(TempObservedDepths,z[i,z_index],rule=2)
      the_temps_init <- temp_inter(the_depths_init)
      x[i,] =  the_temps_init
    }else{
      x[i,] =  x_star
    }
    ndays = 1
  }
}


plot(model_obs_array[1,,1])
points(model_obs_array[2,,1],col='red')

plot(model_obs_array[1,,1])
points(model_obs_array[2,,1],col='red')

i = 1
z_index = 1
plot(model_obs_array[1,,z_states[1,z_index]],type = 'o',ylim=c(5,35))
points(model_obs_array[2,,z_states[1,z_index]],col='red',type = 'o')
for(z_index in 1:10){
  points(model_obs_array[1,,z_states[i,z_index]],col='black',type = 'o')
  points(model_obs_array[2,,z_states[i,z_index]],col='red',type = 'o')
}
i = 2
for(z_index in 1:10){
  print(sqrt(mean((model_obs_array[1,,z_states[i,z_index]] - model_obs_array[2,,z_states[i,z_index]])^2,na.rm = TRUE)))
}

pdf('AED_test.pdf',width = 7, height = 7)
par(mfrow=c(3,3))
for(i in 1:length(wq_names)){
focal_wq_var <- i
lim<- range(x[,wq_start[focal_wq_var]:wq_end[focal_wq_var]],na.rm = TRUE)
plot(as.POSIXct(full_time),x[,wq_start[focal_wq_var]+which(the_depths_init == 0.1)-1],type='l',ylim=lim,col='black',main=wq_names[focal_wq_var],ylab = 'GLM units',xlab='day')
points(as.POSIXct(full_time),x[,wq_start[focal_wq_var]+which(the_depths_init == 1)-1],type='l',col='green',ylab = 'GLM units',xlab='day')
points(as.POSIXct(full_time),x[,wq_start[focal_wq_var]+which(the_depths_init == 2)-1],type='l',col='brown',ylab = 'GLM units',xlab='day')
#points(as.POSIXct(full_time),x[,wq_start[1]+which(the_depths_init == 3)-1],type='l')
#points(as.POSIXct(full_time),x[,wq_start[1]+which(the_depths_init == 4)-1],type='l')
#points(as.POSIXct(full_time),x[,wq_start[1]+which(the_depths_init == 5)-1],type='l')
#points(as.POSIXct(full_time),x[,wq_start[1]+which(the_depths_init == 6)-1],type='l')
#points(as.POSIXct(full_time),x[,wq_start[1]+which(the_depths_init == 7)-1],type='l')
points(as.POSIXct(full_time),x[,wq_start[focal_wq_var]+which(the_depths_init == 8)-1],type='l',col='orange',ylab = 'GLM units',xlab='day')
#points(as.POSIXct(full_time),x[,wq_start[1]+which(the_depths_init == 9)-1],type='l')
legend('bottomright',c('0.1 m','1 m','2 m','8 m'),lty=c(1,1,1,1),col=c('black','green','brown','orange'),bty='n')
}
dev.off()