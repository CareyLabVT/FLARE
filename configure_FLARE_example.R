##########################
# Lake information
###########################

lake_name <<- "FCR"
lake_latitude <<- 37.307   #Degrees North 
lake_longitude <<- 79.837  #Degrees West

#Time zone that GLM is run in
#Currently needed to be GMT so that it interfaces with the NOAA forecast
#reference_tzone <<- "GMT"
#Local time zone of the lake
local_tzone <<- "EST"

########################################
## Temperature only or include water quality
#########################################
include_wq <<- TRUE
#TRUE = use AED

##########################
# Management Options
###########################
simulate_SSS <<- TRUE
#Include SSS in data assimilation

forecast_no_SSS <<- TRUE
#Run forecasts without SSS turned off

forecast_SSS <<- TRUE
#Run forecasts without SSS turned on
forecast_SSS_flow <<- 1000
#m3/day rate of flow if SSS turned on in forecast
forecast_SSS_Oxy <<- 500
#umol/m3  of oxygen if SSS turned on in forecast



#####################
# Weather forcing options
######################
use_future_met <<- TRUE
#TRUE = use NOAA forecast for "Future"
#FALSE = use observed weather for "Future"; only works if "forecasting" past dates

DOWNSCALE_MET <<- TRUE
#Downscale the coarse resolutoin NOAA data to the local
#site using the meterology station at the lake

downscaling_coeff <<- NA
#file name of previous downscaling coefficients
#use NA if not using an existing file

met_ds_obs_start <<- as.Date("2018-04-06")
met_ds_obs_end <<- as.Date("2018-12-06")
#Dates to use to developing the downscaling coefficient

############################
# Run information
#############################

GLMversion <<- "GLM 3.0.0beta10"
FLAREversion <<- "v1.0_beta.1"
#GLM and FLARE version; the code adds these to the output files

#GLM NML used as base 
if(include_wq){
  if(simulate_SSS){
    base_GLM_nml <<- "glm3_wAED_SSS.nml"
  }else{
    base_GLM_nml <<- "glm3_wAED.nml"  
  }
}else{
  base_GLM_nml <<- "glm3_w0AED.nml" 
}

#################################
### Uncertainty simulated
################################

uncert_mode <<- 1
#Choose the types of uncertainty include in foreast
#1 = all types
#2 = no uncertainty
#3 = only process uncertainty
#4 = only NOAA weather forecast uncertainty
#5 = only initial condition uncertainty
#6 = only initial condition uncertainty and no state updating  with EnKF
#7 = only parameter uncertainty
#8 = only meteorology downscaling uncertainty
#9 = no sources of uncertainty and no state updating with EnKF

single_run <<- FALSE
#Removes uncertainty and only simulates 3 ensemble members

#########################
### Depth information
#########################
#Depths used in the EnKF
#This are the depths that are saved between days
#Init depth of lake
lake_depth_init <<- 9.4  #not a modeled state

if(!include_wq){
  modeled_depths <<- c(0.1, 0.33, 0.66, 
                       1.00, 1.33, 1.66,
                       2.00, 2.33, 2.66,
                       3.0, 3.33, 3.66,
                       4.0, 4.33, 4.66,
                       5.0, 5.33, 5.66,
                       6.0, 6.33, 6.66,
                       7.00, 7.33, 7.66,
                       8.0, 8.33, 8.66,
                       9.00)
  #modeled_depths <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
}else{
  modeled_depths <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
  #modeled_depths <<- c(0.1, 0.33, 0.66, 
  #                     1.00, 1.33, 1.66,
  #                     2.00, 2.33, 2.66,
  #                     3.0, 3.33, 3.66,
  #                     4.0, 4.33, 4.66,
  #                     5.0, 5.33, 5.66,
  #                     6.0, 6.33, 6.66,
  #                     7.00, 7.33, 7.66,
  #                     8.0, 8.33, 8.66,
  #                     9.00)
}

##############################
##  Ensemble members used
##############################
n_enkf_members <<- 1
n_ds_members <<- 5
n_inflow_outflow_members <<- 1
#Note: this number is multiplied by 
# 1) the number of NOAA ensembles (21)
# 2) the number of downscaling essembles (50 is current)
# get to the total number of essembles

################################
### Process uncertainty adaption
##################################
qt_alpha <<- 0.8  #0 - all weight on the new Qt, 1 - all weight on the current Qt
qt_beta <<- 0.7 # 


#################################
# Parameter calibration information
#################################

include_pars_in_qt_update <<- TRUE
#Adapt the parameter noise

#Initial zone temperatures and the upper and lower bounds
#Zone 1 is Xm to Xm 
#Zone 2 is Xm to Xm
zone1_temp_init_mean <<- 10
zone1_temp_init_lowerbound <<- 4
zone1_temp_init_upperbound <<- 15
#daily perturbance of parameter value
zone1temp_init_qt <<- 0.1^2  #THIS IS THE VARIANCE, NOT THE SD

zone2_temp_init_mean <<- 10 #11
zone2_temp_init_lowerbound <<-  4
zone2_temp_init_upperbound <<-  15
zone2temp_init_qt <<- 0.1^2 #THIS IS THE VARIANCE, NOT THE SD

#Shortwave factor
swf_init_mean <<- 1.0
swf_init_lowerbound <<- 0.5
swf_init_upperbound <<- 1.75
#daily perturbance of parameter value
swf_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD

#Wind factor
windf_init_mean <<- 1.0
windf_init_lowerbound <<- 0.5
windf_init_upperbound <<- 1.5
#daily perturbance of parameter value
windf_init_qt <<- 0.001^2 #THIS IS THE VARIANCE, NOT THE SD

#Fsed_oxy
Fsed_oxy_init_mean <<- -24.219
Fsed_oxy_init_lowerbound <<-  -100 #-30 #-24.220
Fsed_oxy_init_upperbound <<-  -10 #10 #-24.218
#daily perturbance of parameter value
Fsed_oxy_init_qt <<- 0.1^2 #THIS IS THE VARIANCE, NOT THE SD

#Rdom_minerl
Rdom_minerl_init_mean <<- 0.0005
Rdom_minerl_init_lowerbound <<-  0.0001 #0.00049
Rdom_minerl_init_upperbound <<-  0.001 #0.00051
#daily perturbance of parameter value
Rdom_minerl_init_qt <<- 0.000001^2 #THIS IS THE VARIANCE, NOT THE SD

#R_growth
R_growth_init_mean <<- 1
R_growth_init_lowerbound <<-  0.25 #0.99
R_growth_init_upperbound <<-  2.00 #1.01
#daily perturbance of parameter value
R_growth_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD

#R_resp
R_resp_init_mean <<- 0.009399413
R_resp_init_lowerbound <<-  0.001 #0.99
R_resp_init_upperbound <<-  0.015 #1.01
#daily perturbance of parameter value
R_resp_init_qt <<- 0.001^2 #THIS IS THE VARIANCE, NOT THE SD

#Create parameter vectors
if(include_wq){
  par_names <<- c()
  par_names_save <<- c()
  par_nml <<- c()
  par_init_mean <<- c()
  par_init_lowerbound <<- c()
  par_init_upperbound <<- c()
  par_init_qt <<- c()
  par_units <<- c() #
  # par_names <<- c("sed_temp_mean","Fsed_oxy")
  # par_names_save <<- c("zone1temp","Fsed_oxy")
  # par_nml <<- c("glm3.nml","aed2.nml") #Fsed_oxy_init_mean,Rdom_minerl_init_mean,Rdom_minerl_init_mean,R_growth_init_mean)
  # par_init_mean <<- c(zone1_temp_init_mean,Fsed_oxy_init_mean)
  # par_init_lowerbound <<- c(zone1_temp_init_lowerbound,Fsed_oxy_init_lowerbound) #Fsed_oxy_init_lowerbound,Rdom_minerl_init_lowerbound,R_growth_init_lowerbound)
  # par_init_upperbound <<- c(zone1_temp_init_upperbound,Fsed_oxy_init_upperbound)#Fsed_oxy_init_upperbound,Rdom_minerl_init_upperbound,R_growth_init_upperbound)
  # par_init_qt <<- c(zone1temp_init_qt,Fsed_oxy_init_qt)#Fsed_oxy_init_qt,Rdom_minerl_init_qt,R_growth_init_qt)
  # par_units <<- c("deg_C","-") #
  
  par_names <<- c("sed_temp_mean","sed_temp_mean","Fsed_oxy","sw_factor")
  par_names_save <<- c("zone1temp","zone2temp","Fsed_oxy","sw_factor")
  par_nml <<- c("glm3.nml","glm3.nml","aed2.nml","glm3.nml") #Fsed_oxy_init_mean,Rdom_minerl_init_mean,Rdom_minerl_init_mean,R_growth_init_mean)
  par_init_mean <<- c(zone1_temp_init_mean,zone2_temp_init_mean, Fsed_oxy_init_mean, swf_init_mean)
  par_init_lowerbound <<- c(zone1_temp_init_lowerbound,zone2_temp_init_lowerbound, Fsed_oxy_init_lowerbound,swf_init_lowerbound) #Fsed_oxy_init_lowerbound,Rdom_minerl_init_lowerbound,R_growth_init_lowerbound)
  par_init_upperbound <<- c(zone1_temp_init_upperbound,zone2_temp_init_upperbound, Fsed_oxy_init_upperbound,swf_init_upperbound)#Fsed_oxy_init_upperbound,Rdom_minerl_init_upperbound,R_growth_init_upperbound)
  par_init_qt <<- c(zone1temp_init_qt,zone2temp_init_qt, Fsed_oxy_init_qt,swf_init_qt)#Fsed_oxy_init_qt,Rdom_minerl_init_qt,R_growth_init_qt)
  par_units <<- c("deg_C","-","-","-") #
  
  #par_names <<- c("sed_temp_mean","sed_temp_mean","Fsed_oxy","sw_factor","pd%R_growth")
  #par_names_save <<- c("zone1temp","zone2temp","Fsed_oxy","sw_factor","R_growth")
  #par_nml <<- c("glm3.nml","glm3.nml","aed2.nml","glm3.nml","aed2_phyto_pars.nml") #Fsed_oxy_init_mean,Rdom_minerl_init_mean,Rdom_minerl_init_mean,R_growth_init_mean)
  #par_init_mean <<- c(zone1_temp_init_mean,zone2_temp_init_mean, Fsed_oxy_init_mean, swf_init_mean,R_growth_init_mean)
  #par_init_lowerbound <<- c(zone1_temp_init_lowerbound,zone2_temp_init_lowerbound, Fsed_oxy_init_lowerbound,swf_init_lowerbound,R_growth_init_lowerbound) #Fsed_oxy_init_lowerbound,Rdom_minerl_init_lowerbound,R_growth_init_lowerbound)
  #par_init_upperbound <<- c(zone1_temp_init_upperbound,zone2_temp_init_upperbound, Fsed_oxy_init_upperbound,swf_init_upperbound,R_growth_init_upperbound)#Fsed_oxy_init_upperbound,Rdom_minerl_init_upperbound,R_growth_init_upperbound)
  #par_init_qt <<- c(zone1temp_init_qt,zone2temp_init_qt, Fsed_oxy_init_qt,swf_init_qt,R_growth_init_qt)#Fsed_oxy_init_qt,Rdom_minerl_init_qt,R_growth_init_qt)
  #par_units <<- c("deg_C","-","-","-","-") #
  
}else{
  #par_names <<- c("sw_factor")
  #par_names_save <<- c("SW_factor")
  #par_nml <<- c("glm3.nml")
  #par_init_mean <<- c(swf_init_mean)
  #par_init_lowerbound <<- c(swf_init_lowerbound)
  #par_init_upperbound <<- c(swf_init_upperbound)
  #par_init_qt <<- c(swf_init_qt)
  #par_units <<- c("-")
  par_names <<- c("sed_temp_mean","sed_temp_mean","Fsed_oxy") #"Fsed_oxy","Rdom_minerl","pd%R_growth")
  par_names_save <<- c("zone1temp","zone2temp","Fsed_oxy")
  par_nml <<- c("glm3.nml","glm3.nml","aed2.nml") #"aed2.nml","aed2.nml","aed2_phyto_pars.nml")
  par_init_mean <<- c(zone1_temp_init_mean,zone2_temp_init_mean,Fsed_oxy_init_mean)#Fsed_oxy_init_mean,Rdom_minerl_init_mean,Rdom_minerl_init_mean,R_growth_init_mean)
  par_init_lowerbound <<- c(zone1_temp_init_lowerbound,zone2_temp_init_lowerbound,Fsed_oxy_init_lowerbound)#Fsed_oxy_init_lowerbound,Rdom_minerl_init_lowerbound,R_growth_init_lowerbound)
  par_init_upperbound <<- c(zone1_temp_init_upperbound,zone2_temp_init_upperbound,Fsed_oxy_init_upperbound)#Fsed_oxy_init_upperbound,Rdom_minerl_init_upperbound,R_growth_init_upperbound)
  par_init_qt <<- c(zone1temp_init_qt,zone2temp_init_qt,Fsed_oxy_init_qt)#Fsed_oxy_init_qt,Rdom_minerl_init_qt,R_growth_init_qt)
  par_units <<- c("deg_C","deg_C","-") #
}

#####################################
###  Observation information
######################################


use_ctd <<- TRUE
#Use CTD data in place of the sensor string

observed_depths_temp <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
#Depths with temperature observations

observed_depths_do <<- c(1, 5, 9)
#Depths with do observations

observed_depths_chla_fdom <<- 1
#Depths with Chla and fdom observations

temp_obs_fname <<- c("Catwalk.csv","Catwalk_cleanedEDI.csv")
#name of catwalk file name
#Catwalk has the temperature string, fdom, chla, and do sensors

met_obs_fname <<- c(paste0(data_location,"/carina-data/FCRmet.csv"),paste0(data_location, "/extra_files/Met_final_2015_2018.csv"))
#met_obs_fname <<- c(paste0(data_location,"/carina-data/FCRmet.csv"))
#Name of meteorology file name

inflow_file1 <<- "FCR_weir_inflow_newEDI_2013_2017_20190128_oneDOC.csv"
outflow_file1 <<- "FCR_spillway_outflow_newEDI_SUMMED_WeirWetland_2013_2017_20190128.csv"
inflow_file2 <<- "FCR_wetland_inflow_newEDI_2013_2017_20190305_oneDOC.csv"
#Name of the historical inflow and outflow files

#########################################
###  Water quality state information
#########################################

#define water quality variables modeled.  Not used if include_wq == FALSE
wq_names <<- c("OXY_oxy",
               "CAR_pH",
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
               "PHY_TCHLA"
)

#Default initial states if lacking observations 
init_donc <<- 0.1/47.4
init_dopc <<- 0.1/47.4
init_ponc <<- (0.1/78.5)
init_popc <<-(0.1/78.5)

OXY_oxy_init <<- 300.62
CAR_pH_init <<- 6.2
CAR_dic_init <<- 200
CAR_ch4_init <<- 5
SIL_rsi_init <<- 100
NIT_amm_init <<- 0.69
NIT_nit_init <<- 0.05
PHS_frp_init <<- 0.07
OGM_doc_init <<- 47.4
OGM_poc_init <<- 78.5
OGM_pon_init <<- OGM_poc_init * init_ponc
OGM_pop_init <<- OGM_poc_init * init_popc
NCS_ss1_init <<- 1.0
PHS_frp_ads_init <<- 0.0
PHY_TCHLA_init <<- 2.0

#The names of the phytoplankton groups that contribute to
#total chl-a in GLM
tchla_components_vars <<- c("PHY_AGGREGATE")

#carbon to chlorophyll ratio (mg C/mg chla)
#12 g/ mole of C vs. X g/ mole of chla
#Initial concentration of phytoplankton (mmol C/m3)
biomass_to_chla <<- c(103.7871176/12)

#uncertainy in temperature measurement
obs_error_temperature <<- 0.0001 #NEED TO DOUBLE CHECK
#Observational uncertainty for each variable
obs_error_wq <<- c(100, #OXY_oxy #0.25
                   NA, #CAR_pH
                   NA, #CAR_dic
                   NA, #CAR_ch4
                   NA, #SIL_rsi
                   0.1, #NIT_amm
                   0.01, #NIT_nit
                   0.01, #PHS_frp
                   1, #OGM_doc
                   NA, #OGM_poc
                   NA, #OGM_don
                   NA, #OGM_pon
                   NA, #OGM_dop
                   NA, #OGM_pop
                   NA, #NCS_ss1
                   NA, #PHS_frp_ads
                   0.25) #PHY_TCHLA

exo_2_ctd_chla <- c(0.184, 0.525)

temp_process_error <<- 0.5
OXY_oxy_process_error <<- 1000
CAR_pH_process_error <<- 0.001
CAR_dic_process_error <<- 1
CAR_ch4_process_error <<- 1
SIL_rsi_process_error <<- 1
NIT_amm_process_error <<- 0.005
NIT_nit_process_error <<- 0.001
PHS_frp_process_error <<- 0.001
OGM_doc_process_error <<- 1
OGM_poc_process_error <<- 0.1
OGM_don_process_error <<- 0.1
OGM_pon_process_error <<- 0.1
OGM_dop_process_error <<- 0.1
OGM_pop_process_error <<- 0.1
NCS_ss1_process_error <<- 0.01
PHS_frp_ads_process_error <<- 0.0001
PHY_TCHLA_process_error <<- 0.3

temp_init_error <<- 0.5
OXY_oxy_init_error <<- 1000
CAR_pH_init_error <<- 0.001
CAR_dic_init_error <<- 0.001
CAR_ch4_init_error <<- 2
SIL_rsi_init_error <<- 20
NIT_amm_init_error <<- 2
NIT_nit_init_error <<- 2
PHS_frp_init_error <<- 2
OGM_doc_init_error <<- 1000
OGM_poc_init_error <<- 2
OGM_don_init_error <<- 4
OGM_pon_init_error <<- 2
OGM_dop_init_error <<- 3
OGM_pop_init_error <<- 4
NCS_ss1_init_error <<- 0.5
PHS_frp_ads_init_error <<- 0.1
PHY_TCHLA_init_error <<- 3

#########################################
# Archiving options
#######################################

#Pull data from github?
#Push results to github?
pull_from_git <<- TRUE
push_to_git <<- FALSE

#########################################
# Plotting related options
#######################################

# Options for printing function
# Depths (meters) that the water quality variables are plotted
focal_depths_wq <<- c(1,5,9)
#Depths that are plotted for the manager plot
focal_depths_manager <<- c(1,5,9) #c(4,16,25)
#Indexes for the depths that are compared to calculate turnover
turnover_index_1 <<- 2
turnover_index_2 <<- 9

####################################
# Extra options that you will not adjust
####################################
hold_inflow_outflow_constant <<- FALSE
print_glm2screen <<- FALSE