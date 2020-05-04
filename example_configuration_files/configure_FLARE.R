##########################
# Lake information
###########################

lake_name <<- "fcre"
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
include_wq <<- FALSE
#TRUE = use AED

use_null_model <<- FALSE

##########################
# Management Options
###########################
simulate_SSS <<- TRUE
#Include SSS in data assimilation

forecast_no_SSS <<- TRUE
#Run forecasts without SSS turned off

use_specified_sss <<- TRUE

forecast_SSS_flow <<- 1000
#m3/day rate of flow if SSS turned on in forecast
forecast_SSS_Oxy <<- 500
#umol/m3  of oxygen if SSS turned on in forecast
sss_fname <<- paste0(data_location,"/manual-data/FCR_SSS_inflow_2013_2020.csv")

sss_inflow_factor <<- 1.0
sss_depth <- 8

#####################
# Weather forcing options
######################
use_future_met <<- TRUE
#TRUE = use NOAA forecast for "Future"
#FALSE = use observed weather for "Future"; only works if "forecasting" past dates

DOWNSCALE_MET <<- TRUE
#Downscale the coarse resolutoin NOAA data to the local
#site using the meterology station at the lake

noaa_location <<- paste0(data_location, "/",lake_name,"/")

downscaling_coeff <<- paste0(data_location, "/manual-data/debiased.coefficients.2018_07_12_2019_07_11.RData")

#file name of previous downscaling coefficients
#use NA if not using an existing file

met_ds_obs_start <<- as.Date("2018-07-12")
met_ds_obs_end <<- as.Date("2019-07-11")
#Dates to use to developing the downscaling coefficient

missing_met_data_threshold <<- 100

use_future_inflow <<- TRUE
future_inflow_flow_coeff <<- c(0.0010803, 0.9478724, 0.3478991)
future_inflow_flow_error <<- 0.00965
future_inflow_temp_coeff <<- c(0.20291, 0.94214, 0.04278)
future_inflow_temp_error <<- 0.943
############################
# Run information
#############################

GLMversion <<- "GLM 3.0.0beta10"
FLAREversion <<- "v1.0_beta.1"
#GLM and FLARE version; the code adds these to the output files

base_GLM_nml <- paste0(forecast_location,"/glm3_woAED.nml" )
if(include_wq){
  base_AED_nml <<- paste0(forecast_location,"/aed2_only_Oxy.nml")
  base_AED_phyto_pars_nml  <<- paste0(forecast_location,"/aed2_phyto_pars.nml")
  base_AED_zoop_pars_nml  <<- paste0(forecast_location,"/aed2_zoop_pars.nml")
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

modeled_depths <<- round(c(0.1, seq(0.33334, 9.33, 0.333334)), 2)

default_temp_init <<- c(6.2, 5.7, 5.5, 5.5, 5.4, 5.3, 5.3, 5.3, 5.2, 5.0)
default_temp_init_depths <<-  c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
the_sals_init <<- 0.0

default_snow_thickness_init <<- 0.0
default_white_ice_thickness_init <<- 0.0
default_blue_ice_thickness_init <<- 0.0

##############################
##  Ensemble members used
##############################
ensemble_size <<- 21*10
n_ds_members <<- 10
n_inflow_outflow_members <<- 21*10

################################
### Process uncertainty adaption
##################################
use_cov <<- TRUE
adapt_qt_method <<- 1  #0 = no adapt, 1 = variance in residuals, 2 = Rastetter et al 2011
num_adapt_days <<- 30
Inflat_pars <<- 1.02

#################################
# Parameter calibration information
#################################

#include_pars_in_qt_update <<- TRUE
#Adapt the parameter noise

#Initial zone temperatures and the upper and lower bounds
#Zone 1 is Xm to Xm
#Zone 2 is Xm to Xm
zone1_temp_init_mean <<- 12
zone1_temp_init_lowerbound <<- 10
zone1_temp_init_upperbound <<- 20
zone1_temp_lowerbound <<- -100
zone1_temp_upperbound <<- 100
#daily perturbance of parameter value
zone1temp_init_qt <<- 1^2  #THIS IS THE VARIANCE, NOT THE SD

zone2_temp_init_mean <<- 15
zone2_temp_init_lowerbound <<-  10
zone2_temp_init_upperbound <<-  20
zone2_temp_lowerbound <<-  -100
zone2_temp_upperbound <<-  100
zone2temp_init_qt <<- 1^2 #THIS IS THE VARIANCE, NOT THE SD

#Shortwave factor
swf_init_mean <<- 1.0
swf_init_lowerbound <<- 0.5
swf_init_upperbound <<- 1.5
swf_lowerbound <<- -10
swf_upperbound <<- 10
#daily perturbance of parameter value
swf_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD

#Longwave factor
lwf_init_mean <<- 1.0
lwf_init_lowerbound <<- 0.5
lwf_init_upperbound <<- 1.5
lwf_lowerbound <<- -10
lwf_upperbound <<- 10
#daily perturbance of parameter value
lwf_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD

#Inflow factor
inflow_factor_init_mean <<- 0.5
inflow_factor_init_lowerbound <<- 0.0
inflow_factor_init_upperbound <<- 1.0
inflow_factor_lowerbound <<- 0.0
inflow_factor_upperbound <<- 1.0
#daily perturbance of parameter value
inflow_factor_init_qt <<- 0.001^2 #THIS IS THE VARIANCE, NOT THE SD

par_names <<- c("sed_temp_mean"
                ,"sed_temp_mean"
                ,"sw_factor"
                #,"lw_factor"
                ,"inflow_factor"
)
par_names_save <<- c("zone1temp"
                     ,"zone2temp"
                     ,"sw_factor"
                     #,"lw_factor"
                     ,"inflow_factor"
)
par_nml <<- c("glm3.nml"
              ,"glm3.nml"
              ,"glm3.nml"
              #,"glm3.nml"
              ,"glm3.nml"
)
par_init_mean <<- c(zone1_temp_init_mean
                    ,zone2_temp_init_mean
                    ,swf_init_mean
                    #,lwf_init_mean
                    ,inflow_factor_init_mean
)
par_init_lowerbound <<- c(zone1_temp_init_lowerbound
                          ,zone2_temp_init_lowerbound
                          ,swf_init_lowerbound
                          #,lwf_init_lowerbound
                          ,inflow_factor_init_lowerbound
)
par_init_upperbound <<- c(zone1_temp_init_upperbound
                          ,zone2_temp_init_upperbound
                          ,swf_init_upperbound
                          #, lwf_init_upperbound
                          ,inflow_factor_init_upperbound
)
par_lowerbound <<- c(zone1_temp_lowerbound
                     ,zone2_temp_lowerbound
                     ,swf_lowerbound
                     #, lwf_lowerbound
                     ,inflow_factor_lowerbound
)
par_upperbound <<- c(zone1_temp_upperbound
                     ,zone2_temp_upperbound
                     ,swf_upperbound
                     #, lwf_upperbound
                     ,inflow_factor_upperbound
)
par_init_qt <<- c(zone1temp_init_qt
                  ,zone2temp_init_qt
                  ,swf_init_qt
                  #, lwf_init_qt
                  ,inflow_factor_init_qt
)
par_units <<- c("deg_C"
                ,"deg_C"
                ,"-"
                #,"-"
                ,"-"
) 

#####################################
###  Observation information
######################################
realtime_insitu_location <- paste0(data_location,"/mia-data")
realtime_met_station_location <- paste0(data_location,"/carina-data") 
manual_data_location <- paste0(data_location, "/manual-data") 
realtime_inflow_data_location <- paste0(data_location, "/diana-data")


ctd_fname <<- NA #paste0(manual_data_location,"/CTD_final_2013_2019.csv")

nutrients_fname <<- paste0(manual_data_location,"/chemistry.csv")

insitu_obs_fname <<- c(paste0(realtime_insitu_location,"/Catwalk.csv"),
                     paste0(manual_data_location,"/Catwalk_cleanedEDI.csv"))
variable_obsevation_depths <<- FALSE
ctd_2_exo_chla <<- c(-0.35, 1.9)

met_obs_fname <<- c(paste0(realtime_met_station_location,"/FCRmet.csv"),
                    paste0(manual_data_location,"/Met_final_2015_2019.csv"))

inflow_file1 <<- c(paste0(realtime_inflow_data_location,"/FCRweir.csv"),
                   paste0(manual_data_location,"/inflow_for_EDI_2013_06Mar2020.csv"))

outflow_file1 <<- paste0(manual_data_location,"/FCR_spillway_outflow_newEDI_SUMMED_WeirWetland_2013_2018_20190912.csv")

inflow_file2 <<- NA

do_methods <<- c("do_sensor", "exo_sensor")
chla_methods <<- c("exo_sensor", "ctd")
temp_methods <<- c("thermistor", "do_sensor", "exo_sensor")
fdom_methods <<- c("exo_sensor","grab_sample")
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

#########################################
###  Water quality state information
#########################################

#The names of the phytoplankton groups that contribute to
#total chl-a in GLM
tchla_components_vars <<- c("PHY_cyano","PHY_green","PHY_diatom")

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
               "PHY_TCHLA")

#Default initial states if lacking observations

#carbon to chlorophyll ratio (mg C/mg chla)
#12 g/ mole of C vs. X g/ mole of chla
#Initial concentration of phytoplankton (mmol C/m3)
biomass_to_chla <<- c((50/12),(50/12), (50/12))


init_donc <<- 0.1/47.4
init_dopc <<- 0.1/47.4
init_ponc <<- (0.1/78.5)
init_popc <<-(0.1/78.5)

OXY_oxy_init <<- 300.62
CAR_pH_init <<- 6.2
CAR_dic_init <<- 2000
CAR_ch4_init <<- 5
SIL_rsi_init <<- 100
NIT_amm_init <<- 0.69
NIT_nit_init <<- 0.05
PHS_frp_init <<- 0.07
OGM_doc_init <<- 200
OGM_poc_init <<- 20 #78.5
OGM_pon_init <<- OGM_poc_init * init_ponc
OGM_pop_init <<- OGM_poc_init * init_popc
NCS_ss1_init <<- 1.0
PHS_frp_ads_init <<- 0.0
PHY_TCHLA_init <<- 10.0

init_phyto_proportion <<- c(0.3, 0.3, 0.4)

#uncertainy in temperature measurement
obs_error_temperature_intercept <<- 0.1^2

obs_error_temperature_slope <<- 0.0

#Observational uncertainty for each variable

obs_error_wq_intercept_phyto = c(NA, NA, NA)

obs_error_wq_intercept <<- c(5, #OXY_oxy #0.25
                             NA, #CAR_pH
                             NA, #CAR_dic
                             NA, #CAR_ch4
                             NA, #SIL_rsi
                             0.01, #NIT_amm
                             0.001, #NIT_nit
                             0.001, #PHS_frp
                             500, #OGM_doc
                             NA, #OGM_poc
                             NA, #OGM_don
                             NA, #OGM_pon
                             NA, #OGM_dop
                             NA, #OGM_pop
                             NA, #NCS_ss1
                             NA, #PHS_frp_ads
                             0.25) #PHY_TCHLA


obs_error_wq_slope_phyto = c(NA, NA, NA)

obs_error_wq_slope <<- c(0, #OXY_oxy #0.25
                         NA, #CAR_pH
                         NA, #CAR_dic
                         NA, #CAR_ch4
                         NA, #SIL_rsi
                         0, #NIT_amm
                         0, #NIT_nit
                         0, #PHS_frp
                         0, #OGM_doc
                         NA, #OGM_poc
                         NA, #OGM_don
                         NA, #OGM_pon
                         NA, #OGM_dop
                         NA, #OGM_pop
                         NA, #NCS_ss1
                         NA, #PHS_frp_ads
                         0) #PHY_TCHLA

temp_process_error <<- 0.5
OXY_oxy_process_error <<- 1000
CAR_pH_process_error <<- 0.001
CAR_dic_process_error <<- 1
CAR_ch4_process_error <<- 1
SIL_rsi_process_error <<- 1
NIT_amm_process_error <<- 1
NIT_nit_process_error <<- 0.1
PHS_frp_process_error <<- 0.1
OGM_doc_process_error <<- 1000
OGM_poc_process_error <<- 0.1
OGM_don_process_error <<- 0.1
OGM_pon_process_error <<- 0.1
OGM_dop_process_error <<- 0.1
OGM_pop_process_error <<- 0.1
NCS_ss1_process_error <<- 0.01
PHS_frp_ads_process_error <<- 0.0001
PHY_TCHLA_process_error <<- 5
PHY_process_error <<- c(0.001, 0.001, 0.001)

temp_init_error <<- 0.5
OXY_oxy_init_error <<- 1000
CAR_pH_init_error <<- 0.001
CAR_dic_init_error <<- 0.001
CAR_ch4_init_error <<- 2
SIL_rsi_init_error <<- 20
NIT_amm_init_error <<- 2
NIT_nit_init_error <<- 0.01
PHS_frp_init_error <<- 0.01
OGM_doc_init_error <<- 1000
OGM_poc_init_error <<- 2
OGM_don_init_error <<- 4
OGM_pon_init_error <<- 2
OGM_dop_init_error <<- 3
OGM_pop_init_error <<- 4
NCS_ss1_init_error <<- 0.5
PHS_frp_ads_init_error <<- 0.1
PHY_TCHLA_init_error <<- 3
PHY_init_error <<- c(3, 3, 3)

#########################################
# Archiving options
#######################################

#Pull data from github?
#Push results to github?
pull_from_git <<- FALSE
push_to_git <<- FALSE

#########################################
# Plotting related options
#######################################

# Options for printing function
# Depths (meters) that the water quality variables are plotted
focal_depths_wq <<- c(1,5,9)
#Depths that are plotted for the manager plot
focal_depths_manager <<- c(1,5,8) #c(2, 4, 9) #c(4,16,25) #c(4,16,25)
#Indexes for the depths that are compared to calculate turnover
turnover_index_1 <<- 1 #1 #4
turnover_index_2 <<- 8 #8 #25