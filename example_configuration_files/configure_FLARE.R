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
include_wq <<- TRUE
#TRUE = use AED

use_null_model <<- FALSE

##########################
# Management Options
###########################
simulate_SSS <<- TRUE
#Include SSS in data assimilation
forecast_SSS <<- FALSE
#Run forecasts without SSS turned on
use_specified_sss <<- TRUE
# Use SSS from file in forecast
forecast_SSS_flow <<- 1000
#m3/day rate of flow if SSS turned on in forecast
forecast_SSS_Oxy <<- 500
#umol/m3  of oxygen if SSS turned on in forecast
sss_fname <<- paste0(data_location,"/manual-data/FCR_SSS_inflow_2013_2020.csv")

sss_inflow_factor <<- 1.0
sss_depth <<- 8.0


#####################
# Weather forcing options
######################
use_future_met <<- FALSE
#TRUE = use NOAA forecast for "Future"
#FALSE = use observed weather for "Future"; only works if "forecasting" past dates

DOWNSCALE_MET <<- FALSE
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

use_future_inflow <<- FALSE
future_inflow_flow_coeff <<- c(0.0010803, 0.9478724, 0.3478991)
future_inflow_flow_error <<- 0.00965
future_inflow_temp_coeff <<- c(0.20291, 0.94214, 0.04278)
future_inflow_temp_error <<- 0.943

doc_scalar <<- 3.0

############################
# Run information
#############################

model_name <- "glm_aed" #other is "null"

GLMversion <<- "GLM 3.1.0a"
FLAREversion <<- "v1.1"
#GLM and FLARE version; the code adds these to the output files

base_GLM_nml <- paste0(forecast_location,"/glm3.nml" )
if(include_wq){
  base_AED_nml <<- paste0(forecast_location,"/aed2_20200701_2DOCpools.nml")
  base_AED_phyto_pars_nml  <<- paste0(forecast_location,"/aed2_phyto_pars_30June2020.nml")
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

default_temp_init <<- c(25.667, 24.9101, 23.067, 21.8815, 19.6658, 16.5739, 12.9292, 12.8456, 12.8127, 12.8079, 12.778)
default_temp_init_depths <<-  c(0.127, 1.004, 2.005, 3.021, 4.002, 5.004, 6.004, 7.01, 8.001, 9.015, 9.518)
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
localization_distance <<- NA #distance in meters were covariances in the model error are used
Inflat_pars <<- 1.02
vert_decorr_length <- 4.0
no_negative_states <- TRUE

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

zone2_temp_init_mean <<- 15
zone2_temp_init_lowerbound <<-  10
zone2_temp_init_upperbound <<-  20
zone2_temp_lowerbound <<-  -100
zone2_temp_upperbound <<-  100

#Shortwave factor
swf_init_mean <<- 0.9
swf_init_lowerbound <<- 0.5
swf_init_upperbound <<- 1.5
swf_lowerbound <<- -10
swf_upperbound <<- 10


#Longwave factor
lwf_init_mean <<- 1.0
lwf_init_lowerbound <<- 0.5
lwf_init_upperbound <<- 1.5
lwf_lowerbound <<- -10
lwf_upperbound <<- 10

#Inflow factor
inflow_factor_init_mean <<- 0.5
inflow_factor_init_lowerbound <<- 0.0
inflow_factor_init_upperbound <<- 1.0
inflow_factor_lowerbound <<- 0.0
inflow_factor_upperbound <<- 1.0

#Fsed_oxy
Fsed_oxy_init_mean <<- -20
Fsed_oxy_init_lowerbound <<-  -50 #-30 #-24.220
Fsed_oxy_init_upperbound <<-  -5 #10 #-24.218
Fsed_oxy_lowerbound <<-  -1000 #-30 #-24.220
Fsed_oxy_upperbound <<-  1000 #10 #-24.218

#Rdom_minerl
Rdom_minerl_init_mean <<- 000102192
Rdom_minerl_init_lowerbound <<-  0.0005 #0.00049
Rdom_minerl_init_upperbound <<-  0.005 #0.00051
Rdom_minerl_lowerbound <<-  0 #0.0000 #0.00049
Rdom_minerl_upperbound <<-  1 #0.00051

#R_growth
R_growth_init_mean <<-  1.0
R_growth_init_lowerbound <<-  0.5 #0.99
R_growth_init_upperbound <<-  1.5 #1.01
R_growth_lowerbound <<-  -10.00 #0.99
R_growth_upperbound <<-  10.00 #1.01

#Rnitrif
Rnitrif_init_mean <<- 0.013
Rnitrif_init_lowerbound <<-  0.005 #0.99
Rnitrif_init_upperbound <<-  0.05 #1.01
Rnitrif_lowerbound <<-  0 #0.99
Rnitrif_upperbound <<-  1 #1.01

#w_p
w_p_init_mean <<- 0.0
w_p_init_lowerbound <<-  -0.1 #0.99
w_p_init_upperbound <<-  0.1 #1.01
w_p_lowerbound <<-  -0.1 #0.99
w_p_upperbound <<-  0.1 #1.01

#Fsed_frp
Fsed_frp_init_mean <<- 0.1
Fsed_frp_init_lowerbound <<-  0.001 #0.99
Fsed_frp_init_upperbound <<-  0.05 #1.01
Fsed_frp_lowerbound <<-  0#0.99
Fsed_frp_upperbound <<-  10 #1.01

#Fsed_amm
Fsed_amm_init_mean <<- 2
Fsed_amm_init_lowerbound <<-  1 #0.99
Fsed_amm_init_upperbound <<-  2 #1.01
Fsed_amm_lowerbound <<-  -1000 #0.99
Fsed_amm_upperbound <<-  1000 #1.01

#Fsed_nit
Fsed_nit_init_mean <<- -0.5
Fsed_nit_init_lowerbound <<-  -1 #0.99
Fsed_nit_init_upperbound <<-  -0.5 #1.01
Fsed_nit_lowerbound <<-  -100 #0.99
Fsed_nit_upperbound <<-  0 #1.01

#Fsed_doc
Fsed_doc_init_mean <<- 50
Fsed_doc_init_lowerbound <<-  10 #0.99
Fsed_doc_init_upperbound <<-  60 #1.01
Fsed_doc_lowerbound <<-  -1000 #0.99
Fsed_doc_upperbound <<-  1000 #1.01

par_names <<- c("sed_temp_mean"
                ,"sed_temp_mean"
                ,"sw_factor"
                ,"Fsed_oxy"
                ,"Fsed_oxy"
                ,"Rdomr_minerl"
                #,"Fsed_frp"
                #,"Fsed_amm"
                #,"Fsed_nit"
                ,"pd%R_resp"
                ,"Rnitrif"
                #,"pd%X_ncon"
                #,"pd%X_pcon"
                ,"pd%K_N"
                ,"pd%K_P"
                #,"pd%I_K"
)

#par_names <<- c()

par_names_save <<- c("zone1temp"
                     ,"zone2temp"
                     ,"sw_factor"
                     ,"Fsed_oxy_zone1"
                     ,"Fsed_oxy_zone2"
                     ,"Rdomr_minerl"
                     #,"Fsed_frp"
                     #,"Fsed_amm"
                     #,"Fsed_nit"
                     ,"R_resp"
                     ,"Rnitrif"
                     #,"X_ncon"
                     #,"X_pcon"
                     ,"K_N"
                     ,"K_P"
                     #,"I_K" 
)
par_nml <<- c("glm3.nml"
              ,"glm3.nml"
              ,"glm3.nml"
              ,"aed2.nml"
              ,"aed2.nml"
              ,"aed2.nml"
              #,"aed2.nml"
              #,"aed2.nml"
              #,"aed2.nml"
              ,"aed2_phyto_pars.nml"
              ,"aed2.nml"
              #,"aed2_phyto_pars.nml"
              #,"aed2_phyto_pars.nml"
              ,"aed2_phyto_pars.nml"
              ,"aed2_phyto_pars.nml"
              #,"aed2_phyto_pars.nml"
)

par_init_mean <<- c(zone1_temp_init_mean
                    ,zone2_temp_init_mean
                    ,swf_init_mean
                    ,-40
                    ,-40
                    ,Rdom_minerl_init_mean
                    #,Fsed_frp_init_mean
                    #,Fsed_amm_init_mean
                    #,Fsed_nit_init_mean
                    ,0.12
                    ,Rnitrif_init_mean
                    # ,0.035
                    #,0.0015
                    ,2
                    ,0.05
                    #,15
)
par_init_lowerbound <<- c(zone1_temp_init_lowerbound
                          ,zone2_temp_init_lowerbound
                          ,swf_init_lowerbound
                          ,-40
                          ,-30
                          ,Rdom_minerl_init_lowerbound
                          #,Fsed_frp_init_lowerbound
                          #,Fsed_amm_init_lowerbound
                          #,Fsed_nit_init_lowerbound
                          ,0.05
                          ,Rnitrif_init_lowerbound
                          #,0.02
                          #,0.0005
                          ,2
                          ,0.1
                          #,10
)
par_init_upperbound <<- c(zone1_temp_init_upperbound
                          ,zone2_temp_init_upperbound
                          ,swf_init_upperbound
                          ,-10
                          ,20
                          ,Rdom_minerl_init_upperbound
                          #,Fsed_frp_init_upperbound
                          #,Fsed_amm_init_upperbound
                          #,Fsed_nit_init_upperbound
                          ,0.15
                          ,Rnitrif_init_upperbound
                          #,0.070
                          #,0.005
                          ,4
                          ,0.2
                          #,25
)
par_lowerbound <<- c(zone1_temp_lowerbound
                     ,zone2_temp_lowerbound
                     ,swf_lowerbound
                     ,Fsed_oxy_lowerbound
                     ,Fsed_oxy_lowerbound
                     ,Rdom_minerl_lowerbound
                     #,Fsed_frp_lowerbound
                     #,Fsed_amm_lowerbound
                     #,Fsed_nit_lowerbound
                     ,0.001
                     ,Rnitrif_lowerbound
                     #,0.02
                     #,0.0005
                     ,0.26
                     ,0.04
                     #,5
)
par_upperbound <<- c(zone1_temp_upperbound
                     ,zone2_temp_upperbound
                     ,swf_upperbound
                     ,Fsed_oxy_upperbound
                     ,Fsed_oxy_upperbound
                     ,Rdom_minerl_upperbound
                     #,Fsed_frp_upperbound
                     #,Fsed_amm_upperbound
                     #,Fsed_nit_upperbound
                     ,1
                     ,Rnitrif_upperbound
                     #,0.070
                     #,0.005
                     ,10
                     ,1
                     #,30
)

par_units <<- c("deg_C"
                ,"deg_C"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
                ,"-"
) 

#####################################
###  Observation information
######################################

realtime_insitu_location <- paste0(data_location,"/mia-data")
realtime_met_station_location <- paste0(data_location,"/carina-data") 
manual_data_location <- paste0(data_location, "/manual-data") 
realtime_inflow_data_location <- paste0(data_location, "/diana-data")


ctd_fname <<- paste0(manual_data_location,"/CTD_final_2013_2019.csv")

nutrients_fname <<- paste0(manual_data_location,"/chemistry.csv")

insitu_obs_fname <<- c(paste0(realtime_insitu_location,"/Catwalk.csv"),
                       paste0(manual_data_location,"/Catwalk_cleanedEDI.csv"))
variable_obsevation_depths <<- FALSE


met_obs_fname <<- c(paste0(realtime_met_station_location,"/FCRmet.csv"),
                    paste0(manual_data_location,"/Met_final_2015_2019.csv"))

inflow_file1 <<- c(paste0(realtime_inflow_data_location,"/FCRweir.csv"),
                   paste0(manual_data_location,"/inflow_for_EDI_2013_06Mar2020.csv"))

outflow_file1 <<- paste0(manual_data_location,"/FCR_spillway_outflow_newEDI_SUMMED_WeirWetland_2013_2018_20190912.csv")

inflow_file2 <<- NA

focal_depths <<- 1.6

do_methods <<- c("do_sensor", "exo_sensor") #,"ctd")
chla_methods <<- c("exo_sensor") # "ctd")
temp_methods <<- c("thermistor", "do_sensor", "exo_sensor") # "ctd")
fdom_methods <<- c("exo_sensor") #"grab_sample")
nh4_methods <<- c("grab_sample")
no3_methods <<- c("grab_sample")
srp_methods <<- c("grab_sample")
dic_methods <<- c("grab_sample")

time_threshold_seconds_temp <<- 60*60*12
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

#carbon to chlorophyll ratio (mg C/mg chla)
#12 g/ mole of C vs. X g/ mole of chla
#Initial concentration of phytoplankton (mmol C/m3)
biomass_to_chla <<- c((160/12),(60/12), (60/12))

#define water quality variables modeled.  Not used if include_wq == FALSE
state_names <<- c("temp",
                  "OXY_oxy",
                  "CAR_dic",
                  "CAR_ch4",
                  "SIL_rsi",
                  "NIT_amm",
                  "NIT_nit",
                  "PHS_frp",
                  "OGM_doc",
                  "OGM_docr",
                  "OGM_poc",
                  "OGM_don",
                  "OGM_donr",
                  "OGM_pon",
                  "OGM_dop",
                  "OGM_dopr",
                  "OGM_pop",
                  "PHY_cyano",
                  "PHY_green",
                  "PHY_diatom"
)

states_to_obs <<- list(temp = 1, 
                       OXY_oxy = 2, 
                       CAR_dic = NA, 
                       CAR_ch4 = NA, 
                       SIL_rsi = NA, 
                       NIT_amm = c(3,5), 
                       NIT_nit = c(4,5),  
                       PHS_frp = c(6,7), 
                       OGM_doc = 8, 
                       OGM_docr = 8, 
                       OGM_poc = NA, 
                       OGM_don = 5, 
                       OGM_donr = 5, 
                       OGM_pon = 5, 
                       OGM_dop = 7, 
                       OGM_dopr = 7, 
                       OGM_pop = 7, 
                       PHY_cyano = 9, #20
                       PHY_green = 9, #23
                       PHY_diatom = 9 #26
                       
)

states_to_obs_mapping <<- list(temp = 1, 
                               OXY_oxy= 1, 
                               CAR_dic = NA, 
                               CAR_ch4 = NA, 
                               SIL_rsi=  NA, 
                               NIT_amm = 1, 
                               NIT_nit = 1, 
                               PHS_frp = 1, 
                               OGM_doc = 1, 
                               OGM_docr = 1, 
                               OGM_poc = NA, 
                               OGM_don = 1, 
                               OGM_donr = 1, 
                               OGM_pon = 1, 
                               OGM_dop = 1, 
                               OGM_dopr = 1, 
                               OGM_pop = 1, 
                               PHY_cyano = 1/(biomass_to_chla[1]), 
                               PHY_green = 1/(biomass_to_chla[2]), 
                               PHY_diatom = 1/(biomass_to_chla[3])
)

state_names_obs <<- c(
  "temp",
  "OXY_oxy",
  "NIT_amm",
  "NIT_nit",
  "NIT_total",
  "PHS_frp",
  "PHS_total",
  "OGM_doc_total",
  "PHY_TCHLA")

#Default initial states if lacking observations
init_donc <<- 0.1/47.4
init_dopc <<- 0.0 #0.1/47.4
init_ponc <<-  (0.1/78.5)
init_popc <<- 0.0 #(0.1/78.5)

don_doc_init_ratio <<- 0.2
donr_docr_init_ratio <<- 0.1
dop_doc_init_ratio <<- 0.005
dopr_docr_init_ratio <<- 0

docr_to_total_doc <<- 0.80

phyto_n_biomass_ratio <<- 0.035
phyto_p_biomass_ratio <<- 0.0016

OXY_oxy_init <<- 300.62
CAR_pH_init <<- 6.2
CAR_dic_init <<- 100
CAR_ch4_init <<- 0 #5
SIL_rsi_init <<- 100
NIT_amm_init <<- 0.69
NIT_nit_init <<- 0.05
PHS_frp_init <<- 0.07
OGM_doc_init <<- 50
OGM_docr_init <<- 250
OGM_don_init <<- 3
OGM_donr_init <<- 7
OGM_dop_init <<- 0.05
OGM_dopr_init <<- 0.03
OGM_poc_init <<- 5
OGM_pon_init <<- 1
OGM_pop_init <<- 0.02
NCS_ss1_init <<- 0.0
PHS_frp_ads_init <<- 0.05

PHY_cyano_init <<- 10.0
PHY_green_init <<- 10.0
PHY_diatom_init <<- 10.0
PHY_cyano_IN_init <<- PHY_cyano_init * phyto_n_biomass_ratio
PHY_green_IN_init <<- PHY_green_init * phyto_n_biomass_ratio
PHY_diatom_IN_init <<- PHY_diatom_init * phyto_n_biomass_ratio
PHY_cyano_IP_init <<- PHY_cyano_init * phyto_p_biomass_ratio
PHY_green_IP_init <<- PHY_green_init * phyto_p_biomass_ratio
PHY_diatom_IP_init <<- PHY_diatom_init * phyto_p_biomass_ratio

init_phyto_proportion <<- c(0.3, 0.3, 0.4)

#Observational uncertainty for each variable

obs_error_intercept <<- list(temp = 0.07117918, # Temp
                             OXY_oxy = 26.86071, #OXY_oxy 0.25 1958.838
                             NIT_amm = (1.5 *1000*0.001*(1/18.04)), #NIT_amm
                             NIT_nit = (1.4*1000*0.001*(1/62.00)), #NIT_nit #0.08
                             NIT_total = (26.0*1000*0.001*(1/14)), #TN
                             PHS_frp = (1.2*1000*0.001*(1/94.9714)), #PHS_frp #0.05
                             PHS_total = (4.4*1000*0.001*(1/30.97)), #PHS_total
                             OGM_doc_total = 75.56199, #OGM_doc
                             PHY_TCHLA = 0.961727 #PHY_TCHLA 3.338957
) 

obs_error_slope <<- c(temp = 0, #temp
                      OXY_oxy = 0, #OXY_oxy #0.25
                      NIT_amm = 0, #NIT_amm
                      NIT_nit = 0, #NIT_nit
                      NIT_total = 0, #NIT_total
                      PHS_frp = 0, #PHS_frp
                      PHS_total = 0, #PHS_total
                      OGM_doc = 0, #OGM_doc
                      PHY_TCHLA = 0) #PHY_TCHLA

exo_2_ctd_chla <- c(0, 1)  #c(-2.0430, 2.5314) #c(1.8795, 0.6662)
exo_2_ctd_do <- c(0, 1) #c(8.3670, 0.7152)
do_2_ctd_do_5 <- c(0, 1) #c(19.6254, 0.8636)
do_2_ctd_do_9 <- c(0, 1) #c(11.0971, 0.9156)
ctd_2_exo_chla <<- c(0, 1)  #c(-2.0430, 2.5314) #c(-1.582, 1.335)
ctd_2_do_do <<- c(0, 1) #c(-10.770, 1.061)
exo_fdom_2_doc <<- c(-38.95, 22.47)

error_scaler <- 0.75
temp_process_error <<- (1.35 * error_scaler)
OXY_oxy_process_error <<- (42 * error_scaler)
CAR_pH_process_error <<- 0
CAR_dic_process_error <<- (179.86 * error_scaler)
CAR_ch4_process_error <<- (8.47 * error_scaler)
SIL_rsi_process_error <<- (37.9 * error_scaler)
NIT_amm_process_error <<- (0.72 * error_scaler) #4.94^2
NIT_nit_process_error <<- (0.12 * error_scaler) #1.06^2
PHS_frp_process_error <<- (0.04 * error_scaler)
OGM_doc_process_error <<- (7.80 * error_scaler)
OGM_docr_process_error <<- (65.8 * error_scaler)
OGM_poc_process_error <<- (10 * error_scaler)
OGM_don_process_error <<- (0.72 * 2 * error_scaler)
OGM_donr_process_error <<- (0.72 * 2 * error_scaler)
OGM_pon_process_error <<- (0.72 * 2 * error_scaler)
OGM_dop_process_error <<- (0.04 * 2 * error_scaler)
OGM_dopr_process_error <<- (0.04 * 2 *  error_scaler)
OGM_pop_process_error <<- (0.04 * 2 * error_scaler)


PHY_cyano_process_error <<- (2.52/3 * biomass_to_chla[1] * error_scaler)
PHY_green_process_error <<- (2.52/3 * biomass_to_chla[2] * error_scaler)
PHY_diatom_process_error <<- (2.52/3 * biomass_to_chla[3] * error_scaler)


temp_init_error <<- 1.5
OXY_oxy_init_error <<- 50
CAR_pH_init_error <<- 0.001
CAR_dic_init_error <<- 0.001
CAR_ch4_init_error <<- 2
SIL_rsi_init_error <<- 20
NIT_amm_init_error <<- 2
NIT_nit_init_error <<- 0.01
PHS_frp_init_error <<- 0.01
OGM_doc_init_error <<- 25
OGM_docr_init_error <<- 50
OGM_poc_init_error <<- 2
OGM_don_init_error <<- 4
OGM_donr_init_error <<- 4
OGM_pon_init_error <<- 2
OGM_dop_init_error <<- 3
OGM_dopr_init_error <<- 3
OGM_pop_init_error <<- 4
PHY_cyano_init_error <<- 10.0
PHY_green_init_error <<- 10.0
PHY_diatom_init_error <<- 10.0

####
# Dignostics
###

diagnostics_names <- c("extc_coef",
                       "PHY_cyano_fI",
                       "PHY_cyano_fNit",
                       "PHY_cyano_fPho",
                       "PHY_cyano_fT",
                       "PHY_green_fI",
                       "PHY_green_fNit",
                       "PHY_green_fPho",
                       "PHY_green_fT",
                       "PHY_diatom_fI",
                       "PHY_diatom_fNit",
                       "PHY_diatom_fPho",
                       "PHY_diatom_fT",
                       "rad")

secchi_file <- paste0(manual_data_location,"/Secchi_depth_2013-2019.csv")

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
focal_depths_plotting <<- modeled_depths
#Depths that are plotted for the manager plot
focal_depths_manager <<- c(1,5,8) #c(2, 4, 9) #c(4,16,25) #c(4,16,25)
#Indexes for the depths that are compared to calculate turnover
turnover_index_1 <<- 1 #1 #4
turnover_index_2 <<- 8 #8 #25