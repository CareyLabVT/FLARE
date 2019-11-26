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
include_wq <<- FALSE
#TRUE = use AED

##########################
# Management Options
###########################
simulate_SSS <<- TRUE
#Include SSS in data assimilation

forecast_no_SSS <<- TRUE
#Run forecasts without SSS turned off

forecast_SSS <<- FALSE
#Run forecasts without SSS turned on
forecast_SSS_flow <<- 1000
#m3/day rate of flow if SSS turned on in forecast
forecast_SSS_Oxy <<- 500
#umol/m3  of oxygen if SSS turned on in forecast
sss_fname <<- paste0(data_location,"/manual-data/FCR_SSS_inflow_2013_2019.csv")



#####################
# Weather forcing options
######################
use_future_met <<- TRUE
#TRUE = use NOAA forecast for "Future"
#FALSE = use observed weather for "Future"; only works if "forecasting" past dates

DOWNSCALE_MET <<- TRUE
#Downscale the coarse resolutoin NOAA data to the local
#site using the meterology station at the lake

downscaling_coeff <<- paste0(data_location, "/manual-data/debiased.coefficients.2018_07_12_2019_07_11.RData")

#file name of previous downscaling coefficients
#use NA if not using an existing file

met_ds_obs_start <<- as.Date("2018-07-12")
met_ds_obs_end <<- as.Date("2019-07-11")
#Dates to use to developing the downscaling coefficient

missing_met_data_threshold <<- 100

use_future_inflow <<- TRUE

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
  #base_GLM_nml <<- "glm3_woAED.nml"
  base_GLM_nml <<- "glm3_woAED_constant_sedtemp_hotmixing.nml"
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
  
  #modeled_depths <<- c(0.1, 0.5, 
  #                     1.00, 1.5,
  #                     2.00, 2.5,
  #                     3.0, 3.5,
  #                     4.0, 4.5,
  #                     5.0, 5.5,
  #                     6.0, 6.5,
  #                     7.00, 7.5,
  #                     8.0, 8.5,
  #                     9.00)
  
  
  #modeled_depths <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
}else{
  modeled_depths <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
  #modeled_depths <<- c(0.1, 0.5, 
  #                     1, 2.5, 
  #                     2, 2.5, 
  #                     3, 3.5,
  #                     4, 4.5, 
  #                     5, 5.5, 
  #                     6, 6.5, 
  #                     7, 7.5,
  #                     8, 8.5, 
  #                     9) 
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

default_temp_init <<- c(6.2, 5.7, 5.5, 5.5, 5.4, 5.3, 5.3, 5.3, 5.2, 5.0)
default_temp_init_depths <<-  c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
the_sals_init <<- 0.0

default_snow_thickness_init <<- 0.0
default_white_ice_thickness_init <<- 0.0
default_blue_ice_thickness_init <<- 0.0

##############################
##  Ensemble members used
##############################
n_enkf_members <<- 1
n_ds_members <<- 5  
n_inflow_outflow_members <<- 21
#Note: this number is multiplied by 
# 1) the number of NOAA ensembles (21)
# 2) the number of downscaling essembles (50 is current)
# get to the total number of essembles

################################
### Process uncertainty adaption
##################################
qt_alpha <<- 0.8  #0 - all weight on the new Qt, 1 - all weight on the current Qt
qt_beta <<- 0.7 # 
localization_distance <<- 1 #distance in meters were covariances in the process error are used
use_cov <<- TRUE
adapt_qt_method <<- 1  #0 = no adapt, 1 = variance in residuals, 2 = Rastetter et al 2011
num_adapt_days <<- 30

#################################
# Parameter calibration information
#################################

include_pars_in_qt_update <<- TRUE
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
zone1temp_init_qt <<- 0.1^2  #THIS IS THE VARIANCE, NOT THE SD

zone2_temp_init_mean <<- 15
zone2_temp_init_lowerbound <<-  10
zone2_temp_init_upperbound <<-  20
zone2_temp_lowerbound <<-  -100
zone2_temp_upperbound <<-  100
zone2temp_init_qt <<- 0.1^2 #THIS IS THE VARIANCE, NOT THE SD

#Shortwave factor
swf_init_mean <<- 1.0
swf_init_lowerbound <<- 0.5
swf_init_upperbound <<- 2.0
swf_lowerbound <<- -10
swf_upperbound <<- 10
#daily perturbance of parameter value
swf_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD

#Longwave factor
lwf_init_mean <<- 1.0
lwf_init_lowerbound <<- 0.5
lwf_init_upperbound <<- 2.0
lwf_lowerbound <<- -10
lwf_upperbound <<- 10
#daily perturbance of parameter value
lwf_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD

#Fsed_oxy
Fsed_oxy_init_mean <<- -20
Fsed_oxy_init_lowerbound <<-  -100 #-30 #-24.220
Fsed_oxy_init_upperbound <<-  -5 #10 #-24.218
Fsed_oxy_lowerbound <<-  -1000 #-30 #-24.220
Fsed_oxy_upperbound <<-  1000 #10 #-24.218
#daily perturbance of parameter value
Fsed_oxy_init_qt <<- 1^2 #THIS IS THE VARIANCE, NOT THE SD

#Rdom_minerl
Rdom_minerl_init_mean <<- 0.001
Rdom_minerl_init_lowerbound <<-  0.0005 #0.00049
Rdom_minerl_init_upperbound <<-  0.005 #0.00051
Rdom_minerl_lowerbound <<-  -1 #0.0000 #0.00049
Rdom_minerl_upperbound <<-  1 #0.00051
#daily perturbance of parameter value
Rdom_minerl_init_qt <<- 0.0001^2 #THIS IS THE VARIANCE, NOT THE SD

#R_growth
R_growth_init_mean <<-  1.0
R_growth_init_lowerbound <<-  0.5 #0.99
R_growth_init_upperbound <<-  1.5 #1.01
R_growth_lowerbound <<-  -10.00 #0.99
R_growth_upperbound <<-  10.00 #1.01
#daily perturbance of parameter value
R_growth_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD

#Rnitrif
Rnitrif_init_mean <<- 0.02
Rnitrif_init_lowerbound <<-  0.001 #0.99
Rnitrif_init_upperbound <<-  0.1 #1.01
Rnitrif_lowerbound <<-  -1 #0.99
Rnitrif_upperbound <<-  1 #1.01
#daily perturbance of parameter value
Rnitrif_init_qt <<- 0.001^2 #THIS IS THE VARIANCE, NOT THE SD

#w_p
w_p_init_mean <<- 0.0
w_p_init_lowerbound <<-  -0.1 #0.99
w_p_init_upperbound <<-  0.1 #1.01
w_p_lowerbound <<-  -0.1 #0.99
w_p_upperbound <<-  0.1 #1.01
#daily perturbance of parameter value
w_p_init_qt <<- 0.001^2 #THIS IS THE VARIANCE, NOT THE SD

#Fsed_frp
Fsed_frp_init_mean <<- 0.11
Fsed_frp_init_lowerbound <<-  0.01 #0.99
Fsed_frp_init_upperbound <<-  0.2 #1.01
Fsed_frp_lowerbound <<-  -1000 #0.99
Fsed_frp_upperbound <<-  1000 #1.01
#daily perturbance of parameter value
Fsed_frp_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD

#Fsed_amm
Fsed_amm_init_mean <<- 5
Fsed_amm_init_lowerbound <<-  2 #0.99
Fsed_amm_init_upperbound <<-  8 #1.01
Fsed_amm_lowerbound <<-  -1000 #0.99
Fsed_amm_upperbound <<-  1000 #1.01
#daily perturbance of parameter value
Fsed_amm_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD

#Fsed_nit
Fsed_nit_init_mean <<- -10
Fsed_nit_init_lowerbound <<-  -10 #0.99
Fsed_nit_init_upperbound <<-  0.0 #1.01
Fsed_nit_lowerbound <<-  -100 #0.99
Fsed_nit_upperbound <<-  100 #1.01
#daily perturbance of parameter value
Fsed_nit_init_qt <<- 0.1^2 #THIS IS THE VARIANCE, NOT THE SD

#Fsed_doc
Fsed_doc_init_mean <<- 50
Fsed_doc_init_lowerbound <<-  10 #0.99
Fsed_doc_init_upperbound <<-  60 #1.01
Fsed_doc_lowerbound <<-  -1000 #0.99
Fsed_doc_upperbound <<-  1000 #1.01
#daily perturbance of parameter value
Fsed_doc_init_qt <<- 0.1^2 #THIS IS THE VARIANCE, NOT THE SD

#Create parameter vectors
if(include_wq){
  par_names <<- c(
    "sed_temp_mean"
    ,"sed_temp_mean"
    ,"sw_factor"
    ,"lw_factor"
    #,"Fsed_oxy"
    #,"pd%R_growth"
    #,"Rnitrif"
    #,"Fsed_frp"
    #,"Rdom_minerl"
    #,"Fsed_nit"
    #,"Fsed_amm"
    #, "Fsed_doc"
  )
  par_names_save <<- c(
    "zone1temp"
    ,"zone2temp"
    ,"sw_factor"
    ,"lw_factor"
    #,"Fsed_oxy"
    #,"R_growth"
    #,"Rnitrif"
    #,"Fsed_frp"
    #,"Rdom_minerl"
    #,"Fsed_nit"
    #,"Fsed_amm"
    #,"Fsed_doc"
  )
  par_nml <<- c(
    "glm3.nml"
    ,"glm3.nml"
    ,"glm3.nml"
    ,"glm3.nml"
    #,"aed2.nml"
    #,"aed2_phyto_pars.nml"
    #,"aed2.nml"
    #,"aed2.nml"
    #, "aed2.nml"
    #, "aed2.nml"
    #, "aed2.nml"
    #, "aed2.nml"
  ) 
  par_init_mean <<- c(
    zone1_temp_init_mean
    ,zone2_temp_init_mean
    ,swf_init_mean
    ,lwf_init_mean
    #,Fsed_oxy_init_mean
    #,R_growth_init_mean
    #,Rnitrif_init_mean
    #,Fsed_frp_init_mean
    #,Rdom_minerl_init_mean
    #,Fsed_nit_init_mean
    #,Fsed_amm_init_mean
    #,Fsed_doc_init_mean
  )
  par_init_lowerbound <<- c(
    zone1_temp_init_lowerbound
    ,zone2_temp_init_lowerbound
    ,swf_init_lowerbound
    ,lwf_init_lowerbound
    #,Fsed_oxy_init_lowerbound
    #,R_growth_init_lowerbound
    #,Rnitrif_init_lowerbound
    #,Fsed_frp_init_lowerbound
    #,Rdom_minerl_init_lowerbound
    #,Fsed_nit_init_lowerbound
    #,Fsed_amm_init_lowerbound
    #,Fsed_doc_init_lowerbound
  )
  par_init_upperbound <<- c(
    zone1_temp_init_upperbound
    ,zone2_temp_init_upperbound
    ,swf_init_upperbound
    ,lwf_init_upperbound
    #,Fsed_oxy_init_upperbound
    #,R_growth_init_upperbound
    #,Rnitrif_init_upperbound 
    #,Fsed_frp_init_upperbound
    #,Rdom_minerl_init_upperbound
    #,Fsed_nit_init_upperbound
    #,Fsed_amm_init_upperbound
    #,Fsed_doc_init_upperbound
  )
  par_lowerbound <<- c(
    zone1_temp_lowerbound
    ,zone2_temp_lowerbound 
    ,swf_lowerbound
    ,lwf_lowerbound
    #,Fsed_oxy_lowerbound
    #,R_growth_lowerbound
    #,Rnitrif_lowerbound
    #,Fsed_frp_lowerbound
    #,Rdom_minerl_lowerbound
    #,Fsed_nit_lowerbound 
    #,Fsed_amm_lowerbound 
    #,Fsed_doc_lowerbound
  )
  par_upperbound <<- c(
    zone1_temp_upperbound
    ,zone2_temp_upperbound
    ,swf_upperbound
    ,lwf_upperbound
    #,Fsed_oxy_upperbound
    #,R_growth_upperbound
    #,Rnitrif_upperbound
    #,Fsed_frp_upperbound
    #,Rdom_minerl_upperbound
    #,Fsed_nit_upperbound
    #,Fsed_amm_upperbound
    #,Fsed_doc_upperbound
  )
  par_init_qt <<- c(
    zone1temp_init_qt
    ,zone2temp_init_qt
    ,swf_init_qt
    ,lwf_init_qt
    #,Fsed_oxy_init_qt
    #,R_growth_init_qt
    #,Rnitrif_init_qt
    #,Fsed_frp_init_qt
    #,Rdom_minerl_init_qt
    #,Fsed_nit_init_qt
    #,Fsed_amm_init_qt
    #,Fsed_doc_init_qt
  )
  par_units <<- c(
    "deg_C"
    ,"deg_C"
    ,"-"
    ,"-"
    #,"-"
    #,"-"
    #,"-" 
    #,"-" 
    #,"-" 
    #,"-"
    #,"-" 
    #,"-"
  )
  
  
}else{
  par_names <<- c() #c("sed_temp_mean","sed_temp_mean")
  par_names_save <<- c() #c("zone1temp","zone2temp")
  par_nml <<- c() #c("glm3.nml","glm3.nml")
  par_init_mean <<- c() #c(zone1_temp_init_mean,zone2_temp_init_mean)
  par_init_lowerbound <<- c() #c(zone1_temp_init_lowerbound,zone2_temp_init_lowerbound)
  par_init_upperbound <<- c() #c(zone1_temp_init_upperbound,zone2_temp_init_upperbound)
  par_lowerbound <<- c() #c(zone1_temp_lowerbound,zone2_temp_lowerbound)
  par_upperbound <<- c() #c(zone1_temp_upperbound,zone2_temp_upperbound)
  par_init_qt <<- c() #c(zone1temp_init_qt,zone2temp_init_qt)
  par_units <<- c() #("deg_C","deg_C") #
  
  par_names <<- c("sed_temp_mean"
                  ,"sed_temp_mean"
                  ,"sw_factor"
                  ,"lw_factor")
  par_names_save <<- c("zone1temp"
                       ,"zone2temp"
                       ,"sw_factor"
                       ,"lw_factor")
  par_nml <<- c("glm3.nml"
                ,"glm3.nml"
                ,"glm3.nml"
                ,"glm3.nml")
  par_init_mean <<- c(zone1_temp_init_mean
                      ,zone2_temp_init_mean
                      ,swf_init_mean
                      , lwf_init_mean)
  par_init_lowerbound <<- c(zone1_temp_init_lowerbound
                            ,zone2_temp_init_lowerbound
                            ,swf_init_lowerbound
                            ,lwf_init_lowerbound)
  par_init_upperbound <<- c(zone1_temp_init_upperbound
                            ,zone2_temp_init_upperbound
                            ,swf_init_upperbound
                            , lwf_init_upperbound)
  par_lowerbound <<- c(zone1_temp_lowerbound
                       ,zone2_temp_lowerbound
                       ,swf_lowerbound
                       , lwf_lowerbound)
  par_upperbound <<- c(zone1_temp_upperbound
                       ,zone2_temp_upperbound
                       ,swf_upperbound
                       , lwf_upperbound)
  par_init_qt <<- c(zone1temp_init_qt
                    ,zone2temp_init_qt
                    ,swf_init_qt
                    , lwf_init_qt)
  par_units <<- c("deg_C"
                  ,"deg_C"
                  ,"-"
                  ,"-") #
  
  #par_init_qt <- par_init_qt * 0.1
}


#####################################
###  Observation information
######################################


use_ctd <<- FALSE
ctd_fname <<- paste0(data_location,"/manual-data/CTD_Meta_13_18_final.csv")
#Use CTD data in place of the sensor string
use_nutrient_data <<- FALSE
nutrients_fname <<- paste0(data_location,"/manual-data/chemistry.csv")
#Use EDI formated nutrient file

observed_depths_temp <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
#Depths with temperature observations

observed_depths_do <<- c(1, 5, 9)
#Depths with do observations

observed_depths_chla_fdom <<- 1
#Depths with Chla and fdom observations

temp_obs_fname <<- c(paste0(data_location,"/mia-data/Catwalk.csv"),paste0(data_location, "/manual-data/Catwalk_cleanedEDI.csv"))
#temp_obs_fname <<- paste0(data_location,"/mia-data/Catwalk.csv")
#name of catwalk file name
#Catwalk has the temperature string, fdom, chla, and do sensors

met_obs_fname <<- c(paste0(data_location,"/carina-data/FCRmet.csv"),paste0(data_location, "/manual-data/Met_final_2015_2018.csv"))
#met_obs_fname <<- c(paste0(data_location,"/carina-data/FCRmet.csv"))
#Name of meteorology file name
#2013 - dec 2018

inflow_file1 <<- c(paste0(data_location,"/diana-data/FCRweir.csv"),
                   paste0(data_location,"/manual-data/FCR_weir_inflow_newEDI_2013_2018_20190911_oneDOC.csv"),
                   paste0(data_location,"/manual-data/inflow_working_2019.csv"))

chemistry_file <<- paste0(data_location,"/manual-data/FCR_weir_inflow_newEDI_2013_2018_20190911_oneDOC.csv")


#inflow_file1 <<- paste0(data_location,"/manual-data/FCR_weir_inflow_newEDI_2013_2018_20190911_oneDOC.csv")
outflow_file1 <<- paste0(data_location,"/manual-data/FCR_spillway_outflow_newEDI_SUMMED_WeirWetland_2013_2018_20190912.csv")

include_wetland_inflow <<- FALSE
inflow_file2 <<- paste0(data_location,"/manual-data/FCR_wetland_inflow_newEDI_2013_2018_20190912_oneDOC.csv")
#Name of the historical inflow and outflow files


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
init_donc <<- 0.1/47.4
init_dopc <<- 0.1/47.4
init_ponc <<- (0.1/78.5)
init_popc <<-(0.1/78.5)

#carbon to chlorophyll ratio (mg C/mg chla)
#12 g/ mole of C vs. X g/ mole of chla
#Initial concentration of phytoplankton (mmol C/m3)
biomass_to_chla <<- c((50/12),(50/12), (50/12))

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

proportional_obs_error <<- TRUE

#uncertainy in temperature measurement
obs_error_temperature_intercept <<- 0.0001 #NEED TO DOUBLE CHECK

obs_error_temperature_slope <<- 0.0 #NEED TO DOUBLE CHECK

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



ctd_2_exo_chla <- c(-0.35, 1.9)

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
focal_depths_wq <<- c(2,5,9)
#Depths that are plotted for the manager plot
focal_depths_manager <<- c(4,16,25) #c(2, 4, 9) #c(4,16,25) #c(4,16,25)
#Indexes for the depths that are compared to calculate turnover
turnover_index_1 <<- 4 #1 #4
turnover_index_2 <<- 25 #8 #25

####################################
# Extra options that you will not adjust
####################################
hold_inflow_outflow_constant <<- FALSE
print_glm2screen <<- FALSE