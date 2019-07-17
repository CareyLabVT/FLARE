#################################################
#Pull data from github?
#Push results to github?
pull_from_git <<- TRUE
push_to_git <<- FALSE

#Time zone that GLM is run in
#Currently needed to be GMT so that it interfaces with the NOAA forecast
reference_tzone <<- "GMT"
#Local time zone of the lake
local_tzone <<- "EST"


#Use GLM-AED?
include_wq <<- FALSE
#Use CTD data in place of the sensor string
use_ctd <<- TRUE

#Downscale the coarse resolutoin NOAA data to the local
#site using the meterology station at the lake
DOWNSCALE_MET <<- TRUE
#file name of previous downscaling coefficients
#use NA if not using an existing file
downscaling_coeff <<- NA
#Dates to use to developing the downscaling coefficient
met_ds_obs_start <<- as.Date("2018-04-06")
met_ds_obs_end <<- as.Date("2018-12-06")

#GLM and FLARE verizon
#The code adds these to the output files
GLMversion <<- "GLM 3.0.0beta10"
FLAREversion <<- "v1.0_beta.1"

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
uncert_mode <<- 1

cov_matrix <<- "Qt_cov_matrix_init_AED.csv"

single_run <<- FALSE
use_future_met <<- FALSE

lake_latitude <<- 37.307   #Degrees North 
lake_longitude <<- 79.837  #Degrees West

#Depths used in the EnKF
#This are the depths that are saved between days
if(!include_wq){
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
  modeled_depths <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
}else{
  modeled_depths <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
}

#Note: this number is multiplied by 
# 1) the number of NOAA ensembles (21)
# 2) the number of downscaling essembles (50 is current)
# get to the total number of essembles
n_enkf_members <<- 1
n_ds_members <<- 21
n_inflow_outflow_members <<- 1

#Init depth of lake
lake_depth_init <<- 9.4  #not a modeled state

#Initial zone temperatures and the upper and lower bounds
#Zone 1 is Xm to Xm 
#Zone 2 is Xm to Xm
zone1_temp_init_mean <<- 9
zone1_temp_init_lowerbound <<- 5
zone1_temp_init_upperbound <<- 20
#daily perturbance of parameter value
zone1temp_init_qt <<- 0.1^2 #THIS IS THE VARIANCE, NOT THE SD

zone2_temp_init_mean <<- 9 #11
zone2_temp_init_lowerbound <<-  5
zone2_temp_init_upperbound <<-  20
zone2temp_init_qt <<- 0.1^2 #THIS IS THE VARIANCE, NOT THE SD

#Shortwave factor
swf_init_mean <<- 1.0
swf_init_lowerbound <<- 0.5
swf_init_upperbound <<- 1.5
#daily perturbance of parameter value
swf_init_qt <<- 0.001^2 #THIS IS THE VARIANCE, NOT THE SD

#Fsed_oxy
Fsed_oxy_init_mean <<- -24.219
Fsed_oxy_init_lowerbound <<-  -30 #-24.220
Fsed_oxy_init_upperbound <<-  10 #-24.218
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
R_growth_init_lowerbound <<-  0.75 #0.99
R_growth_init_upperbound <<-  1.25 #1.01
#daily perturbance of parameter value
R_growth_init_qt <<- 0.001^2 #THIS IS THE VARIANCE, NOT THE SD

#Parameter vectors
if(include_wq){
  #par_names <<- c()
  #par_names_save <<- c()
  #par_nml <<- c()
  #par_init_mean <<- c()
  #par_init_lowerbound <<- c()
  #par_init_upperbound <<- c()
  #par_init_qt <<- c()
  #par_units <<- c() #
  par_names <<- c("sed_temp_mean","sed_temp_mean","Fsed_oxy","pd%R_growth")
  par_names_save <<- c("zone1temp","zone2temp","Fsed_oxy","pd%R_growth")
  par_nml <<- c("glm3.nml","glm3.nml","aed2.nml","aed2_phyto_pars.nml")
  par_init_mean <<- c(zone1_temp_init_mean,zone2_temp_init_mean, Fsed_oxy_init_mean,R_growth_init_mean)
  par_init_lowerbound <<- c(zone1_temp_init_lowerbound,zone2_temp_init_lowerbound, Fsed_oxy_init_lowerbound ,R_growth_init_lowerbound)
  par_init_upperbound <<- c(zone1_temp_init_upperbound,zone2_temp_init_upperbound, Fsed_oxy_init_upperbound ,R_growth_init_upperbound)
  par_init_qt <<- c(zone1temp_init_qt,zone2temp_init_qt, Fsed_oxy_init_qt,R_growth_init_qt)
  par_units <<- c("deg_C","-","-","-") #
}else{
  #par_names <<- c("sw_factor")
  #par_names_save <<- c("SW_factor")
  #par_nml <<- c("glm3.nml")
  #par_init_mean <<- c(swf_init_mean)
  #par_init_lowerbound <<- c(swf_init_lowerbound)
  #par_init_upperbound <<- c(swf_init_upperbound)
  #par_init_qt <<- c(swf_init_qt)
  #par_units <<- c("-")
  par_names <<- c("sed_temp_mean","sed_temp_mean","swf_init_qt") #"Fsed_oxy","Rdom_minerl","pd%R_growth")
  par_names_save <<- c("zone1temp","zone2temp","swf_init_qt")
  par_nml <<- c("glm3.nml","glm3.nml","glm3.nml") #"aed2.nml","aed2.nml","aed2_phyto_pars.nml")
  par_init_mean <<- c(zone1_temp_init_mean,zone2_temp_init_mean,swf_init_mean)#Fsed_oxy_init_mean,Rdom_minerl_init_mean,Rdom_minerl_init_mean,R_growth_init_mean)
  par_init_lowerbound <<- c(zone1_temp_init_lowerbound,zone2_temp_init_lowerbound,swf_init_lowerbound)#Fsed_oxy_init_lowerbound,Rdom_minerl_init_lowerbound,R_growth_init_lowerbound)
  par_init_upperbound <<- c(zone1_temp_init_upperbound,zone2_temp_init_upperbound,swf_init_upperbound)#Fsed_oxy_init_upperbound,Rdom_minerl_init_upperbound,R_growth_init_upperbound)
  par_init_qt <<- c(zone1temp_init_qt,zone2temp_init_qt,swf_init_qt)#Fsed_oxy_init_qt,Rdom_minerl_init_qt,R_growth_init_qt)
  par_units <<- c("deg_C","deg_C","-") #
}

#Depths with temperature observations
observed_depths_temp <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9)

#Depths with do observations
observed_depths_do <<- c(1, 5, 9)
#Depths with Chla and fdom observations
observed_depths_chla_fdom <<- 1

#name of catwalk file name
#Catwalk has the temperature string, fdom, chla, and do sensors
temp_obs_fname <<- "Catwalk.csv"
#Name of meteorology file name
met_obs_fname <<- "FCRmet.csv"

#Name of the historical inflow and outflow files
inflow_file1 <<- "FCR_weir_inflow_newEDI_2013_2017_20190128_oneDOC.csv"
outflow_file1 <<- "FCR_spillway_outflow_newEDI_SUMMED_WeirWetland_2013_2017_20190128.csv"
inflow_file2 <<- "FCR_wetland_inflow_newEDI_2013_2017_20190305_oneDOC.csv"

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
               "PHY_TCHLA"
)

#Default initial states if lacking observations 
init_donc <<- 1.3/47.4
init_dopc <<-1.5/47.4
OXY_oxy_init <<- 300.62
CAR_pH_init <<- 6.5
CAR_dic_init <<- 59.1
CAR_ch4_init <<- 0.58
SIL_rsi_init <<- 300
NIT_amm_init <<- 0.69
NIT_nit_init <<- 0.05
PHS_frp_init <<- 0.07
OGM_doc_init <<- 100
OGM_poc_init <<- 78.5
OGM_pon_init <<- 8.3
OGM_pop_init <<- 8.3
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
obs_error_wq <<- c(0.25, #OXY_oxy #0.25
                   NA, #CAR_pH
                   NA, #CAR_dic
                   NA, #CAR_ch4
                   NA, #SIL_rsi
                   NA, #NIT_amm
                   NA, #NIT_nit
                   NA, #PHS_frp
                   1, #OGM_doc
                   NA, #OGM_poc
                   NA, #OGM_don
                   NA, #OGM_pon
                   NA, #OGM_dop
                   NA, #OGM_pop
                   0.25) #PHY_TCHLA

exo_2_ctd_chla <- c(0.184, 0.525)

OXY_oxy_process_error <<- 10
CAR_pH_process_error <<- 0.001
CAR_dic_process_error <<- 0.001
CAR_ch4_process_error <<- 0.001
SIL_rsi_process_error <<- 0.001
NIT_amm_process_error <<- 0.001
NIT_nit_process_error <<- 0.001
PHS_frp_process_error <<- 0.001
OGM_doc_process_error <<- 0.001
OGM_poc_process_error <<- 0.001
OGM_don_process_error <<- 0.001
OGM_pon_process_error <<- 0.001
OGM_dop_process_error <<- 0.001
OGM_pop_process_error <<- 0.001
PHY_TCHLA_process_error <<- 0.3

qt_alpha <<- 0.9  #0 - all weight on the new Qt, 1 - all weight on the current Qt
qt_beta <<- 0.75 # 

# Options for printing function
# Depths that the water quality variables are plotted
focal_depths_wq <<- c(1,5,9)
#Depths that are plotted for the manager plot
focal_depths_manager <<- c(1,5,9) #c(4,16,25)
#Indexes for the depths that are compared to calculate turnover
turnover_index_1 <<- 2
turnover_index_2 <<- 9

#Options that you will not adjust but are needed as global variables

pre_scc <<- FALSE
hold_inflow_outflow_constant <<- FALSE
print_glm2screen <<- FALSE