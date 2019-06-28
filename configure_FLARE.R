#################################################
#Pull data from github?
#Push results to github?
pull_from_git <<- FALSE
push_to_git <<- FALSE

#Time zone that GLM is run in
#Currently needed to be GMT so that it interfaces with the NOAA forecast
reference_tzone <<- "GMT"
#Local time zone of the lake
local_tzone <<- "EST5EDT"
local_tzone <<- "GMT"

#Use GLM-AED?
include_wq <<- TRUE
#Use CTD data in place of the sensor string
use_ctd <<- FALSE

#Downscale the coarse resolutoin NOAA data to the local
#site using the meterology station at the lake
DOWNSCALE_MET <<- FALSE
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

single_run <<- TRUE

#Depths used in the EnKF
#This are the depths that are saved between days
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
                      9.00, 9.33)
}else{
  modeled_depths <<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9) 
}

#Note: this number is multiplied by 
# 1) the number of NOAA ensembles (21)
# 2) the number of downscaling essembles (50 is current)
# get to the total number of essembles
n_enkf_members <<- 1
n_ds_members <<- 1


#Init depth of lake
lake_depth_init <<- 9.4  #not a modeled state

#Initial zone temperatures and the upper and lower bounds
#Zone 1 is Xm to Xm 
#Zone 2 is Xm to Xm
zone2_temp_init_mean <<- 17 #11
zone2_temp_init_lowerbound <<- 5
zone2_temp_init_upperbound <<- 20
zone1_temp_init_mean <<- 11
zone1_temp_init_lowerbound <<- 5
zone1_temp_init_upperbound <<- 20
#daily perturbance of parameter value
zone1temp_init_qt <<- 0.01^2 #THIS IS THE VARIANCE, NOT THE SD
zone2temp_init_qt <<- 0.00000001^2 #THIS IS THE VARIANCE, NOT THE SD

#Shortwave factor
swf_init_mean <<- 0.75
swf_init_lowerbound <<- 0.50
swf_init_upperbound <<- 1.0
#daily perturbance of parameter value
swf_init_qt <<- 0.001^2 #THIS IS THE VARIANCE, NOT THE SD

#Fsed_oxy
Fsed_oxy_init_mean <<- -24.219
Fsed_oxy_init_lowerbound <<- -30
Fsed_oxy_init_upperbound <<- -10
#daily perturbance of parameter value
Fsed_oxy_init_qt <<- 0.1^2 #THIS IS THE VARIANCE, NOT THE SD

#Fsed_oxy
Rdom_minerl_init_mean <<- 0.0005
Rdom_minerl_init_lowerbound <<- 0.0001
Rdom_minerl_init_upperbound <<- 0.001
#daily perturbance of parameter value
Rdom_minerl_init_qt <<- 0.00000001^2 #THIS IS THE VARIANCE, NOT THE SD

#R_growth
R_growth_init_mean <<- 1
R_growth_init_lowerbound <<- 1.00001 #0.2
R_growth_init_upperbound <<- 1.5
#daily perturbance of parameter value
Rdom_minerl_init_qt <<- 0.000000001^2 #THIS IS THE VARIANCE, NOT THE SD

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

#The names of the phytoplankton groups that contribute to
#total chl-a in GLM
tchla_components_vars <<- c("PHY_CYANOPCH1")

#carbon to chlorophyll ratio (mg C/mg chla)
#12 g/ mole of C vs. X g/ mole of chla
#Initial concentration of phytoplankton (mmol C/m3)
biomass_to_chla <<- c(200/12)

#uncertainy in temperature measurement
obs_error_temperature <<- 0.0001 #NEED TO DOUBLE CHECK
#Observational uncertainty for each variable
obs_error_wq <<- c(0.001, #OXY_oxy
               NA, #CAR_pH
               NA, #CAR_dic
               NA, #CAR_ch4
               NA, #SIL_rsi
               NA, #NIT_amm
               NA, #NIT_nit
               NA, #PHS_frp
               0.001, #OGM_doc
               NA, #OGM_poc
               NA, #OGM_don
               NA, #OGM_pon
               NA, #OGM_dop
               NA, #OGM_pop
               0.001) #PHY_TCHLA
              
# Options for printing function
# Depths that the water quality variables are plotted
focal_depths_wq <<- c(1,5,9)
#Depths that are plotted for the manager plot
focal_depths_manager <<- c(1,5,9) #c(4,16,25)
#Indexes for the depths that are compared to calculate turnover
turnover_index_1 <<- 2
turnover_index_2 <<- 9

#Options that you will not adjust but are needed as global variables
npars <<- 6 
pre_scc <<- FALSE
hold_inflow_outflow_constant <<- FALSE
print_glm2screen <<- FALSE