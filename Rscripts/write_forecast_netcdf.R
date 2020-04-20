write_forecast_netcdf <- function(x,
                                  full_time_local,
                                  qt,
                                  modeled_depths,
                                  save_file_name,
                                  x_restart,
                                  qt_restart,
                                  time_of_forecast,
                                  hist_days,
                                  x_prior,
                                  include_wq,
                                  wq_start,
                                  wq_end,
                                  z,
                                  nstates,
                                  npars,
                                  GLMversion,
                                  FLAREversion,
                                  local_tzone,                      
                                  surface_height_restart,
                                  snow_ice_restart,
                                  snow_ice_thickness,
                                  surface_height,
                                  avg_surf_temp_restart,
                                  x_phyto_groups_restart,
                                  x_phyto_groups,
                                  running_residuals,
                                  mixing_restart,
                                  glm_depths_restart,
                                  forecast_location){
  
  obs <- z
  
  num_wq_vars <- length(wq_end)
  
  ncfname <- paste0(forecast_location,"/",save_file_name,'.nc')
  #Set dimensions
  ens <- seq(1,dim(x)[2],1)
  depths <- modeled_depths
  t <- as.numeric(as.POSIXct(full_time_local,tz=local_tzone,origin = '1970-01-01 00:00.00 UTC'))
  states <- seq(1,nstates,1)
  states_aug <- seq(1,dim(x)[3],1)
  obs_states <- seq(1,dim(z)[2],1)
  qt_update_days <- seq(1,dim(running_residuals)[1],1)
  mixing_restart_vars <- seq(1, dim(mixing_restart)[2], 1)
  
  if(include_wq & "PHY_TCHLA" %in% wq_names){
    phytos_restart <- seq(1, dim(x_phyto_groups_restart)[2],1)
  }
  
  restart_depths <- seq(1, dim(glm_depths_restart)[2])
  
  #Set variable that states whether value is forecasted
  forecasted <- rep(1,length(t))
  forecasted[1:(hist_days+1)] <- 0
  
  ice_thickness <- snow_ice_thickness[, ,2] + snow_ice_thickness[, , 3]
  
  #Create summary output
  mean_temp <- array(NA,dim=c(length(t),length(depths)))
  upper95_temp <- array(NA,dim=c(length(t),length(depths)))
  lower95_temp <- array(NA,dim=c(length(t),length(depths)))
  for(i in 1:length(depths)){
    for(j in 1:length(t)){
      mean_temp[j,i] <- mean(x[j,,i])
      lower95_temp[j,i] <- quantile(x[j,,i],0.025)
      upper95_temp[j,i] <- quantile(x[j,,i],0.975)
    }
  }
  
  #Define dims
  ensdim <- ncdim_def("ens",units = "",vals = ens, longname = 'ensemble member') 
  depthdim <- ncdim_def("z",units = "meters",vals = as.double(depths), longname = 'Depth from surface') 
  timedim <- ncdim_def("time",units = 'seconds', longname = 'seconds since 1970-01-01 00:00.00 UTC',vals = t)
  statedim <- ncdim_def("states",units = '', vals = states)
  stateagudim <- ncdim_def("states_aug",units = '', vals = states_aug, longname = 'length of model states plus parameters')
  obsdim <- ncdim_def("obs_dim",units = '', vals = obs_states, longname = 'length of ')
  snow_ice_dim <- ncdim_def("snow_ice_dim",units = "",vals = c(1, 2, 3), longname = 'snow ice dims') 
  qt_update_days_dim <- ncdim_def("qt_update_days",units = '', vals = qt_update_days, longname = 'Number of running days that qt smooths over')
  mixing_restart_vars_dim <- ncdim_def("mixing_restart_vars_dim",units = '', vals = mixing_restart_vars, longname = 'number of mixing restart variables')
  depth_restart_vars_dim <- ncdim_def("depth_restart_vars_dim",units = '', vals = restart_depths, longname = 'number of possible depths that are simulated in GLM')
  
  
  if(include_wq & "PHY_TCHLA" %in% wq_names){
    phyto_restart_dim <- ncdim_def("phyto_restart_dim",units = "",vals = phytos_restart, longname = 'phyto_restart_dim') 
  }
  
  #Define variables
  fillvalue <- 1e32
  
  def_list <- list()
  def_list[[1]] <- ncvar_def("temp","deg_C",list(timedim,ensdim,depthdim),fillvalue,'temperature',prec="single")
  def_list[[2]] <- ncvar_def("temp_mean","deg_C",list(timedim,depthdim),fillvalue,'temperature_mean',prec="single")
  def_list[[3]] <- ncvar_def("temp_upperCI","deg_C",list(timedim,depthdim),fillvalue,'temperature_upperCI',prec="single")
  def_list[[4]] <- ncvar_def("temp_lowerCI","deg_C",list(timedim,depthdim),fillvalue,'temperature_lowerCI',prec="single")
  def_list[[5]] <- ncvar_def("qt_restart","-",list(stateagudim,stateagudim),fillvalue,'restart covariance matrix',prec="float")
  def_list[[6]]  <- ncvar_def("x_restart","-",list(ensdim,stateagudim),fillvalue,'matrix for restarting EnKF',prec="float")
  def_list[[7]] <- ncvar_def("x_prior","-",list(timedim,ensdim,stateagudim),fillvalue,'Predicted states prior to Kalman correction',prec="float")
  def_list[[8]] <- ncvar_def("obs","various",list(timedim,obsdim),fillvalue,'temperature observations',prec="single")
  def_list[[9]] <- ncvar_def("forecasted","-",list(timedim),missval = -99,longname = '0 = historical; 1 = forecasted',prec="integer")
  def_list[[10]] <- ncvar_def("surface_height_restart","m",list(ensdim),missval = -99,longname =  'Surface Height',prec="single")
  def_list[[11]] <- ncvar_def("snow_ice_restart","m",list(ensdim, snow_ice_dim),missval = -99,longname = 'Snow (1), White Ice (2), Blue Ice (3)',prec="single")
  def_list[[12]] <- ncvar_def("ice_thickness","m", list(timedim,ensdim),missval = -99,longname = 'Ice Thickness',prec="single")
  def_list[[13]] <- ncvar_def("lake_depth","m",list(timedim,ensdim),missval = -99,longname = 'Depth of lake',prec="single")
  def_list[[14]] <- ncvar_def("avg_surf_temp_restart","deg_C",list(ensdim),missval = -99,longname ='Running Average of Surface Temperature',prec="single")
  def_list[[15]] <- ncvar_def("running_residuals","various",list(qt_update_days_dim,statedim),fillvalue,longname = "running residual for updating qt",prec="single")
  def_list[[16]] <- ncvar_def("mixing_restart","various",list(ensdim,mixing_restart_vars_dim),fillvalue,longname = "variables required to restart mixing",prec="single")
  def_list[[17]] <- ncvar_def("depths_restart","various",list(ensdim,depth_restart_vars_dim),fillvalue,longname = "depths simulated by glm that are required to restart ",prec="single")
  
    if(include_wq & "PHY_TCHLA" %in% wq_names){
    index <- 18
    def_list[[index]] <- ncvar_def("phyto_restart","mmol/m3",list(ensdim,phyto_restart_dim),missval = -99,longname ='Restart Phyto biomass',prec="single")
  }else{
    index <- 17
  }
  if(npars > 0){
    for(par in 1:npars){
      def_list[[index+par]] <-ncvar_def(par_names_save[par],par_units[par],list(timedim,ensdim),fillvalue,par_names_save[par],prec="single")
    }
  }
  
  if(include_wq){
    def_list[[index+npars+1]] <- ncvar_def("OXY_oxy","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'OXY_oxy',prec="single")
    if("PHY_TCHLA" %in% wq_names){
    def_list[[index+npars+2]]<- ncvar_def("CAR_pH","-",list(timedim,ensdim,depthdim),fillvalue,'CAR_pH',prec="single")
    def_list[[index+npars+3]] <- ncvar_def("CAR_dic","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'CAR_dic',prec="single")
    def_list[[index+npars+4]] <- ncvar_def("CAR_ch4","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'CAR_ch4',prec="single")
    def_list[[index+npars+5]] <- ncvar_def("SIL_rsi","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'SIL_rsi',prec="single")
    def_list[[index+npars+6]] <- ncvar_def("NIT_amm","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'NIT_amm',prec="single")
    def_list[[index+npars+7]] <- ncvar_def("NIT_nit","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'NIT_nit',prec="single")
    def_list[[index+npars+8]] <- ncvar_def("PHS_frp","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'PHS_frp',prec="single")
    def_list[[index+npars+9]] <- ncvar_def("OGM_doc","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'OGM_doc',prec="single")
    def_list[[index+npars+10]] <- ncvar_def("OGM_poc","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'OGM_poc',prec="single")
    def_list[[index+npars+11]] <- ncvar_def("OGM_don","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'OGM_don',prec="single")
    def_list[[index+npars+12]] <- ncvar_def("OGM_pon","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'OGM_pon',prec="single")
    def_list[[index+npars+13]] <- ncvar_def("OGM_dop","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'OGM_dop',prec="single")
    def_list[[index+npars+14]] <- ncvar_def("OGM_pop","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'OGM_pop',prec="single")
    def_list[[index+npars+15]] <- ncvar_def("NCS_ss1","",list(timedim,ensdim,depthdim),fillvalue,'NCS_ss1',prec="single")
    def_list[[index+npars+16]] <- ncvar_def("PHS_frp_ads","mmol/m3",list(timedim,ensdim,depthdim),fillvalue,'PHS_frp_ads',prec="single")
    def_list[[index+npars+17]] <- ncvar_def("PHY_TCHLA","mg/L",list(timedim,ensdim,depthdim),fillvalue,'PHY_TCHLA',prec="single")
    for(phyto in 1:length(tchla_components_vars)){
      def_list[[index+npars +17 +phyto]] <- ncvar_def(tchla_components_vars[phyto],"mmol/m3",list(timedim,ensdim,depthdim),fillvalue,tchla_components_vars[phyto],prec="single")
    }
    }
  }
  
  ncout <- nc_create(ncfname,def_list,force_v4=T)
  
  # create netCDF file and put arrays
  ncvar_put(ncout,def_list[[1]] ,x[,,1:length(depths)])
  ncvar_put(ncout,def_list[[2]] ,mean_temp)
  ncvar_put(ncout,def_list[[3]] ,upper95_temp)
  ncvar_put(ncout,def_list[[4]] ,lower95_temp)
  ncvar_put(ncout,def_list[[5]] ,as.matrix(qt_restart))
  ncvar_put(ncout,def_list[[6]] ,as.matrix(x_restart))
  ncvar_put(ncout,def_list[[7]] ,as.matrix(x_prior))
  ncvar_put(ncout,def_list[[8]] ,obs)
  ncvar_put(ncout,def_list[[9]] ,as.array(forecasted))
  ncvar_put(ncout,def_list[[10]] ,surface_height_restart)
  ncvar_put(ncout,def_list[[11]] ,snow_ice_restart)
  ncvar_put(ncout,def_list[[12]] ,ice_thickness)
  ncvar_put(ncout,def_list[[13]] ,surface_height)
  ncvar_put(ncout,def_list[[14]] ,avg_surf_temp_restart)
  ncvar_put(ncout,def_list[[15]] ,running_residuals)
  ncvar_put(ncout,def_list[[16]] ,mixing_restart)
  ncvar_put(ncout,def_list[[17]] ,glm_depths_restart)
  if(include_wq & "PHY_TCHLA" %in% wq_names){
    index <- 18
    ncvar_put(ncout,def_list[[index]] ,x_phyto_groups_restart)
  }else{
    index <- 17
  }
  
  if(npars > 0){
    for(par in 1:npars){
      ncvar_put(ncout,def_list[[index + par]] ,x[,,nstates + par])
    }
  }
  
  if(include_wq){
    ncvar_put(ncout,def_list[[index+npars+1]],x[,,wq_start[1]:wq_end[1]])
    if("PHY_TCHLA" %in% wq_names){
    ncvar_put(ncout,def_list[[index+npars+2]],x[,,wq_start[2]:wq_end[2]])
    ncvar_put(ncout,def_list[[index+npars+3]],x[,,wq_start[3]:wq_end[3]])
    ncvar_put(ncout,def_list[[index+npars+4]],x[,,wq_start[4]:wq_end[4]])
    ncvar_put(ncout,def_list[[index+npars+5]],x[,,wq_start[5]:wq_end[5]])
    ncvar_put(ncout,def_list[[index+npars+6]],x[,,wq_start[6]:wq_end[6]])
    ncvar_put(ncout,def_list[[index+npars+7]],x[,,wq_start[7]:wq_end[7]])
    ncvar_put(ncout,def_list[[index+npars+8]],x[,,wq_start[8]:wq_end[8]])
    ncvar_put(ncout,def_list[[index+npars+9]],x[,,wq_start[9]:wq_end[9]])
    ncvar_put(ncout,def_list[[index+npars+10]],x[,,wq_start[10]:wq_end[10]])
    ncvar_put(ncout,def_list[[index+npars+11]],x[,,wq_start[11]:wq_end[11]])
    ncvar_put(ncout,def_list[[index+npars+12]],x[,,wq_start[12]:wq_end[12]])
    ncvar_put(ncout,def_list[[index+npars+13]],x[,,wq_start[13]:wq_end[13]])
    ncvar_put(ncout,def_list[[index+npars+14]],x[,,wq_start[14]:wq_end[14]])
    ncvar_put(ncout,def_list[[index+npars+15]],x[,,wq_start[15]:wq_end[15]])
    ncvar_put(ncout,def_list[[index+npars+16]],x[,,wq_start[16]:wq_end[16]])
    ncvar_put(ncout,def_list[[index+npars+17]],x[,,wq_start[17]:wq_end[17]])
    ndepths <- length(modeled_depths)
    for(phyto in 1:length(tchla_components_vars)){
      ncvar_put(ncout,def_list[[index+npars+17 + phyto]],x_phyto_groups[, , ((phyto-1)* ndepths + 1):(phyto*ndepths)])
    }
  }
  }
  
  
  #Global file metadata
  ncatt_put(ncout,0,"title",'Falling Creek Reservoir forecast', prec =  "text")
  ncatt_put(ncout,0,"institution",'Virginia Tech', prec =  "text")
  ncatt_put(ncout,0,"GLM_version",as.character(GLMversion), prec =  "text")
  ncatt_put(ncout,0,"FLARE_version",as.character(FLAREversion), prec =  "text")
  ncatt_put(ncout,0,"time_zone_of_simulation",as.character(local_tzone), prec =  "text")
  ncatt_put(ncout,0,"forecast_issue_time",as.character(time_of_forecast), 
            prec =  "text")
  #ncatt_put(ncout,0,"Conventions",)
  
  nc_close(ncout)
  
}

