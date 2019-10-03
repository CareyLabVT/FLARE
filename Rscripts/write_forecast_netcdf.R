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
                                  snow_ice_restart){
  
  obs <- z
  
  num_wq_vars <- length(wq_end)
  
  ncfname <- paste0(save_file_name,'.nc')
  #Set dimensions
  ens <- seq(1,dim(x)[2],1)
  depths <- modeled_depths
  t <- as.numeric(as.POSIXct(full_time_local,tz=local_tzone,origin = '1970-01-01 00:00.00 UTC'))
  states <- seq(1,nstates,1)
  states_aug <- seq(1,dim(x)[3],1)
  obs_states <- seq(1,dim(z)[2],1)
  
  #Set variable that states whether value is forecasted
  forecasted <- rep(1,length(t))
  forecasted[1:(hist_days+1)] <- 0
  
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
  def_list[[9]] <- ncvar_def("forecasted","-",list(timedim),missval = -99,longname = dlname <- '0 = historical; 1 = forecasted',prec="integer")
  def_list[[10]] <- ncvar_def("surface_height_restart","-",list(ensdim),missval = -99,longname = dlname <- 'Surface Height',prec="integer")
  def_list[[11]] <- ncvar_def("snow_ice_restart","-",list(ensdim, snow_ice_dim),missval = -99,longname = dlname <- 'Snow (1), White Ice (2), Blue Ice (3)',prec="integer")
  
  if(npars > 0){
    for(par in 1:npars){
      def_list[[11+par]] <-ncvar_def(par_names_save[par],par_units[par],list(timedim,ensdim),fillvalue,par_names_save[par],prec="single")
    }
  }
  
  if(include_wq){
    def_list[[11+npars+1]] <- ncvar_def("OXY_oxy","umol/L",list(timedim,ensdim,depthdim),fillvalue,'OXY_oxy',prec="single")
    def_list[[11+npars+2]]<- ncvar_def("CAR_pH","-",list(timedim,ensdim,depthdim),fillvalue,'CAR_pH',prec="single")
    def_list[[11+npars+3]] <- ncvar_def("CAR_dic","umol/L",list(timedim,ensdim,depthdim),fillvalue,'CAR_dic',prec="single")
    def_list[[11+npars+4]] <- ncvar_def("CAR_ch4","umol/L",list(timedim,ensdim,depthdim),fillvalue,'CAR_ch4',prec="single")
    def_list[[11+npars+5]] <- ncvar_def("SIL_rsi","umol/L",list(timedim,ensdim,depthdim),fillvalue,'SIL_rsi',prec="single")
    def_list[[11+npars+6]] <- ncvar_def("NIT_amm","umol/L",list(timedim,ensdim,depthdim),fillvalue,'NIT_amm',prec="single")
    def_list[[11+npars+7]] <- ncvar_def("NIT_nit","umol/L",list(timedim,ensdim,depthdim),fillvalue,'NIT_nit',prec="single")
    def_list[[11+npars+8]] <- ncvar_def("PHS_frp","umol/L",list(timedim,ensdim,depthdim),fillvalue,'PHS_frp',prec="single")
    def_list[[11+npars+9]] <- ncvar_def("OGM_doc","umol/L",list(timedim,ensdim,depthdim),fillvalue,'OGM_doc',prec="single")
    def_list[[11+npars+10]] <- ncvar_def("OGM_poc","umol/L",list(timedim,ensdim,depthdim),fillvalue,'OGM_poc',prec="single")
    def_list[[11+npars+11]] <- ncvar_def("OGM_don","umol/L",list(timedim,ensdim,depthdim),fillvalue,'OGM_don',prec="single")
    def_list[[11+npars+12]] <- ncvar_def("OGM_pon","umol/L",list(timedim,ensdim,depthdim),fillvalue,'OGM_pon',prec="single")
    def_list[[11+npars+13]] <- ncvar_def("OGM_dop","umol/L",list(timedim,ensdim,depthdim),fillvalue,'OGM_dop',prec="single")
    def_list[[11+npars+14]] <- ncvar_def("OGM_pop","umol/L",list(timedim,ensdim,depthdim),fillvalue,'OGM_pop',prec="single")
    def_list[[11+npars+15]] <- ncvar_def("NCS_ss1","",list(timedim,ensdim,depthdim),fillvalue,'NCS_ss1',prec="single")
    def_list[[11+npars+16]] <- ncvar_def("PHS_frp_ads","umol/L",list(timedim,ensdim,depthdim),fillvalue,'PHS_frp_ads',prec="single")
    def_list[[11+npars+17]] <- ncvar_def("PHY_AGGREGATE","umol/L",list(timedim,ensdim,depthdim),fillvalue,'PHY_AGGREGATE',prec="single")
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
  
  if(npars > 0){
    for(par in 1:npars){
      ncvar_put(ncout,def_list[[11 + par]] ,x[,,nstates + par])
    }
  }
  
  if(include_wq){
    ncvar_put(ncout,def_list[[11+npars+1]],x[,,wq_start[1]:wq_end[1]])
    ncvar_put(ncout,def_list[[11+npars+2]],x[,,wq_start[2]:wq_end[2]])
    ncvar_put(ncout,def_list[[11+npars+3]],x[,,wq_start[3]:wq_end[3]])
    ncvar_put(ncout,def_list[[11+npars+4]],x[,,wq_start[4]:wq_end[4]])
    ncvar_put(ncout,def_list[[11+npars+5]],x[,,wq_start[5]:wq_end[5]])
    ncvar_put(ncout,def_list[[11+npars+6]],x[,,wq_start[6]:wq_end[6]])
    ncvar_put(ncout,def_list[[11+npars+7]],x[,,wq_start[7]:wq_end[7]])
    ncvar_put(ncout,def_list[[11+npars+8]],x[,,wq_start[8]:wq_end[8]])
    ncvar_put(ncout,def_list[[11+npars+9]],x[,,wq_start[9]:wq_end[9]])
    ncvar_put(ncout,def_list[[11+npars+10]],x[,,wq_start[10]:wq_end[10]])
    ncvar_put(ncout,def_list[[11+npars+11]],x[,,wq_start[11]:wq_end[11]])
    ncvar_put(ncout,def_list[[11+npars+12]],x[,,wq_start[12]:wq_end[12]])
    ncvar_put(ncout,def_list[[11+npars+13]],x[,,wq_start[13]:wq_end[13]])
    ncvar_put(ncout,def_list[[11+npars+14]],x[,,wq_start[14]:wq_end[14]])
    ncvar_put(ncout,def_list[[11+npars+15]],x[,,wq_start[15]:wq_end[15]])
    ncvar_put(ncout,def_list[[11+npars+16]],x[,,wq_start[16]:wq_end[16]])
    ncvar_put(ncout,def_list[[11+npars+17]],x[,,wq_start[17]:wq_end[17]])
  }
  
  
  #Global file metadata
  ncatt_put(ncout,0,"title",'Falling Creek Reservoir forecast', prec =  "text")
  ncatt_put(ncout,0,"institution",'Virginia Tech', prec =  "text")
  ncatt_put(ncout,0,"GLM_version",as.character(GLMversion), prec =  "text")
  ncatt_put(ncout,0,"FLARE_version",as.character(FLAREversion), prec =  "text")
  ncatt_put(ncout,0,"time_zone_of_simulation",as.character(local_tzone), prec =  "text")
  #ncatt_put(ncout,0,"references",references$value)
  history <- paste('Run date:',time_of_forecast, sep=", ")
  ncatt_put(ncout,0,"history",history, prec =  "text")
  #ncatt_put(ncout,0,"Conventions",)
  
  nc_close(ncout)
  
}

