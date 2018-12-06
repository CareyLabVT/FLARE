write_forecast_netcdf <- function(x,full_time,qt,modeled_depths,save_file_name,x_restart,qt_restart,time_of_forecast,hist_days,x_prior,
                                  include_wq,wq_start,wq_end,par1,par2,par3,z,nstates,npars){
  
  obs <- z
  
  ncfname <- paste0(save_file_name,'.nc')
  #Set dimensions
  ens <- seq(1,dim(x)[2],1)
  depths <- modeled_depths
  t <- as.numeric(as.POSIXct(full_time,tz='EST5EDT',origin = '1970-01-01 00:00.00 UTC'))
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
  stateagudim <- ncdim_def("states_aug",units = '', vals = states_aug)
  obsdim <- ncdim_def("obs_dim",units = '', vals = obs_states)
  
  #Define variables
  fillvalue <- 1e32
  dlname <- 'temperature'
  tmp_def <- ncvar_def("temp","deg_C",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
  dlname <- 'temperature_mean'
  tmp_mean_def <- ncvar_def("temp_mean","deg_C",list(timedim,depthdim),fillvalue,dlname,prec="single")
  dlname <- 'temperature_upperCI'
  tmp_upper_def <- ncvar_def("temp_upperCI","deg_C",list(timedim,depthdim),fillvalue,dlname,prec="single")
  dlname <- 'temperature_lowerCI'
  tmp_lower_def <- ncvar_def("temp_lowerCI","deg_C",list(timedim,depthdim),fillvalue,dlname,prec="single")
  dlname <- 'zone 1 temperature'
  par1_def <- ncvar_def("zone1temp","deg_C",list(timedim,ensdim),fillvalue,dlname,prec="single")
  dlname <- 'zone 2 temperature'
  par2_def <- ncvar_def("zone2temp","deg_C",list(timedim,ensdim),fillvalue,dlname,prec="single")
  dlname <- 'Kw'
  par3_def <- ncvar_def("Kw","unitless",list(timedim,ensdim),fillvalue,dlname,prec="single")
  dlname <- 'restart covariance matrix'
  qt_restart_def <- ncvar_def("qt_restart","-",list(statedim,statedim),fillvalue,dlname,prec="float")
  dlname <- 'matrix for restarting EnKF'
  x_def <- ncvar_def("x_restart","-",list(ensdim,stateagudim),fillvalue,dlname,prec="float")
  dlname <- 'Predicted states prior to Kalman correction'
  x_prior_def <- ncvar_def("x_prior","-",list(timedim,ensdim,stateagudim),fillvalue,dlname,prec="float")
  dlname <- 'observations'
  obs_def <- ncvar_def("obs","various",list(timedim,obsdim),fillvalue,dlname,prec="single")
  
  fillvalue <- -99
  dlname <- '0 = historical; 1 = forecasted'
  forecast_def <- ncvar_def("forecasted","-",list(timedim),fillvalue,longname = dlname,prec="integer")
  
  
  
  if(include_wq){
    dlname <- 'OXY_oxy'
    wq_def1 <- ncvar_def("OXY_oxy","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'CAR_pH'
    wq_def2 <- ncvar_def("CAR_pH","-",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'CAR_dic'
    wq_def3 <- ncvar_def("CAR_dic","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'CAR_ch4'
    wq_def4 <- ncvar_def("CAR_ch4","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'SIL_rsi'
    wq_def5 <- ncvar_def("SIL_rsi","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'NIT_amm'
    wq_def6 <- ncvar_def("NIT_amm","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'NIT_nit'
    wq_def7 <- ncvar_def("NIT_nit","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'PHS_frp'
    wq_def8 <- ncvar_def("PHS_frp","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'OGM_doc'
    wq_def9 <- ncvar_def("OGM_doc","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'OGM_poc'
    wq_def10 <- ncvar_def("OGM_poc","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'OGM_don'
    wq_def11 <- ncvar_def("OGM_don","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'OGM_pon'
    wq_def12 <- ncvar_def("OGM_pon","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'OGM_dop'
    wq_def13 <- ncvar_def("OGM_dop","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'OGM_pop'
    wq_def14 <- ncvar_def("OGM_pop","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'PHY_CYANOPCH1'
    wq_def15 <- ncvar_def("PHY_CYANOPCH1","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'PHY_CYANONPCH2'
    wq_def16 <- ncvar_def("PHY_CYANONPCH2","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'PHY_CHLOROPCH3'
    wq_def17 <- ncvar_def("PHY_CHLOROPCH3","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'PHY_DIATOMPCH4'
    wq_def18 <- ncvar_def("PHY_DIATOMPCH4","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single") 
    dlname <- 'ZOO_COPEPODS1'
    wq_def19 <- ncvar_def("ZOO_COPEPODS1","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'ZOO_COPEPODS1'
    wq_def20 <- ncvar_def("ZOO_DAPHNIABIG2","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    dlname <- 'ZOO_COPEPODS1'
    wq_def21 <- ncvar_def("ZOO_DAPHNIASMALL3","umol/L",list(timedim,ensdim,depthdim),fillvalue,dlname,prec="single")
    
    ncout <- nc_create(ncfname,list(tmp_def,forecast_def,tmp_mean_def,tmp_upper_def,tmp_lower_def,x_def,par1_def,par2_def,par3_def,x_prior_def,obs_def,
                                    wq_def1,wq_def2,wq_def3,wq_def4,wq_def5,wq_def6,wq_def7,wq_def8,wq_def9,wq_def10,wq_def11,wq_def12, wq_def13,wq_def14,wq_def15,
                                    wq_def16,wq_def17,wq_def18,wq_def19,wq_def20,wq_def21),force_v4=T)
    
  }else{
    ncout <- nc_create(ncfname,list(tmp_def,forecast_def,tmp_mean_def,tmp_upper_def,tmp_lower_def,x_def,par1_def,par2_def,par3_def,x_prior_def,obs_def),force_v4=T)
  }
  
  # create netCDF file and put arrays
  ncvar_put(ncout,tmp_mean_def,mean_temp)
  
  ncvar_put(ncout,tmp_upper_def,upper95_temp)
  
  ncvar_put(ncout,tmp_lower_def,lower95_temp)
  
  ncvar_put(ncout,tmp_def,x[,,1:29])
  
  ncvar_put(ncout,par1_def,array(x[,,par1]))
  
  ncvar_put(ncout,par2_def,array(x[,,par2]))
  
  ncvar_put(ncout,par3_def,array(x[,,par3]))
  
  ncvar_put(ncout,forecast_def,as.array(forecasted))
  
  ncvar_put(ncout,x_def,as.matrix(x_restart))
  
  ncvar_put(ncout,x_prior_def,as.matrix(x_prior))
  
  #ncvar_put(ncout,qt_restart_def,as.matrix(qt_restart))
  
  ncvar_put(ncout,obs_def,obs)
  
  if(include_wq){
    ncvar_put(ncout,wq_def1,x[,,wq_start[1]:wq_end[1]])
    ncvar_put(ncout,wq_def2,x[,,wq_start[2]:wq_end[2]])
    ncvar_put(ncout,wq_def3,x[,,wq_start[3]:wq_end[3]])
    ncvar_put(ncout,wq_def4,x[,,wq_start[4]:wq_end[4]])
    ncvar_put(ncout,wq_def5,x[,,wq_start[5]:wq_end[5]])
    ncvar_put(ncout,wq_def6,x[,,wq_start[6]:wq_end[6]])
    ncvar_put(ncout,wq_def7,x[,,wq_start[7]:wq_end[7]])
    ncvar_put(ncout,wq_def8,x[,,wq_start[8]:wq_end[8]])
    ncvar_put(ncout,wq_def9,x[,,wq_start[9]:wq_end[9]])
    ncvar_put(ncout,wq_def10,x[,,wq_start[10]:wq_end[10]])
    ncvar_put(ncout,wq_def11,x[,,wq_start[11]:wq_end[11]])
    ncvar_put(ncout,wq_def12,x[,,wq_start[12]:wq_end[12]])
    ncvar_put(ncout,wq_def13,x[,,wq_start[13]:wq_end[13]])
    ncvar_put(ncout,wq_def14,x[,,wq_start[14]:wq_end[14]])
    ncvar_put(ncout,wq_def15,x[,,wq_start[15]:wq_end[15]])
    ncvar_put(ncout,wq_def16,x[,,wq_start[16]:wq_end[16]])
    ncvar_put(ncout,wq_def17,x[,,wq_start[17]:wq_end[17]])
    ncvar_put(ncout,wq_def18,x[,,wq_start[18]:wq_end[18]])
    ncvar_put(ncout,wq_def19,x[,,wq_start[19]:wq_end[19]])
    ncvar_put(ncout,wq_def20,x[,,wq_start[20]:wq_end[20]])
    ncvar_put(ncout,wq_def21,x[,,wq_start[21]:wq_end[21]])
  }
  
  
  #Global file metadata
  ncatt_put(ncout,0,"title",'Falling Creek Reservoir forecast')
  ncatt_put(ncout,0,"institution",'Virginia Tech')
  ncatt_put(ncout,0,"source",'GLMV3')
  #ncatt_put(ncout,0,"references",references$value)
  history <- paste('Run date:',time_of_forecast, sep=", ")
  ncatt_put(ncout,0,"history",history)
  #ncatt_put(ncout,0,"Conventions",)
  
  nc_close(ncout)
}

