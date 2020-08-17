write_forecast_netcdf <- function(x,
                                  full_time_local,
                                  qt,
                                  modeled_depths,
                                  save_file_name,
                                  x_restart,
                                  qt_restart,
                                  forecast_iteration_id,
                                  forecast_project_id,
                                  hist_days,
                                  x_prior,
                                  include_wq,
                                  wq_start,
                                  wq_end,
                                  z,
                                  nstates,
                                  pars_config,
                                  GLMversion,
                                  FLAREversion,
                                  local_tzone,                      
                                  surface_height_restart,
                                  snow_ice_restart,
                                  snow_ice_thickness,
                                  surface_height,
                                  avg_surf_temp_restart,
                                  running_residuals,
                                  mixing_restart,
                                  glm_depths_restart,
                                  forecast_location,
                                  states_config,
                                  obs_config,
                                  diagnostics_names,
                                  diagnostics,
                                  data_assimilation_flag,
                                  time_of_forecast){
  
  obs <- z
  
  num_wq_vars <- length(wq_end)
  
  x_efi <- aperm(x, c(1,3,2))
  diagnostics_efi <- aperm(diagnostics, c(1,3,2, 4))
  
  ncfname <- paste0(forecast_location,"/",save_file_name,'.nc')
  #Set dimensions
  ens <- seq(1,dim(x)[2],1)
  depths <- modeled_depths
  t <- as.numeric(as.POSIXct(with_tz(full_time_local),origin = '1970-01-01 00:00.00 UTC'))
  states <- seq(1,nstates,1)
  states_aug <- seq(1,dim(x)[3],1)
  obs_states <- seq(1,dim(z)[3],1)
  qt_update_days <- seq(1,dim(running_residuals)[1],1)
  mixing_restart_vars <- seq(1, dim(mixing_restart)[2], 1)
  
  npars <- nrow(pars_config)
  
  restart_depths <- seq(1, dim(glm_depths_restart)[2])
  
  #Set variable that states whether value is forecasted
  forecasted <- rep(1,length(t))
  forecasted[1:(hist_days+1)] <- 0
  
  ice_thickness <- snow_ice_thickness[, ,2] + snow_ice_thickness[, , 3]
  
  #Define dims
  ensdim <- ncdim_def("ensemble",units = "-",vals = ens, longname = 'ensemble member') 
  depthdim <- ncdim_def("depth",units = "meters",vals = as.double(depths), longname = 'Depth from surface') 
  timedim <- ncdim_def("time",units = 'seconds', longname = 'seconds since 1970-01-01 00:00.00 UTC',vals = t)
  statedim <- ncdim_def("states",units = '', vals = states)
  stateagudim <- ncdim_def("states_aug",units = '', vals = states_aug, longname = 'length of model states plus parameters')
  obsdim <- ncdim_def("obs_dim",units = '', vals = obs_states, longname = 'length of ')
  snow_ice_dim <- ncdim_def("snow_ice_dim",units = "",vals = c(1, 2, 3), longname = 'snow ice dims') 
  qt_update_days_dim <- ncdim_def("qt_update_days",units = '', vals = qt_update_days, longname = 'Number of running days that qt smooths over')
  mixing_restart_vars_dim <- ncdim_def("mixing_restart_vars_dim",units = '', vals = mixing_restart_vars, longname = 'number of mixing restart variables')
  depth_restart_vars_dim <- ncdim_def("depth_restart_vars_dim",units = '', vals = restart_depths, longname = 'number of possible depths that are simulated in GLM')
  
  
  #Define variables
  fillvalue <- 1e32
  
  def_list <- list()
  def_list[[1]] <- ncvar_def("temp","degC",list(timedim,depthdim, ensdim),fillvalue,'state: temperature',prec="single")
  def_list[[2]] <- ncvar_def("qt_restart","dimensionless",list(statedim,statedim),fillvalue,'restart covariance matrix',prec="float")
  def_list[[3]]  <- ncvar_def("x_restart","dimensionless",list(ensdim,stateagudim),fillvalue,'matrix for restarting EnKF',prec="float")
  def_list[[4]] <- ncvar_def("x_prior","dimensionless",list(timedim,ensdim,stateagudim),fillvalue,'Predicted states prior to Kalman correction',prec="float")
  def_list[[5]] <- ncvar_def("data_assimilation","dimensionless",list(timedim),missval = -99,longname = '0 = historical; 1 = forecasted',prec="integer")
  def_list[[6]] <- ncvar_def("surface_height_restart","meter",list(ensdim),missval = -99,longname =  'Surface Height',prec="single")
  def_list[[7]] <- ncvar_def("snow_ice_restart","meter",list(ensdim, snow_ice_dim),missval = -99,longname = 'Snow (1), White Ice (2), Blue Ice (3)',prec="single")
  def_list[[8]] <- ncvar_def("ice_thickness","meter", list(timedim,ensdim),missval = -99,longname = 'Ice Thickness',prec="single")
  def_list[[9]] <- ncvar_def("lake_depth","meter",list(timedim,ensdim),missval = -99,longname = 'Depth of lake',prec="single")
  def_list[[10]] <- ncvar_def("avg_surf_temp_restart","degC",list(ensdim),missval = -99,longname ='Running Average of Surface Temperature',prec="single")
  def_list[[11]] <- ncvar_def("running_residuals","dimensionless",list(qt_update_days_dim,statedim),fillvalue,longname = "running residual for updating qt",prec="single")
  def_list[[12]] <- ncvar_def("mixing_restart","dimensionless",list(ensdim,mixing_restart_vars_dim),fillvalue,longname = "variables required to restart mixing",prec="single")
  def_list[[13]] <- ncvar_def("depths_restart","meter",list(ensdim,depth_restart_vars_dim),fillvalue,longname = "depths simulated by glm that are required to restart ",prec="single")
  
  index <- 13
  

  for(i in 1:length(obs_config$state_names_obs)){
      def_list[[index+i]] <-ncvar_def(paste0(obs_config$state_names_obs[i],"_observed"),obs_config$obs_units[i],list(timedim, depthdim),fillvalue,paste0("observed: ", obs_config$state_names_obs[i]),prec="single")
  }
  
  index <- index + length(obs_config$state_names_obs)
  
  if(npars > 0){
    for(par in 1:npars){
      def_list[[index+par]] <-ncvar_def(pars_config$par_names_save[par],pars_config$par_units[par],list(timedim,ensdim),fillvalue,paste0("parameter :",pars_config$par_names_save[par]),prec="single")
    }
  }
  
  
  if(include_wq){
    for(s in 2:length(states_config$state_names)){
      def_list[[index+npars+s-1]]<- ncvar_def(states_config$state_names[s],"mmol m-3",list(timedim,depthdim, ensdim),fillvalue,paste("state :", states_config$state_names[s]),prec="single")
    }
  }
  
  if(length(diagnostics_names) > 0){
    for(s in 1:length(diagnostics_names)){
      def_list[[index+npars+length(states_config$state_names)-1 + s]]<- ncvar_def(diagnostics_names[s],"-",list(timedim,depthdim, ensdim),fillvalue,paste0("diagnostic: ",diagnostics_names[s]),prec="single")
    }
  }
  ncout <- nc_create(ncfname,def_list,force_v4=T)
  
  # create netCDF file and put arrays
  ncvar_put(ncout,def_list[[1]] ,x_efi[,1:length(depths),])
  ncvar_put(ncout,def_list[[2]] ,as.matrix(qt_restart))
  ncvar_put(ncout,def_list[[3]] ,as.matrix(x_restart))
  ncvar_put(ncout,def_list[[4]] ,as.matrix(x_prior))
  ncvar_put(ncout,def_list[[5]] ,as.array(data_assimilation_flag))
  ncvar_put(ncout,def_list[[6]] ,surface_height_restart)
  ncvar_put(ncout,def_list[[7]] ,snow_ice_restart)
  ncvar_put(ncout,def_list[[8]] ,ice_thickness)
  ncvar_put(ncout,def_list[[9]] ,surface_height)
  ncvar_put(ncout,def_list[[10]] ,avg_surf_temp_restart)
  ncvar_put(ncout,def_list[[11]] ,running_residuals)
  ncvar_put(ncout,def_list[[12]] ,mixing_restart)
  ncvar_put(ncout,def_list[[13]] ,glm_depths_restart)
  
  index <- 13
  
  for(i in 1:length(obs_config$state_names_obs)){
    ncvar_put(ncout,def_list[[index + i]] ,obs[,,i])
  }
  
  index <- index + length(obs_config$state_names_obs)
  
  if(npars > 0){
    for(par in 1:npars){
      ncvar_put(ncout,def_list[[index + par]] ,x[,,nstates + par])
    }
  }
  
  if(include_wq){
    for(s in 2:length(states_config$state_names)){
      ncvar_put(ncout,def_list[[index+npars+s-1]],x_efi[,wq_start[s-1]:wq_end[s-1], ])
    }
    
  }
  
  if(length(diagnostics_names) > 0){
    for(s in 1:length(diagnostics_names)){
      ncvar_put(ncout, def_list[[index+npars+length(states_config$state_names) - 1 + s]],diagnostics_efi[, , ,s])
    }
  }
  
  time_of_forecast <- with_tz(time_of_forecast, tzone = "UTC")
  
  #Global file metadata
  ncatt_put(ncout,0,"title",forecast_title, prec =  "text")
  ncatt_put(ncout,0,"forecast_iteration_id",forecast_iteration_id, prec =  "text")
  ncatt_put(ncout,0,"forecast_project_id",forecast_project_id, prec =  "text")
  ncatt_put(ncout,0,"local_time_zone_of_simulation",as.character(local_tzone), prec =  "text")
  ncatt_put(ncout,0,"forecast_issue_time",paste0(as.character(time_of_forecast),"Z"), prec =  "text") 
  
  nc_close(ncout)
  
}

