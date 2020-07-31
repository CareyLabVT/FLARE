plotting_general <- function(pdf_file_name,
                             output_file,
                             save_location){
  
  #lake_name <- "fcre"
  #code_folder <- "/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE/"
  #data_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/"
  #manual_data_location <- paste0(data_location, "/manual-data") 
  #realtime_insitu_location <- paste0(data_location,"/mia-data")
  #insitu_obs_fname <- c(paste0(realtime_insitu_location,"/Catwalk.csv"),
  #                       paste0(manual_data_location,"/Catwalk_cleanedEDI.csv"))
  #maintenance_file <- maintenance_file
  #nutrients_fname <- paste0(manual_data_location,"/chemistry.csv")
  #ctd_fname <- NA #paste0(manual_data_location,"/CTD_final_2013_2019.csv")
  #local_tzone <- "EST"
  #do_methods <<- c("do_sensor", "exo_sensor")
  #chla_methods <<- c("exo_sensor", "ctd")
  #temp_methods <<- c("thermistor", "do_sensor", "exo_sensor")
  #fdom_methods <<- c("exo_sensor")
  #nh4_methods <<- c("grab_sample")
  #no3_methods <<- c("grab_sample")
  #srp_methods <<- c("grab_sample")
  
  #time_threshold_seconds_temp <<- 60*30
  #time_threshold_seconds_oxygen <<- 60*60*12
  #time_threshold_seconds_chla <<- 60*60*12
  #time_threshold_seconds_fdom <<- 60*60*12
  #time_threshold_seconds_nh4 <<- 60*60*12
  #time_threshold_seconds_no3 <<- 60*60*12
  #time_threshold_seconds_srp <<- 60*60*12
  #distance_threshold_meter <<- 0.15
  
  source(paste0(code_folder,"/","Rscripts/extract_observations.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/in_situ_qaqc.R"))  
  
  setwd(realtime_insitu_location)
  if(pull_from_git){
    system(paste0("git pull"))
  }
  
  ###### EVERY THING ABOVE HERE SHOULD BE DEALT WITH IN THE QAQC CONTAINER###
  
  ### EVERYTHING AFTER IS FOR THE PDF GENERATION ####
  
  par_names_potential <- c("zone1temp",
                           "zone2temp"
                           ,"sw_factor"
                           ,"lw_factor"
                           ,"Fsed_oxy_zone1"
                           ,"Fsed_oxy_zone2"
                           ,"Rdom_minerl"
                           ,"Rdomr_minerl"
                           ,"Fsed_frp"
                           ,"Fsed_amm"
                           ,"Fsed_nit"
                           ,"R_resp"
                           ,"K_P"
                           ,"K_N"
                           ,"X_ncon"
                           ,"X_pcon"
                           ,"Fsed_frp1"
                           ,"Fsed_frp2",
                           "I_K")
  
  wq_names_potential <- c("temp",
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
                          "NCS_ss1",
                          "PHS_frp_ads",
                          "PHY_cyano",
                          "PHY_cyano_IN",
                          "PHY_cyano_IP",
                          "PHY_green",
                          "PHY_green_IN",
                          "PHY_green_IP",
                          "PHY_diatom",
                          "PHY_diatom_IN",
                          "PHY_diatom_IP")
  
  combined_states_potential <- list(NIT_total = c("NIT_amm","NIT_nit","OGM_don","OGM_pon"),
                          #PHS_total = c("PHS_frp","OGM_dop","OGM_dopr","OGM_pop","PHS_frp_ads"),
                          PHS_total = c("PHS_frp","OGM_dop","OGM_pop"),
                          OGM_doc_total = c("OGM_doc","OGM_docr"),
                          PHY_TCHLA = c("PHY_cyano","PHY_green","PHY_diatom"))
  
  diagnostics_potential <- c("extc_coef",
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
  
  #biomass_to_chla <- c((80/12),(30/12), (30/12))
  combined_states_conversion_potential <- list(NIT_total = c(1,1,1,1,1),
                                     PHS_total = c(1,1,1,1,1),
                                     OGM_doc_total = c(1,1),
                                     PHY_TCHLA = c(1/biomass_to_chla[1], 1/biomass_to_chla[2], 1/biomass_to_chla[3]))
  
  
  nc <- nc_open(output_file)
  t <- ncvar_get(nc,'time')
  local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
  full_time_local <- as.POSIXct(t, 
                                origin = '1970-01-01 00:00.00 UTC', 
                                tz = local_tzone)
  full_time_day_local <- as_date(full_time_local)
  nsteps <- length(full_time_day_local)
  forecasted <- ncvar_get(nc, 'forecasted')
  depths <- round(ncvar_get(nc, 'z'),2)
  
  wq_names <- wq_names_potential[wq_names_potential %in% names(nc$var)]
  combined_states_conversion <- combined_states_conversion_potential[names(combined_states_conversion_potential) %in% wq_names_obs]
  combined_states <- combined_states_potential[names(combined_states_potential) %in% wq_names_obs]
  
  state_names <- c(wq_names, names(combined_states))
  par_names <- par_names_potential[par_names_potential %in% names(nc$var)]
  
  diagnostics_names <- diagnostics_potential[diagnostics_potential %in% names(nc$var)]
  
  
  if(length(which(forecasted == 1)) > 0){
    forecast_index <- which(forecasted == 1)[1]
  }else{
    forecast_index <- 0
  }
  
  
  par_list <- list()
  if(length(par_names) > 0){
    for(par in 1:length(par_names)){
      par_list[[par]] <- ncvar_get(nc, par_names[par])
    }
  }
  
  state_list <- list()
  for(s in 1:length(wq_names)){
    state_list[[s]] <- ncvar_get(nc, wq_names[s])
  }
  
  if(length(combined_states) > 0){
  for(i in 1:length(combined_states)){
    for(s in 1:length(combined_states[[i]])){
      if(s > 1){
        tmp_list <- tmp_list + ncvar_get(nc, combined_states[[i]][s]) * combined_states_conversion[[i]][s]
      }else{
        tmp_list <- ncvar_get(nc, combined_states[[i]][s]) * combined_states_conversion[[i]][s]
      }
    }
    state_list[[length(wq_names)+i]] <- tmp_list
  }
  }
  
  names(state_list) <- state_names
  
  diagnostic_list <- list()
  for(s in 1:length(diagnostics_names)){
    diagnostic_list[[s]] <- ncvar_get(nc, diagnostics_names[s])
  }
  
  names(diagnostic_list) <- diagnostics_names
  
  
  
  #PROCESS TEMPERATURE OBSERVATIONS
  
  cleaned_observations_file_long <- paste0(save_location, 
                                           "/observations_postQAQC_long.csv")
  
  in_situ_qaqc(insitu_obs_fname = insitu_obs_fname, 
               data_location = data_location, 
               maintenance_file = maintenance_file,
               ctd_fname = ctd_fname, 
               nutrients_fname = nutrients_fname,
               cleaned_observations_file_long = cleaned_observations_file_long,
               lake_name,
               code_folder)
  
  #####
  
  print("Extracting temperature observations")
  obs_temp <- extract_observations(fname = cleaned_observations_file_long,
                                   full_time_local,
                                   modeled_depths = modeled_depths,
                                   local_tzone,
                                   target_variable = "temperature",
                                   time_threshold_seconds = time_threshold_seconds_temp,
                                   distance_threshold_meter = distance_threshold_meter,
                                   methods = temp_methods)
  
  if(include_wq){
    
    print("Extracting DO observations")
    obs_do <- extract_observations(fname = cleaned_observations_file_long,
                                   full_time_local,
                                   modeled_depths = modeled_depths,
                                   local_tzone,
                                   target_variable = "oxygen",
                                   time_threshold_seconds = time_threshold_seconds_oxygen,
                                   distance_threshold_meter = distance_threshold_meter,
                                   methods = do_methods)
    
    print("Extracting Chl-a observations")
    obs_chla <- extract_observations(fname = cleaned_observations_file_long,
                                     full_time_local,
                                     modeled_depths = modeled_depths,
                                     local_tzone,
                                     target_variable = "chla",
                                     time_threshold_seconds = time_threshold_seconds_chla,
                                     distance_threshold_meter = distance_threshold_meter,
                                     methods = chla_methods)
    
    print("Extracting fdom observations")
    obs_fdom <- extract_observations(fname = cleaned_observations_file_long,
                                     full_time_local,
                                     modeled_depths = modeled_depths,
                                     local_tzone,
                                     target_variable = "fdom",
                                     time_threshold_seconds = time_threshold_seconds_fdom,
                                     distance_threshold_meter = distance_threshold_meter,
                                     methods = fdom_methods)
    
    print("Extracting NH4 observations")
    obs_NH4 <- extract_observations(fname = cleaned_observations_file_long,
                                    full_time_local,
                                    modeled_depths = modeled_depths,
                                    local_tzone,
                                    target_variable = "NH4",
                                    time_threshold_seconds = time_threshold_seconds_nh4,
                                    distance_threshold_meter = distance_threshold_meter,
                                    methods = nh4_methods)
    
    print("Extracting NO3 observations")
    obs_NO3 <- extract_observations(fname = cleaned_observations_file_long,
                                    full_time_local,
                                    modeled_depths = modeled_depths,
                                    local_tzone,
                                    target_variable = "NO3NO2",
                                    time_threshold_seconds = time_threshold_seconds_no3,
                                    distance_threshold_meter = distance_threshold_meter,
                                    methods = no3_methods)
    
    print("Extracting SRP observations")
    obs_SRP <- extract_observations(fname = cleaned_observations_file_long,
                                    full_time_local,
                                    modeled_depths = modeled_depths,
                                    local_tzone,
                                    target_variable = "SRP",
                                    time_threshold_seconds = time_threshold_seconds_srp,
                                    distance_threshold_meter = distance_threshold_meter,
                                    methods = srp_methods)
    
    print("Extracting TP observations")
    obs_TP <- extract_observations(fname = cleaned_observations_file_long,
                                   full_time_local,
                                   modeled_depths = modeled_depths,
                                   local_tzone,
                                   target_variable = "TP",
                                   time_threshold_seconds = time_threshold_seconds_srp,
                                   distance_threshold_meter = distance_threshold_meter,
                                   methods = srp_methods)
    
    print("Extracting TN observations")
    obs_TN <- extract_observations(fname = cleaned_observations_file_long,
                                   full_time_local,
                                   modeled_depths = modeled_depths,
                                   local_tzone,
                                   target_variable = "TN",
                                   time_threshold_seconds = time_threshold_seconds_no3,
                                   distance_threshold_meter = distance_threshold_meter,
                                   methods = srp_methods)
    
    print("Extracting DIC observations")
    obs_DIC <- extract_observations(fname = cleaned_observations_file_long,
                                    full_time_local,
                                    modeled_depths = modeled_depths,
                                    local_tzone,
                                    target_variable = "DIC",
                                    time_threshold_seconds = time_threshold_seconds_no3,
                                    distance_threshold_meter = distance_threshold_meter,
                                    methods = srp_methods)
  }
  
  obs_dims <- dim(obs_temp)
  
  CAR_pH_obs <- array(NA, dim = obs_dims)
  
  CAR_ch4_obs <- array(NA, dim = obs_dims)
  SIL_rsi_obs <- array(NA, dim = obs_dims)
  OGM_poc_obs <- array(NA, dim = obs_dims)
  OGM_don_obs <- array(NA, dim = obs_dims)
  OGM_donr_obs <- array(NA, dim = obs_dims)
  OGM_pon_obs <- array(NA, dim = obs_dims)
  OGM_dop_obs <- array(NA, dim = obs_dims)
  OGM_dopr_obs <- array(NA, dim = obs_dims)
  OGM_pop_obs <- array(NA, dim = obs_dims)
  NCS_ss1_obs <- array(NA, dim = obs_dims)
  PHS_frp_ads_obs <- array(NA, dim = obs_dims)
  
  if(include_wq){
    OXY_oxy_obs <- obs_do
    CAR_dic_obs <- obs_DIC
    NIT_amm_obs <- obs_NH4
    NIT_nit_obs <- obs_NO3
    PHS_frp_obs <- obs_SRP
    OGM_doc_obs <- obs_fdom * (1 - docr_to_total_doc)
    OGM_docr_obs <- obs_fdom * docr_to_total_doc
    OGM_doc_obs_total <- obs_fdom
    PHY_TCHLA_obs <- obs_chla
    NIT_total_obs <- obs_TN
    PHS_total_obs <- obs_TP
  }else{
    OXY_oxy_obs <-array(NA, dim = obs_dims)
    CAR_dic_obs <-array(NA, dim = obs_dims)
    NIT_amm_obs <-array(NA, dim = obs_dims)
    NIT_nit_obs <-array(NA, dim = obs_dims)
    PHS_frp_obs <-array(NA, dim = obs_dims)
    OGM_doc_obs <- array(NA, dim = obs_dims)
    OGM_docr_obs <- array(NA, dim = obs_dims)
    PHY_TCHLA_obs <- array(NA, dim = obs_dims)
    NIT_total_obs <- array(NA, dim = obs_dims)
    PHS_total_obs <- array(NA, dim = obs_dims)
    OGM_doc_obs_total <- array(NA, dim = obs_dims)
  }
  
  z_potential <- list(temp = obs_temp,
                      OXY_oxy = OXY_oxy_obs,
                      CAR_dic = CAR_dic_obs,
                      CAR_ch4 = CAR_ch4_obs,
                      SIL_rsi = SIL_rsi_obs,
                      NIT_amm = NIT_amm_obs,
                      NIT_nit= NIT_nit_obs,
                      NIT_total = NIT_total_obs,
                      PHS_frp= PHS_frp_obs,
                      PHS_total = PHS_total_obs,
                      OGM_doc = OGM_doc_obs,
                      OGM_docr = OGM_docr_obs,
                      OGM_doc_total = OGM_doc_obs_total,
                      OGM_poc = OGM_poc_obs,
                      OGM_don = OGM_don_obs,
                      OGM_donr = OGM_donr_obs,
                      OGM_pon = OGM_pon_obs,
                      OGM_dop = OGM_dop_obs,
                      OGM_dopr = OGM_dopr_obs,
                      OGM_pop = OGM_pop_obs,
                      NCS_ss1 = NCS_ss1_obs,
                      PHS_frp_ads = PHS_frp_ads_obs,
                      PHY_TCHLA = PHY_TCHLA_obs)
  
  z <- array(unlist(z_potential[names(z_potential) %in% wq_names_obs]), dim = c(nsteps, length(depths), length(wq_names_obs)))
  
  if(length(focal_depths_plotting) < 4){
    plot_height <- 3
  }else{
    plot_height <- 8
  }
  pdf(paste0(save_location,'/',pdf_file_name, ".pdf"),width = 11, height = plot_height)
  
  for(i in 1:length(state_names)){
    
    curr_var <- state_list[[i]]
    
    
    mean_var <- array(NA, dim = c(length(depths), length(full_time_local)))
    upper_var <- array(NA, dim = c(length(depths), length(full_time_local)))
    lower_var <- array(NA,dim = c(length(depths), length(full_time_local)))
    for(j in 1:length(full_time_local)){
      for(ii in 1:length(depths)){
        mean_var[ii, j] <- mean(curr_var[j, , ii], na.rm = TRUE)
        upper_var[ii, j] <- quantile(curr_var[j, , ii], 0.1, na.rm = TRUE)
        lower_var[ii, j] <- quantile(curr_var[j, , ii], 0.9, na.rm = TRUE)
      }
    }
    
    date <- c()
    for(j in 1:length(full_time_local)){
      date <- c(date, rep(full_time_local[j], length(depths)))
    }
    
    if(state_names[i] %in% wq_names_obs){
      obs_index <- which(wq_names_obs == state_names[i])
      obs <- c(t(z[, ,obs_index]))
    }else{
      obs <- as.numeric(rep(NA, length(date)))
    }
    
    curr_tibble <- tibble(date = as_datetime(date),
                          curr_var = c(mean_var),
                          upper_var = c(upper_var),
                          lower_var = c(lower_var),
                          observed = obs,
                          depth = rep(depths, length(full_time_local))) %>% 
      filter(depth %in% focal_depths_plotting)
    
    if(forecast_index > 0){
      forecast_start_day <- full_time_local[forecast_index-1]
      forecast_start_day_alpha <- 1.0
    }else{
      forecast_start_day <- last(full_time_local)
      forecast_start_day_alpha <- 0.0
    }
    
    p <- ggplot(curr_tibble, aes(x = date)) + 
      facet_wrap(~depth, scales = "free") +
      geom_ribbon(aes(ymin = lower_var, ymax = upper_var), 
                  alpha = 0.70, 
                  fill = "gray") +
      geom_line(aes(y = curr_var), size = 0.5) +
      geom_vline(xintercept = forecast_start_day, 
                 alpha = forecast_start_day_alpha) +
      geom_point(aes(y = observed), size = 0.5, color = "red") +
      theme_light() +
      labs(x = "Date", y = state_names[i], title = state_names[i]) + 
      theme(axis.text.x = element_text(angle = 90, size = 10))
    print(p)
  }
  
  if(length(par_names) > 0){
    plist <- list()
    
    for(i in 1:length(par_names)){
      curr_var <- ncvar_get(nc, par_names[i])
      
      mean_var <- array(NA, dim = c(length(full_time_local)))
      upper_var <- array(NA, dim = c(length(full_time_local)))
      lower_var <- array(NA, dim = c(length(full_time_local)))
      for(j in 1:length(full_time_local)){
        mean_var[j] <- mean(curr_var[j, ])
        upper_var[j] <- quantile(curr_var[j, ], 0.1, na.rm = TRUE)
        lower_var[j] <- quantile(curr_var[j, ], 0.9, na.rm = TRUE)
      }
      
      date <- full_time_local
      
      if(forecast_index > 0){
        forecast_start_day <- full_time_local[forecast_index-1]
        forecast_start_day_alpha <- 1.0
      }else{
        forecast_start_day <- last(full_time_local)
        forecast_start_day_alpha <- 0.0
      }
      
      curr_tibble <- tibble(date = as_datetime(date),
                            curr_var = c(mean_var),
                            upper_var = c(upper_var),
                            lower_var = c(lower_var))
      
      plist[[i]] <- ggplot(curr_tibble, aes(x = date)) + 
        geom_ribbon(aes(ymin = lower_var, ymax = upper_var),
                    alpha = 0.70, 
                    fill = "gray") +
        geom_line(aes(y = curr_var), size = 0.5) +
        geom_vline(xintercept = forecast_start_day, 
                   alpha = forecast_start_day_alpha) +
        theme_bw() +
        labs(x = "Date", y = par_names[i]) + 
        theme(axis.text.x = element_text(angle = 90, size = 10))
    }
    
    print(wrap_plots(plist))
  }
  
  if(length(diagnostics_names) > 0 )
    for(i in 1:length(diagnostics_names)){
      
      curr_var <- diagnostic_list[[i]]
      
      
      mean_var <- array(NA, dim = c(length(depths), length(full_time_local)))
      upper_var <- array(NA, dim = c(length(depths), length(full_time_local)))
      lower_var <- array(NA,dim = c(length(depths), length(full_time_local)))
      for(j in 1:length(full_time_local)){
        for(ii in 1:length(depths)){
          mean_var[ii, j] <- mean(curr_var[j, , ii], na.rm = TRUE)
          upper_var[ii, j] <- quantile(curr_var[j, , ii], 0.1, na.rm = TRUE)
          lower_var[ii, j] <- quantile(curr_var[j, , ii], 0.9, na.rm = TRUE)
        }
      }
      
      date <- c()
      for(j in 1:length(full_time_local)){
        date <- c(date, rep(full_time_local[j], length(depths)))
      }
      
      curr_tibble <- tibble(date = as_datetime(date),
                            curr_var = c(mean_var),
                            upper_var = c(upper_var),
                            lower_var = c(lower_var),
                            depth = rep(depths, length(full_time_local))) %>% 
      filter(depth %in% focal_depths_plotting)
      
      if(forecast_index > 0){
        forecast_start_day <- full_time_local[forecast_index-1]
        forecast_start_day_alpha <- 1.0
      }else{
        forecast_start_day <- last(full_time_local)
        forecast_start_day_alpha <- 0.0
      }
      
      p <- ggplot(curr_tibble, aes(x = date)) + 
        facet_wrap(~depth) +
        geom_ribbon(aes(ymin = lower_var, ymax = upper_var), 
                    alpha = 0.70, 
                    fill = "gray") +
        geom_line(aes(y = curr_var), size = 0.5) +
        geom_vline(xintercept = forecast_start_day, 
                   alpha = forecast_start_day_alpha) +
        theme_light() +
        labs(x = "Date", y = diagnostics_names[i], title = diagnostics_names[i]) + 
        theme(axis.text.x = element_text(angle = 90, size = 10))
      print(p)
    }
  
  if("extc_coef" %in% diagnostics_names){
    
    if(!is.na(secchi_file)){
      obs <- read_csv(secchi_file) %>% 
        filter(Reservoir == "FCR" & Site == 50) %>% 
        select(DateTime, Secchi_m) %>% 
        mutate(DateTime = mdy_hm(DateTime)) %>% 
        group_by(DateTime) %>% 
          summarise(Secchi_m = mean(Secchi_m, na.rm = TRUE)) %>% 
        mutate(date = as_date(DateTime))
      
      
      full_time_local_obs <- tibble(date = as_date(full_time_local))
      obs <- obs %>% 
        right_join(full_time_local_obs, by = "date") %>% 
        select(Secchi_m) 
    }
    
    
    
    i <- which(diagnostics_names == "extc_coef")
    ii <- which.min(abs(depths-1.0))
    curr_var <- diagnostic_list[[i]]
    
    
    mean_var <- array(NA, dim = c(length(full_time_local)))
    upper_var <- array(NA, dim = c(length(full_time_local)))
    lower_var <- array(NA,dim = c(length(full_time_local)))
    for(j in 1:length(full_time_local)){
      sechi <- 1.7 / curr_var[j, , ii]
      mean_var[j] <- mean(sechi, na.rm = TRUE)
      upper_var[j] <- quantile(sechi, 0.1, na.rm = TRUE)
      lower_var[j] <- quantile(sechi, 0.9, na.rm = TRUE)
    }
    
    
    curr_tibble <- tibble(date = as_datetime(full_time_local),
                          curr_var = c(mean_var),
                          upper_var = c(upper_var),
                          lower_var = c(lower_var),
                          observed = unlist(obs))
    
    if(forecast_index > 0){
      forecast_start_day <- full_time_local[forecast_index-1]
      forecast_start_day_alpha <- 1.0
    }else{
      forecast_start_day <- last(full_time_local)
      forecast_start_day_alpha <- 0.0
    }
    
    p <- ggplot(curr_tibble, aes(x = date)) + 
      geom_ribbon(aes(ymin = lower_var, ymax = upper_var), 
                  alpha = 0.70, 
                  fill = "gray") +
      geom_line(aes(y = curr_var), size = 0.5) +
      scale_y_reverse() +
      geom_vline(xintercept = forecast_start_day, 
                 alpha = forecast_start_day_alpha) +
      geom_point(aes(y = observed), size = 1, color = "red") +
      theme_light() +
      labs(x = "Date", y = "Sechi depth (m)", title = "Sechi depth") + 
      theme(axis.text.x = element_text(angle = 90, size = 10))
    print(p)
  }
  
  dev.off()
}

