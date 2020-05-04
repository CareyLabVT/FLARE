create_sss_input_output <- function(x, i, m, full_time_local, 
                                    working_directory, wq_start, 
                                    management_input, hist_days, 
                                    forecast_sss_on,
                                    sss_depth,
                                    use_specified_sss){
  
  full_time_day_local <- as_date(full_time_local)
  
  sss_oxy_factor <- 1.0
  
  depth_index <- which.min(abs(modeled_depths - sss_depth))
  
  time_sss <- c(full_time_day_local[i - 1],full_time_day_local[i])
  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      if(use_specified_sss){
        FLOW1 <- management_input[i-1, 1]
        OXY1 <- management_input[i-1, 2]  * sss_oxy_factor
      }else{
      FLOW1 <- forecast_SSS_flow * (1/(60*60*24))
      OXY1 <- forecast_SSS_Oxy * sss_oxy_factor
      }
    }else{
      FLOW1 <- 0.0
      OXY1 <-  0.0
    }
  }else{
    FLOW1 <- management_input[i-1, 1]
    OXY1 <-  management_input[i-1, 2]  * sss_oxy_factor
  }
  
  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      if(use_specified_sss){
        FLOW1 <- management_input[i, 1]
        OXY1 <- management_input[i, 2]  * sss_oxy_factor
      }else{
        FLOW1 <- forecast_SSS_flow * (1/(60*60*24))
        OXY1 <- forecast_SSS_Oxy * sss_oxy_factor
      }
    }else{
      FLOW2 <- 0.0
      OXY2 <- 0.0
    }
  }else{
    FLOW2 <- management_input[i, 1]
    OXY2 <- management_input[i, 2]  * sss_oxy_factor    
  }
  
  FLOW <- round(c(FLOW1, FLOW2), 5)
  TEMP <- round(rep(x[i-1, m, depth_index],2), 3)
  SALT <- rep(0,2)
  
  #OXY_EQ <- Eq.Ox.conc(TEMP[1], elevation.m = 506,
  #           bar.press = NULL, bar.units = NULL,
  #           out.DO.meas = "mg/L",
  #           salinity = 0, salinity.units = "pp.thou")*1000*(1/32)
  
  #if(OXY1 > OXY_EQ){OXY1 = OXY_EQ}
  #if(OXY2 > OXY_EQ){OXY2 = OXY_EQ}
  
  OXY_oxy <- round(c(OXY1, OXY2), 3)
  
  if(length(which(wq_names != "OXY_oxy")) == 0){
  sss_inflow <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT, OXY_oxy = OXY_oxy)
  }else{
    NIT_amm <-  round(rep(x[i-1, m, wq_start[6] + depth_index - 1],2), 3)
    NIT_nit <-  round(rep(x[i-1, m, wq_start[7] + depth_index - 1],2), 3)
    PHS_frp <-  round(rep(x[i-1, m, wq_start[8] + depth_index - 1],2), 3)
    OGM_doc <-  round(rep(x[i-1, m, wq_start[9] + depth_index - 1],2), 3)
    OGM_poc <-  round(rep(x[i-1, m, wq_start[10] + depth_index - 1],2), 3)
    OGM_don <-  round(rep(x[i-1, m, wq_start[11] + depth_index - 1],2), 3)
    OGM_dop <-  round(rep(x[i-1, m, wq_start[13] + depth_index - 1],2), 3)
    OGM_pop <-  round(rep(x[i-1, m, wq_start[14] + depth_index - 1],2), 3)
    OGM_pon <-  round(rep(x[i-1, m, wq_start[12] + depth_index - 1],2), 3)
    PHS_frp_ads <-  round(rep(x[i-1, m, wq_start[16] + depth_index - 1],2), 3)
    sss_inflow <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT, OXY_oxy,
                             NIT_amm = NIT_amm, NIT_nit = NIT_nit, PHS_frp = PHS_frp,
                             OGM_doc = OGM_doc, OGM_poc = OGM_poc, OGM_don = OGM_don,
                             OGM_dop = OGM_dop, OGM_pop = OGM_pop, OGM_pon = OGM_pon,
                             PHS_frp_ads = PHS_frp_ads)
  }
  

  
  sss_outflow <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT)
  
  write.csv(sss_inflow, paste0(working_directory, "/sss_inflow.csv"), row.names = FALSE, quote = FALSE)
  write.csv(sss_outflow, paste0(working_directory, "/sss_outflow.csv"), row.names = FALSE, quote = FALSE)
}
