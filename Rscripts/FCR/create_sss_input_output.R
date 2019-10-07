create_sss_input_output <- function(x, i, m, full_time_day_local, working_directory, wq_start, management_input, hist_days, forecast_sss_on){
  sss_depth <- 8
  depth_index <- which.min(abs(modeled_depths - sss_depth))
  
  time_sss <- c(full_time_day_local[i - 1],full_time_day_local[i])
  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      FLOW1 <- forecast_SSS_flow * (1/(60*60*24))
      OXY1 <- x[i-1, m, wq_start[1] + depth_index - 1] + forecast_SSS_Oxy
    }else{
      FLOW1 <- 0.0
      OXY1 <- x[i-1, m, wq_start[1] + depth_index - 1] + 0.0
    }
  }else{
    FLOW1 <- management_input[i-1, 1]
    #Add input oxygen to the existing concentration
    OXY1 <- x[i-1, m, wq_start[1] + depth_index - 1] + management_input[i-1, 2]
    #Add use the input oxygen as the concentration
    #OXY1 <- management_input[i-1, 2]  
  }
  
  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      FLOW2 <- forecast_SSS_flow * (1/(60*60*24))
      OXY2 <- x[i-1, m, wq_start[1] + depth_index - 1] + forecast_SSS_Oxy
    }else{
      FLOW2 <- 0.0
      OXY2 <- x[i-1, m, wq_start[1] + depth_index - 1]
    }
  }else{
    FLOW2 <- management_input[i, 1]
    #Add input oxygen to the existing concentration
    OXY2 <- x[i-1, m, wq_start[1] + depth_index - 1] + management_input[i, 2]
    #Add use the input oxygen as the concentration
    #OXY2 <- management_input[i, 2]       
  }
  
  FLOW <- round(c(FLOW1, FLOW2), 5)
  TEMP <- round(rep(x[i-1, m, depth_index],2), 3)
  SALT <- rep(0,2)
  
  OXY_oxy <- round(c(OXY1, OXY2), 3)
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
  
  sss_outflow <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT)
  
  write.csv(sss_inflow, paste0(working_directory, "/sss_inflow.csv"), row.names = FALSE, quote = FALSE)
  write.csv(sss_outflow, paste0(working_directory, "/sss_outflow.csv"), row.names = FALSE, quote = FALSE)
}
