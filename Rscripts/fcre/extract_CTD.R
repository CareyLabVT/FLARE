extract_CTD <- function(fname,
                        input_file_tz,
                        local_tzone){
  
  d <- read_csv(fname, guess_max = 1000000) %>% 
    mutate(Date = force_tz(Date, tzone = input_file_tz),
           Date = with_tz(Date, tzone = local_tzone)) %>% 
    filter(Reservoir == "FCR" & Site == "50") %>% 
    select(Date, Depth_m, Temp_C, DO_mgL, Chla_ugL) %>% 
    rename("timestamp" = Date,
           "depth" = Depth_m,
           "temperature" = Temp_C,
           "oxygen" = DO_mgL,
           "chla" = Chla_ugL) %>% 
    mutate(oxygen = oxygen*1000/32,
           chla = ctd_2_exo_chla[1] + ctd_2_exo_chla[2] * chla) %>% 
    pivot_longer(cols = c("temperature","oxygen","chla"), names_to = "variable", values_to = "value") %>% 
    mutate(method = "ctd") %>% 
    select(timestamp , depth, value, variable, method) %>% 
    mutate(timestamp = as_datetime(timestamp, tz = local_tzone))
  return(d)
}
