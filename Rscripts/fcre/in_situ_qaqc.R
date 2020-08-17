in_situ_qaqc <- function(insitu_obs_fname, 
                         data_location, 
                         maintenance_file,
                         ctd_fname, 
                         nutrients_fname,
                         cleaned_observations_file_long,
                         lake_name_code,
                         code_folder){
  
  source(paste0(code_folder,"/","Rscripts/",lake_name_code,"/extract_CTD.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name_code,"/extract_nutrients.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name_code,"/temp_oxy_chla_qaqc.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name_code,"/extract_ch4.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name_code,"/extract_secchi.R"))
  
  
  d <- temp_oxy_chla_qaqc(realtime_file = insitu_obs_fname[1],
                          qaqc_file = insitu_obs_fname[2],
                          maintenance_file = paste0(data_location, '/mia-data/CAT_MaintenanceLog.txt'), 
                          input_file_tz = "EST",
                          focal_depths)
  
  if(exists("ctd_fname")){
    if(!is.na(ctd_fname)){
      d_ctd <- extract_CTD(fname = ctd_fname,
                           input_file_tz = "EST",
                           local_tzone,
                           focal_depths)
      d <- rbind(d,d_ctd)
    }
  }
  if(exists("nutrients_fname")){
    if(!is.na(nutrients_fname)){
      d_nutrients <- extract_nutrients(fname = nutrients_fname,
                                       input_file_tz = "EST", 
                                       local_tzone,
                                       focal_depths)
      d <- rbind(d,d_nutrients)
    }
  }
  if(exists("ch4_fname")){
    if(!is.na(ch4_fname)){
      d_ch4 <- extract_ch4(fname = ch4_fname,
                           input_file_tz = "EST", 
                           local_tzone,
                           focal_depths)
      d <- rbind(d,d_ch4)
    }
  }
  
  if(!is.na(secchi_fname) & exists("secchi_fname")){
    d_secchi <- extract_secchi(fname = secchi_fname,
                               input_file_tz = "EST", 
                               local_tzone,
                               focal_depths)
    d <- rbind(d,d_secchi)
  }
  
  write_csv(d, cleaned_observations_file_long)
}