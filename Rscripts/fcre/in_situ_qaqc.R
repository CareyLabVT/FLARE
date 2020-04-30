in_situ_qaqc <- function(insitu_obs_fname, 
                         data_location, 
                         maintenance_file,
                         ctd_fname, 
                         nutrients_fname,
                         cleaned_observations_file_long,
                         lake_name,
                         code_folder){
  
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/extract_CTD.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/extract_nutrients.R"))
  source(paste0(code_folder,"/","Rscripts/",lake_name,"/temp_oxy_chla_qaqc.R"))
  
  d <- temp_oxy_chla_qaqc(realtime_file = insitu_obs_fname[1],
                          qaqc_file = insitu_obs_fname[2],
                          maintenance_file = paste0(data_location, '/mia-data/CAT_MaintenanceLog.txt'), 
                          input_file_tz = "EST")
  
  
  if(!is.na(ctd_fname)){
    d_ctd <- extract_CTD(fname = ctd_fname,
                         input_file_tz = "EST",
                         local_tzone)
    d <- rbind(d,d_ctd)
  }
  
  if(!is.na(nutrients_fname)){
    d_nutrients <- extract_nutrients(fname = nutrients_fname,
                                     input_file_tz = "EST", 
                                     local_tzone)
    d <- rbind(d,d_nutrients)
  }
  
  write_csv(d, cleaned_observations_file_long)
}