archive_forecast <- function(working_glm,
                             folder,
                             forecast_base_name,
                             forecast_location,
                             push_to_git,
                             save_file_name,
                             time_of_forecast){
  
  ###ARCHIVE AND CLEAN UP FORECAST
  unlink(paste0(working_glm, "/", "FCRmet.csv"), recursive = FALSE)
  unlink(paste0(working_glm, "/", "Catwalk.csv"), recursive = FALSE)
  unlink(paste0(working_glm, "/", forecast_base_name, ".csv"), recursive = FALSE)
  time_of_forecast <- paste0(year(time_of_forecast),
                             month(time_of_forecast),
                             day(time_of_forecast), "_",
                             hour(time_of_forecast), "_",
                             (minute(time_of_forecast)))
  forecast_archive_dir_name <- paste0(save_file_name, "_", time_of_forecast)



  if(is.na(forecast_location)){
    forecast_archive_dir <- paste0(folder, "/", "Forecasts", "/" ,forecast_archive_dir_name)
  }else{
    forecast_archive_dir <- paste0(forecast_location, "/", forecast_archive_dir_name)
  }
  
  file.rename(from = paste0(working_glm, "/", save_file_name, ".nc"), 
              to = paste0(working_glm, "/", forecast_archive_dir_name, ".nc"))
  
  #dir.create(forecast_archive_dir)
  files <- list.files(paste0(working_glm), full.names = TRUE)
  files_pdf <- list.files(paste0(working_glm), pattern = ".pdf", full.names = TRUE)
  files_nc <- list.files(paste0(working_glm),
                         pattern = paste0(forecast_archive_dir_name, ".nc"), 
                         full.names = TRUE)
  tmp <- file.copy(from = files_pdf, to = forecast_location)
  tmp <- file.copy(from = files_nc, to = forecast_location)
  zip(zipfile = forecast_archive_dir, files = files)
  netcdf_name <- paste0(forecast_archive_dir_name, ".nc")
  if(push_to_git){
    setwd(forecast_location)
    system(paste0('git add ',netcdf_name))
    system('git commit -m "forecast" ')
    system("git push")
  }
  return(list(paste0(forecast_location, "/", netcdf_name)))
}