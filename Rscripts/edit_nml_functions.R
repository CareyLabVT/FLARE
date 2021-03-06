#FUNCTIONS Shared between the MCMC and EnKF



#Set GLM Initial conditions for temperature at each layer from first observations
#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
update_temps <- function(curr_temps,curr_depths,working_directory){
  orig_nml = read_nml(paste0(working_directory,'/','glm3.nml'))
  index1 = NA; index2 = NA;index3 = NA; index4 = NA
  for (g in 1:length(orig_nml)) {
    for (q in 1:length(orig_nml[[g]])) {
      if (names(orig_nml[[g]][q]) == "the_temps") {
        temps = as.numeric(as.character(unlist(orig_nml[[g]][q])))
        index1 = g; index2 = q; 
      }
      if (names(orig_nml[[g]][q]) == "the_depths") {
        depths = as.numeric(as.character(unlist(orig_nml[[g]][q])))
        index3 = g; index4 = q
      }
    }
  }
  temp_inter = approxfun(curr_depths,curr_temps,rule=2)
  init_temps = temp_inter(depths)
  char_temps = paste(init_temps, collapse = ', ')
  holder2 = unlist(orig_nml[[index1]][index2])
  holder2[1:length(holder2)] = init_temps
  holder2 = list(holder2)
  holder3 = unlist(orig_nml[[index3]][index4])
  holder3[1:length(holder3)] = depths
  holder3 = list(holder3)
  orig_nml[[index1]][index2] = holder2
  orig_nml[[index3]][index4] = holder3
  write_nml(orig_nml, paste0(working_directory,'/','glm3.nml'))
  return(list(depths,init_temps))
}

#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
update_var <- function(var_value,var_name,working_directory, nml){
  orig_nml = read_nml(paste0(working_directory,'/',nml))
  index1 = NA; index2 = NA
  for (g in 1:length(orig_nml)) {
    for (q in 1:length(orig_nml[[g]])) {
      if (names(orig_nml[[g]][q]) == var_name) {
        index1 = g; index2 = q; 
      }
    }
  }
  holder2 = unlist(orig_nml[[index1]][index2])
  holder2[1:length(var_value)] = var_value
  holder2 = list(holder2[1:length(var_value)])
  orig_nml[[index1]][index2] = holder2
  write_nml(orig_nml, paste0(working_directory,'/',nml))
}

update_nml <- function(var_list,var_name_list,working_directory, nml){
  orig_nml = read_nml(paste0(working_directory,'/',nml))
  
  for(k in 1:length(var_list)){
    index1 = NA; index2 = NA
    for (g in 1:length(orig_nml)) {
      for (q in 1:length(orig_nml[[g]])) {
        if (names(orig_nml[[g]][q]) == var_name_list[k]) {
          index1 = g; index2 = q; 
        }
      }
    }
    holder2 = unlist(orig_nml[[index1]][index2])
    holder2[1:length(var_list[[k]])] = var_list[[k]]
    holder2 = list(holder2[1:length(var_list[[k]])])
    orig_nml[[index1]][index2] = holder2
  }
  
  write_nml(orig_nml, paste0(working_directory,'/',nml))
}

#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
update_time <- function(start_value,stop_value,working_directory){
  orig_nml = read_nml(paste0(working_directory,'/','glm3.nml'))
  index1 = NA; index2 = NA; index3 = NA; index4 = NA
  for (g in 1:length(orig_nml)) {
    for (q in 1:length(orig_nml[[g]])) {
      if (names(orig_nml[[g]][q]) == 'start') {
        index1 = g; index2 = q; 
      }
      if (names(orig_nml[[g]][q]) == 'stop') {
        index3 = g; index4 = q; 
      }
    }
  }
  orig_nml[[index1]][index2] = as.character(start_value)
  orig_nml[[index1]][index2] = as.character(start_value)
  orig_nml[[index3]][index4] = stop_value
  write_nml(orig_nml, paste0(working_directory,'/','glm3.nml'))
}

#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
get_glm_nc_var <- function(ncFile,working_dir, z_out,var = 'temp'){
  glm_nc <- nc_open(paste0(working_dir,ncFile))
  tallest_layer <- ncvar_get(glm_nc, "NS")
  elev <- ncvar_get(glm_nc, "z")
  temp <- ncvar_get(glm_nc, var)
  nc_close(glm_nc)
  
  elev_surf = get_surface_height(ncFile)
  
  max_i <- tallest_layer[length(tallest_layer)]
  
  elev <- elev[1:max_i, length(tallest_layer)]
  temp <- temp[1:max_i, length(tallest_layer)]
  
  num_step <- length(tallest_layer)
  num_dep <- length(z_out)
  temp_out <- rep(NA,num_dep)
  tme = num_step
  elevs_out <- elev_surf[tme, 2] - z_out
  
  elevs = elev
  temps = temp
  
  num_z <- max_i
  layer_mids <- c(elevs[1]/2, elevs[1:num_z-1] + diff(elevs)/2)
  temps_re <- c(temps[1], temps, tail(temps,1))
  elevs_re <- c(0, layer_mids, tail(elevs, 1))
  temps <- approx(x = elevs_re, y = temps_re, xout = elevs_out)$y
  return(temps)
}

#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
update_phyto <- function(p_initial,nml_name = 'aed2_phyto_pars.nml'){
  
  if(length(p_initial)<6){
    print('number of phyto group does not equal 6')
  }   
  nml_file <- paste0(working_directory,'/',nml_name)
  c <- file(nml_file, "r")
  fileLines <- readLines(c)
  close(c)
  lineStart <- substr(fileLines, 1, 1)
  ignoreLn <- lineStart == "!" | fileLines == ""
  lineStart <- lineStart[!ignoreLn]
  fileLines <- fileLines[!ignoreLn]
  l <- strsplit(fileLines, ",")
  
  l[[2]][2] = p_initial[1]
  l[[3]][2] = p_initial[2]
  l[[4]][2] = p_initial[3]
  l[[5]][2] = p_initial[4]
  l[[5]][2] = p_initial[5]
  
  zz <- file(nml_file, open = "wt")
  sink(zz)
  cat(noquote(paste(paste(l[[1]],collapse = ''),'\n')))
  cat(noquote(paste(paste(l[[2]],collapse = ','),'\n')))
  cat(noquote(paste(paste(l[[3]],collapse = ','),'\n')))
  cat(noquote(paste(paste(l[[4]],collapse = ','),'\n')))
  cat(noquote(paste(paste(l[[5]],collapse = ','),'\n')))
  cat(noquote(paste(paste(l[[6]],collapse = ','),'\n')))
  cat(noquote(paste(paste(l[[7]],collapse = ','),'\n')))
  sink()
}

#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
get_glm_nc_var_all_wq <- function(ncFile,working_dir, z_out,vars_depth, vars_no_depth, diagnostic_vars){
  glm_nc <- nc_open(paste0(working_dir,ncFile))
  tallest_layer <- ncvar_get(glm_nc, "NS")
  final_time_step <- length(tallest_layer)
  tallest_layer <- tallest_layer[final_time_step]
  heights <- ncvar_get(glm_nc, "z")
  heights_surf <- heights[tallest_layer,final_time_step]
  heights <- heights[1:tallest_layer,final_time_step]
  heights_out <- heights_surf - z_out
  
  snow <- ncvar_get(glm_nc, "hsnow")[final_time_step]
  ice_white <- ncvar_get(glm_nc, "hwice")[final_time_step] 
  ice_blue <- ncvar_get(glm_nc, "hice")[final_time_step] 
  avg_surf_temp <- ncvar_get(glm_nc, "avg_surf_temp")[final_time_step] 
  
  
  glm_temps <- ncvar_get(glm_nc, "temp")[1:tallest_layer, final_time_step]
  
  output <- array(NA,dim=c(tallest_layer,length(vars_depth)))
  for(v in 1:length(vars_depth)){
    var_modeled <- ncvar_get(glm_nc, vars_depth[v])
    output[,v] <- var_modeled[1:tallest_layer, final_time_step]
  }
  
  
  #if(length(vars_no_depth) > 0){
  #  output_no_depth <- rep(NA, length(vars_no_depth))
  #  for(v in 1:lengthlength(vars_no_depth)){
  #    if(vars_no_depth[v] == "secchi"){
  #      var_modeled <- ncvar_get(glm_nc, "extc_coef")
  #      var_modeled <- var_modeled[1:tallest_layer, final_time_step]
  #      output_no_depth[v] <- var_modeled[which.min(abs(heights_out - 1.0))]
  #    }else{
  #      output_no_depth[v] <- NA
  #  }
  #  }
  #}else{
  #  output_no_depth <- NA
  #}
  
  output_no_depth <- NA
  
  if(length(diagnostic_vars) > 0){
    diagnostics_output <- array(NA,dim=c(tallest_layer,length(diagnostic_vars)))
    for(v in 1:length(diagnostic_vars)){
      var_modeled <- ncvar_get(glm_nc, diagnostic_vars[v])
      diagnostics_output[,v] <- var_modeled[1:tallest_layer, final_time_step]
    }
  }else{
    diagnostics_output <- NA
  }
  
  mixing_vars <- ncvar_get(glm_nc, "restart_variables")
  
  nc_close(glm_nc)
  return(list(output = output,
              output_no_depth = output_no_depth,
              surface_height = heights_surf,
              depths_enkf = rev(heights_surf - heights),
              snow_wice_bice = c(snow, ice_white, ice_blue),
              avg_surf_temp = avg_surf_temp,
              mixing_vars = mixing_vars,
              diagnostics_output = diagnostics_output))
}


