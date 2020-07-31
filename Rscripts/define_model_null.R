run_model <- function(i,
                      m,
                      mixing_vars,
                      curr_start,
                      curr_stop,
                      par_names,
                      curr_pars,
                      working_directory,
                      par_nml,
                      num_phytos,
                      glm_depths,
                      surface_height,
                      simulate_SSS,
                      x,
                      full_time_local, 
                      wq_start, 
                      wq_end,
                      management_input, 
                      hist_days, 
                      forecast_sss_on,
                      sss_depth,
                      use_specified_sss,
                      modeled_depths,
                      ndepths_modeled,
                      curr_met_file,
                      inflow_file_names,
                      inflow_outflow_index,
                      outflow_file_names,
                      glm_output_vars,
                      diagnostics_names,
                      machine,
                      npars,
                      num_wq_vars,
                      snow_ice_thickness,
                      avg_surf_temp,
                      x_star,
                      diagnostics){
  
  nstates <- dim(x)[3] - npars 
  
  x_star[m, ] <- x[i - 1, m ,1:nstates]
  
  return(list(x_star = x_star,
              surface_height = NA, 
              snow_ice_thickness = NA,
              avg_surf_temp = NA,
              mixing_vars = NA,
              diagnostics = NA,
              glm_depths = NA))
}


set_up_model <- function(code_folder, 
                         working_directory, 
                         base_GLM_nml, 
                         num_wq_vars, 
                         base_AED_nml,
                         base_AED_phyto_pars_nml,
                         base_AED_zoop_pars_nml,
                         ndepths_modeled,
                         modeled_depths,
                         the_sals_init,
                         machine){
  
  
}