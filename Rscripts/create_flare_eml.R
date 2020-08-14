create_flare_eml <- function(file_name,
                             time_of_forecast,
                             forecast_iteration_id,
                             forecast_project_id,
                             nstates, 
                             npars, 
                             n_met_members, 
                             n_ds_members,
                             nmembers,
                             process_uncertainty,
                             weather_uncertainty,
                             initial_condition_uncertainty,
                             parameter_uncertainty,
                             met_downscale_uncertainty,
                             inflow_process_uncertainty){

  emld::eml_version("eml-2.2.0")
  
  nc <- nc_open(file_name)
  t <- ncvar_get(nc,'time')
  local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
  full_time <- as.POSIXct(t, 
                          origin = '1970-01-01 00:00.00 UTC', 
                          tz = "UTC")

  data_assimilation <- ncvar_get(nc, "data_assimilation")
  
  forecast_issue_time <-ncatt_get(nc, varid = 0)$forecast_issue_time
  
  var_names <- names(nc$var)
  
  attributeName <- rep(NA, length(var_names))
  attributeDefinition <- rep(NA, length(var_names))
  unit <- rep(NA, length(var_names))
  formatString <- rep(NA, length(var_names))
  numberType <- rep(NA, length(var_names))
  definition <- rep(NA, length(var_names))
  col_classes <- rep("numeric", length(var_names))
  for(i in 1:length(var_names)){
    curr_var <- nc$var[[i]]
    attributeName[i] <- curr_var$name
    attributeDefinition[i] <- curr_var$longname 
    unit[i] <- "dimensionless"
    formatString[i] <- NA
    numberType[i] <- typeof(ncvar_get(nc, var_names[i]))
    if(numberType[i] == "double"){numberType[i] = "real"}
  }
  
  nc_close(nc)
  
  attributes <- tibble(
    attributeName = attributeName,
    attributeDefinition = attributeDefinition,
    unit = unit,
    formatString = formatString,
    numberType = numberType,
    definition = definition
    
  )

  
  attrList <- set_attributes(attributes, 
                            col_classes = col_classes)

  ## sets metadata about the file itself (name, file type, size, MD5, etc)
  physical <- set_physical(basename(file_name))
  ## set metadata for the file as a whole
  dataTable <- eml$dataTable(
    entityName = "output",  ## this is a standard name to allow us to distinguish this entity from 
    entityDescription = "Forecast of water physics, chemistry, and biology",
    physical = physical,
    attributeList = attrList)
  
  set_unitList(data.frame(id = 'netcdf', unitType="dimensionless", "parentSI"="dimensionless", "multiplierToSI" = 1, "description"="units are defined in the netcdf file"))
  
  coverage <- 
    set_coverage(begin = as_date(first(full_time)), 
                 end = as_date(last(full_time)),
                 #sci_names = "NA",
                 geographicDescription = lake_name,
                 west = lake_longitude, east = lake_longitude, 
                 north = lake_latitude, south = lake_latitude)
  
  
  keywordSet <- list(
    list(
      keywordThesaurus = "EFI controlled vocabulary",
      keyword = list("forecast",
                     "water quality",
                     "timeseries")
    ))
  
  abstract <- abstract #system.file("extdata", "abstract.md", package="EFIstandards", mustWork = TRUE)
  
  dataset = eml$dataset(
    title = forecast_title,
    creator = me,
    contact = list(references=me$id),
    pubDate = as_date(time_of_forecast),
    intellectualRights = intellectualRights,
    abstract =  abstract,
    dataTable = dataTable,
    keywordSet = keywordSet,
    coverage = coverage
  )
  
  if(initial_condition_uncertainty){
    initial_conditions = list(
      # Possible values: no, contains, data_driven, propagates, assimilates
      uncertainty = "assimilates",
      # Number of parameters / dimensionality
      complexity = nstates,
      propagation = list(
        type = "ensemble", # ensemble vs analytic
        size = nmembers          # required if ensemble
      ),
      assimilation = list(
        type = "EnKF",
        reference = "NA",
        complexity = nstates
      )
    )
  }else{
    initial_conditions = list(
      # Possible values: no, contains, data_driven, propagates, assimilates
      uncertainty = "data_driven",
      # Number of parameters / dimensionality
      complexity = nstates
    )
  }
  
  if(parameter_uncertainty & npars > 0){
    parameters = list(
      uncertainty = "assimilates",
      complexity = npars,
      propagation = list(
        type = "ensemble", # ensemble vs analytic
        size = nmembers          # required if ensemble
      ),
      assimilation = list(
        type = "EnKF",
        reference = "NA",
        complexity = npars
      )
    )
  }else{
    parameters = list(
      uncertainty =  "contains",
      complexity = 0
    )
  }
  
  if(process_uncertainty){
    process_error = list(
      uncertainty = "propagates",
      propagation = list(
        type = "ensemble", # ensemble vs analytic
        size = nmembers          # required if ensemble
      ),
      complexity = nstates,
      covariance = TRUE,
      localization = "Distance extinction of covariance"
    )

  }else{
    process_error = list(
      uncertainty = "contains",
      complexity = nstates
    )
  }
  
  if(weather_uncertainty | met_downscale_uncertainty){
    drivers = list(
      uncertainty = "propagates",
      complexity = n_met_members * n_ds_members,
      propagation = list(
        type = "ensemble", # ensemble vs analytic
        size = n_met_members * n_ds_members)
    )
    
  }else{
    drivers = list(
      uncertainty = "contains"
    )
  }
  
  forecast_horizon <- length(which(data_assimilation == 0))
  
  if(forecast_horizon == 0){
    forecast_horizon = 1000000
  }
  
  additionalMetadata <- eml$additionalMetadata(
    #  describes="forecast",  ## not sure how to find the correct ID for this to be valid
    metadata = list(
      forecast = list(
        ## Basic elements
        timestep = "1 day", ## should be udunits parsable; already in coverage -> temporalCoverage?
        forecast_horizon = forecast_horizon,
        forecast_issue_time = forecast_issue_time,
        forecast_iteration_id = forecast_iteration_id,
        forecast_project_id = forecast_project_id,
        model_description = model_description,
        ## UNCERTAINTY CLASSES
        initial_conditions = initial_conditions,
        parameters = parameters,
        random_effects = list(
          uncertainty = "no"
        ),
        process_error = process_error,
        drivers = drivers
        # assimilation_method ## required if any uncertainty = assimilates
      ) # forecast
    ) # metadata
  ) # eml$additionalMetadata
  
  my_eml <- eml$eml(dataset = dataset,
                    additionalMetadata = additionalMetadata,
                    packageId = forecast_project_id, 
                    system = "uuid"  ## system used to generate packageId
  )
  
  eml_validate(my_eml)
  
  EFIstandards::forecast_validator(my_eml)
  
  write_eml(my_eml, paste0(forecast_location, "/", file_path_sans_ext(basename(file_name)),"-eml.xml"))
  
}