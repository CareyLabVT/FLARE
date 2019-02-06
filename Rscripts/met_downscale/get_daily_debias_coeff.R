get_daily_debias_coeff <- function(joined.data, VarInfo){
  # --------------------------------------
  # purpose: save coefficients for linear debiasing (slope, intercept, standard deviation of residuals, r2 of linear regression), for comparison of total values over time (precip), and the covariance matrix between variables
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  
  get_coeff <- function(col.obs, col.for, method){
    if(method == "lm"){
      model = lm(unlist(col.obs) ~ unlist(col.for))
      intercept = model$coefficients[1]
      slope = model$coefficients[2]
      res = residuals(model)
      r2 = summary(model)$r.squared
    }else{
      # option where comparing sum rather than fitting a line
      slope = sum(col.obs)/sum(col.for)
      intercept = 0
      res = col.for - col.obs
      r2 = NA
    }
    res.sd = sd(unlist(res))
    
    return(list(intercept, slope, res.sd, r2, res))
  }
  
  n_vars = nrow(VarInfo)
  VarNames = VarInfo$VarNames
  df = data.frame(matrix(NA, ncol = n_vars, nrow = 6))
  colnames(df) = VarInfo$VarNames
  
  for (rowNum in 1:4){
    for(colNum in 1:n_vars){
      VarName = VarNames[colNum]
      method = VarInfo$debias_method[colNum]
      df[rowNum, VarName] = get_coeff(joined.data[,paste0(VarName,".obs")], joined.data[,paste0(VarName,".for")], method = method)[[rowNum]]
    }
  }
  
  df = as.data.frame(df) 
  row.names(df) <- c("intercept", "slope", "sd.res.daily", "r2.daily", "ds.res.hourly", "r2.hourly")

  ## covariance matrix
  df2 = NULL

  for(colNum in 1:n_vars){
    VarName = VarNames[colNum]
    method = VarInfo$debias_method[colNum]
    tmp <- as.numeric(unlist(get_coeff(joined.data[,paste0(VarName,".obs")], joined.data[,paste0(VarName,".for")], method = method)[5]))
    df2 = cbind(df2, tmp)
  }
  
  cov <- cov(df2)
  colnames(cov) <- VarNames
  rownames(cov) <- VarNames
  
  return(list(df, cov))
}
