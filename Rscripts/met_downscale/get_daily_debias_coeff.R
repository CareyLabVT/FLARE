get_daily_debias_coeff <- function(joined.data){
  # --------------------------------------
  # purpose: save coefficients for linear debiasing (slope, intercept, standard deviation of residuals, r2 of linear regression)
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  
  get_lm_coeff <- function(col.obs, col.for){
    model = lm(unlist(col.obs) ~ unlist(col.for))
    intercept = model$coefficients[1]
    slope = model$coefficients[2]
    res.sd = sd(residuals(model))
    res = residuals(model)
    r2 = summary(model)$r.squared
    return(list(intercept, slope, res.sd, r2))
  }
  
  get_lm_resid <- function(col.obs, col.for){
    model = lm(unlist(col.obs) ~ unlist(col.for))
    res = residuals(model)
    return(list(res))
  }
  
  df = data.frame(matrix(NA, ncol = length(VarNames), nrow = 6))
  colnames(df) = VarNames
  
  for (rowNum in 1:4){
    for(colNum in 1:length(VarNames)){
      df[rowNum, VarNames[colNum]] = get_lm_coeff(joined.data[,paste0(VarNames[colNum],".obs")], joined.data[,paste0(VarNames[colNum],".for")])[[rowNum]]
    }
  }
  
  df = as.data.frame(df) 
  row.names(df) <- c("intercept", "slope", "sd.res.daily", "r2.daily", "ds.res.hourly", "r2.hourly")
  # could convert to key column later instead of row,column
  
  df2 = NULL
  for(colNum in 1:length(VarNames)){
    tmp <- unlist(get_lm_resid(joined.data[,paste0(VarNames[colNum],".obs")], joined.data[,paste0(VarNames[colNum],".for")]))
    df2 = cbind(df2, tmp)
  }
  
  df2 <- cov(df2)
  
  return(list(df = df, df2 = df2))
}
