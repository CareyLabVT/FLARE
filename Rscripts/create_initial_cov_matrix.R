if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")

library(mvtnorm)


###Step 1 ####################
# Create initial Qt matrix

#Typical temperatures for summer strafied period
depths <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,
            4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)
height <- 9.4 - depths
temps <- c(27.82136, 27.82105, 27.60364, 26.86277, 26.80006, 26.77351, 26.14525, 25.08262,
           24.20316, 23.33211, 22.13318, 20.64284, 19.26384, 18.06365, 14.43534, 13.31145,
           12.94634, 12.73079, 12.59369, 12.47170, 12.34325, 12.21184, 12.08016, 11.97002,
           11.83500, 11.70664, 11.63835, 11.53222, 11.48501)

nsamples <- 1000
top_temp_error <- 0.5
thermo_depth_error <- 0.14
new_temps <- array(NA,dim=c(nsamples,length(height)))


plot(temps,height,xlim=c(0,35))
for(i in 1:nsamples){
  top <- rnorm(1,0,top_temp_error)
  if(top > 0){
    bottom <- runif(1,0,top*0.5)
  }else{
    bottom <- runif(1,top*0.5,0)
  }
  tmp <- approxfun(c(height[1],height[length(height)]),c(top,bottom),rule = 2)
  
  corr_temps <- temps + tmp(height)
  
  corr <- rnorm(1,0,thermo_depth_error)
  corr_height <- height + corr
  corrupt_profile2 <- approxfun(corr_height,corr_temps,rule = 2)
  new_temps[i,] <- corrupt_profile2(height)
  points(new_temps[i,],height,col='green',type='o')
}

Qt <- cov(new_temps)

write.csv(Qt,file='/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE/sim_files/Qt_cov_matrix_init.csv',
          row.names = FALSE)