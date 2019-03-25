plot_forecast <- function(pdf_file_name,output_file,catwalk_fname,include_wq,forecast_days,code_location,save_location,data_location,plot_summaries,pre_scc,push_to_git,pull_from_git,use_ctd){

    source(paste0(code_location,'/extract_temp_chain.R'))
    source(paste0(code_location,'/extract_temp_CTD.R'))
  
    the_depths_init <- c(0.1, 0.33, 0.66, 1.00, 1.33,1.66,2.00,2.33,2.66,3.0,3.33,3.66,4.0,4.33,4.66,5.0,5.33,5.66,6.0,6.33,6.66,7.00,7.33,7.66,8.0,8.33,8.66,9.00,9.33)
    num_pars <- 0
    output_tz <- 'EST5EDT'
    TempObservedDepths <- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8,9)
    DoObservedDepths <- c(1,5,9)
    Chla_fDOM_ObservedDepths <- 1
    wq_names <- c('OXY_oxy',
                  'CAR_pH','CAR_dic','CAR_ch4',
                  'SIL_rsi',
                  'NIT_amm', 'NIT_nit',
                  'PHS_frp',
                  'OGM_doc','OGM_poc','OGM_don','OGM_pon','OGM_dop','OGM_pop',
                  'PHY_CYANOPCH1','PHY_CYANONPCH2','PHY_CHLOROPCH3','PHY_DIATOMPCH4',
                  'ZOO_COPEPODS1','ZOO_DAPHNIABIG2','ZOO_DAPHNIASMALL3')
    nMETmembers =21
    
    
    mia_location <- paste0(data_location,'/','mia-data')
    setwd(mia_location)
    if(pull_from_git){
      system(paste0("git pull"))
    }
    catwalk_fname <- paste0(mia_location,'/','Catwalk.csv')
    
    
    
    nc <- nc_open(output_file)
    t <- ncvar_get(nc,'time')
    full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = output_tz)
    full_time_day <- strftime(full_time, format="%Y-%m-%d")
    temp_mean <- ncvar_get(nc,'temp_mean')
    temp <- ncvar_get(nc,'temp')
    temp_upper <- ncvar_get(nc,'temp_upperCI')
    temp_lower  <- ncvar_get(nc,'temp_lowerCI')
    depths <- ncvar_get(nc,'z')
    Kw <- ncvar_get(nc,'Kw')
    SW_LW_factor <- ncvar_get(nc,'SW_LW_factor')
    zone1temp <- ncvar_get(nc,'zone1temp')
    zone2temp <- ncvar_get(nc,'zone2temp')
    forecasted <- ncvar_get(nc,'forecasted')

    if(include_wq){
      wq_output <- array(NA,dim = c(length(wq_names),dim(temp)[1],dim(temp)[2],dim(temp)[3]))
      for(i in 1:length(wq_names)){
      wq_output[i,,,] <- ncvar_get(nc,wq_names[i])
      }
      
      OXY_oxy <- wq_output[which(wq_names == 'OXY_oxy'),,,]
      PHY_CYANOPCH1 <- wq_output[which(wq_names == 'PHY_CYANOPCH1'),,,]
    }
    
    nc_close(nc)

    
    #PROCESS TEMPERATURE OBSERVATIONS

    obs_temp <- extract_temp_chain(fname = catwalk_fname,full_time,depths = the_depths_init,observed_depths_temp = TempObservedDepths,input_tz = 'EST5EDT', output_tz = reference_tzone)
    for(i in 1:length(obs_temp$obs[,1])){
      for(j in 1:length(obs_temp$obs[1,])){
        if(obs_temp$obs[i,j] == 0 | is.na(obs_temp$obs[i,j]) | is.nan(obs_temp$obs[i,j])){
          obs_temp$obs[i,j] = NA
        } 
      }
    }
    

    
    #PROCESS DO OBSERVATIONS

    obs_do <- extract_do_chain(fname = catwalk_fname,full_time,depths = the_depths_init,observed_depths_do= DoObservedDepths,input_tz = 'EST5EDT', output_tz = reference_tzone)
    obs_do$obs <- obs_do$obs*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
    init_do1 <- obs_do$obs[1,]
  
    obs_chla_fdom <- extract_chla_chain(fname = catwalk_fname,full_time,depths = the_depths_init,observed_depths_chla_fdom= Chla_fDOM_ObservedDepths,input_tz = 'EST5EDT', output_tz = reference_tzone)
    
    #Use the CTD observation rather than the sensor string when CTD data is avialable
    if(use_ctd){
      ## LOOK AT CTD DATA
      fl <- c(list.files('/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SCC_data/preSCC/', pattern = 'CTD', full.names = TRUE))
      obs_ctd <- extract_temp_CTD(fname = fl[1],full_time_day,depths = the_depths_init,input_tz = 'EST5EDT', output_tz = reference_tzone)
      obs_ctd$obs_do <- obs_ctd$obs_do*1000/32
      for(i in 1:length(full_time_day)){
        if(!is.na(obs_ctd$obs_temp[i,1])){
          obs_temp$obs[i,] <- obs_ctd$obs_temp[i,]
          obs_do$obs[i,] <- obs_ctd$obs_do[i,]
          obs_chla_fdom$Chla_obs[i,] <- obs_ctd$obs_chla[i,]
        }
      }
    }
    

    
    nsteps <- length(full_time)
    if(length(which(forecasted == 1))>0){
      forecast_index <- which(forecasted == 1)[1]
    }else{
      forecast_index <- 0
    }
    nlayers <- length(depths)
    
    if(include_wq){
      nobs <- length(TempObservedDepths) + length(DoObservedDepths)
    }else{
      nobs <- length(TempObservedDepths)
    }
    
    #Observations for each observed state at each time step
    #an observation with at least 1 observation but without an observation in a time-step gets assigned an NA
    #z <- t(matrix(rep(NA,nobs), nrow = nobs, ncol = nsteps))
    
    if(include_wq){
      z <- cbind(obs_temp$obs,obs_do$obs)
    }else{
      z <- cbind(obs_temp$obs) 
    }
    
    num_wq_vars <- length(wq_names) 
    if(include_wq){
      temp_start <- 1
      temp_end <- length(the_depths_init)
      wq_start <- rep(NA,num_wq_vars)
      wq_end <- rep(NA,num_wq_vars)
      for(wq in 1:num_wq_vars){
        if(wq == 1){
          wq_start[wq] <- temp_end+1
          wq_end[wq] <- temp_end + (length(the_depths_init))
        }else{
          wq_start[wq] <- wq_end[wq-1]+1
          wq_end[wq] <- wq_end[wq-1] + (length(the_depths_init))
        }
        
        if(num_pars > 0){
          par1 <- wq_end[num_wq_vars] + 1
          par2 <- par1 + 1
          par3 <-  par2 + 1
        }else{
          par1 <- wq_end[num_wq_vars]
          par2 <- wq_end[num_wq_vars]
          par3 <- wq_end[num_wq_vars]
        }
        
      }
    }else{
      temp_start <- 1
      temp_end <- length(the_depths_init)
      if(num_pars > 0){
        par1 <- temp_end + 1
        par2 <- par1 + 1
        par3 <-  par2 + 1
      }else{
        par1 <- temp_end
        par2 <- temp_end
        par3 <- temp_end
      }
    }
    
    #FIGURE OUT WHICH DEPTHS HAVE OBSERVATIONS
    if(include_wq){
      obs_index <- rep(NA,length(the_depths_init)*(num_wq_vars+1))
      obs_index[1:length(the_depths_init)] <- seq(1,length(the_depths_init),1)
      for(wq in 1:num_wq_vars){
        obs_index[wq_start[wq]:wq_end[wq]] <- seq(wq_start[wq],wq_end[wq],1)
      }
    }else{
      obs_index <- rep(NA,length(the_depths_init))
      obs_index[1:length(the_depths_init)] <- seq(1,length(the_depths_init),1)
    }
    
    
    #Matrix for knowing which state the observation corresponds to
    z_states <- t(matrix(obs_index, nrow = length(obs_index), ncol = nsteps))
    
    pdf(paste0(save_location,'/',pdf_file_name, ".pdf"),width = 12, height = 12)
    par(mfrow=c(4,3))
    
    for(i in 1:nlayers){
      model = i
      if(length(which(z_states[1,] == i) > 0)){
        obs = which(z_states[1,] == i)
      }else{
        obs = NA
      }
      ylim = range(c(temp_mean[,],temp_upper[,],temp_lower[,],c(z[,1:length(TempObservedDepths)])),na.rm = TRUE) 
      #ylim = range(c(temp_mean[,model],temp_upper[,model],temp_lower[,model],c(z[,obs])),na.rm = TRUE)
      if(plot_summaries){
        plot(full_time,temp_mean[,model],type='l',ylab=expression(~degree~C),xlab='Day',main = paste('Depth: ',depths[i],' m',sep=''),ylim=ylim)
        points(full_time,temp_upper[,model],type='l',lty='dashed')
        points(full_time,temp_lower[,model],type='l',lty='dashed')
      }else{
        plot(full_time,temp[,1,model],type='l',ylab=expression(~degree~C),xlab='Day',main = paste('Depth: ',depths[i],' m',sep=''),ylim=ylim)
        if(length(temp[1,,model]) > 1){
          for(m in 2:length(temp[1,,model])){
            points(full_time,temp[,m,model],type='l')
          }
        }
      }
      if(!is.na(obs)){
        tmp = z[,obs]
        tmp[is.na(tmp)] = -999
        points(full_time,tmp,col='red',pch=19,cex=1.0)
      }
      if(forecast_index > 1){
        abline(v = full_time[forecast_index-1])
      }
    }
    
    ###PLOT OF PARAMETERS IF FIT
    plot.new()
    if(num_pars > 0){
    plot(full_time,rowMeans(Kw[,]),xlab ='Day',ylab = 'Kw',type='l',ylim = range(c(Kw),na.rm=TRUE))
    if(length(Kw[1,]) > 1){
      for(m in 1:length(Kw[1,])){
        points(full_time,Kw[,m],type='l')
      }
    }
    plot(full_time,rowMeans(zone1temp[,]),xlab ='Day',ylab = 'Zone 1 sediment temp',type='l',ylim = range(c(zone1temp),na.rm=TRUE))
    if(length(zone1temp[1,]) > 1){
      for(m in 1:length(zone1temp[1,])){
        points(full_time,zone1temp[,m],type='l')
      }
    }
    plot(full_time,rowMeans(zone2temp[,]),xlab ='Day',ylab = 'Zone 2 sediment temp',type='l',ylim = range(c(zone2temp),na.rm=TRUE))
    if(length(zone2temp[1,]) > 1){
      for(m in 1:length(zone2temp[1,])){
        points(full_time,zone2temp[,m],type='l')
      }
    }
    plot(full_time,rowMeans(SW_LW_factor[,]),xlab ='Day',ylab = 'SW_LW_factor',type='l',ylim = range(c(SW_LW_factor),na.rm=TRUE))
    if(length(SW_LW_factor[1,]) > 1){
      for(m in 1:length(SW_LW_factor[1,])){
        points(full_time,SW_LW_factor[,m],type='l')
      }
    }
    }
    
    if(include_wq){
      par(mfrow=c(4,3))
      
      for(i in 1:nlayers){
        model = i
        if(length(which(z_states[1,] == length(the_depths_init)+i) > 0)){
          obs = which(z_states[1,] == length(the_depths_init)+i)
        }else{
          obs = NA
        }
        ylim = range(c(OXY_oxy[,,]),na.rm = TRUE) 
        #ylim = range(c(temp_mean[,model],temp_upper[,model],temp_lower[,model],c(z[,obs])),na.rm = TRUE)
        #if(plot_summaries){
        #  plot(full_time,temp_mean[,model],type='l',ylab='water temperature (celsius)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
        #  points(full_time,temp_upper[,model],type='l',lty='dashed')
        #  points(full_time,temp_lower[,model],type='l',lty='dashed')
        #}else{
        plot(full_time,OXY_oxy[,1,model],type='l',ylab='Oxygen (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
        if(length(temp[1,,model]) > 1){
          for(m in 2:length(temp[1,,model])){
            points(full_time,OXY_oxy[,m,model],type='l')
          }
        }
        
        if(!is.na(obs)){
          tmp = z[,obs]
          tmp[is.na(tmp)] = -999
          points(full_time,tmp,col='red',pch=19,cex=1.0)
        }
        
        if(forecast_index > 1){
          abline(v = full_time[forecast_index-1])
        }
      }
      
      par(mfrow=c(4,3))
      
      for(i in 1:nlayers){
        model = i
        if(length(which(z_states[1,] == length(the_depths_init)+i) > 0)){
          obs = which(z_states[1,] == length(the_depths_init)+i)
        }else{
          obs = NA
        }
        ylim = range(c(PHY_CYANOPCH1[,,]),na.rm = TRUE) 
        plot(full_time,PHY_CYANOPCH1[,1,model],type='l',ylab='PHY_CYANOPCH1 (umol/m3)',xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
        if(length(temp[1,,model]) > 1){
          for(m in 2:length(temp[1,,model])){
            points(full_time,PHY_CYANOPCH1[,m,model],type='l')
          }
        }
        
        #      if(!is.na(obs)){
        #        tmp = z[,obs]
        #        tmp[is.na(tmp)] = -999
        #        points(full_time,tmp,col='red',pch=19,cex=1.0)
        #      }
        
        if(forecast_index > 1){
          abline(v = full_time[forecast_index-1])
        }
      }
      
      par(mfrow=c(4,3))
      for(wq in 1:length(wq_names)){
        
        for(i in c(4,16,25)){
        ylim = range(c(wq_output[wq,,,]),na.rm = TRUE) 
        plot(full_time,wq_output[wq,,1,i],type='l',ylab=wq_names[wq],xlab='time step (day)',main = paste('depth: ',depths[i],' m',sep=''),ylim=ylim)
          for(m in 2:length(temp[1,,model])){
            points(full_time,wq_output[wq,,m,i],type='l')
          }
        }
      }
      
    }
    
    
    
    

    
    
    ###PLOT HISTOGRAMS OF FORECAST
    par(mfrow=c(4,3))
    if(!is.na(forecast_index)){
      if(length(which(forecast_index == 1)) > 6){
        xlim<- range(c(temp[forecast_index+7,,obs_index[1]],z[forecast_index,1]),na.rm = TRUE)
        hist(temp[forecast_index+7,,obs_index[1]],main='0.1m temp. 7 days forecast',xlab='Temperature',xlim=xlim)
        abline(v= z[forecast_index+7,1],col='red')
        xlim<- range(c(temp[forecast_index+7,,obs_index[5]],z[forecast_index+7,5]),na.rm = TRUE)
        hist(temp[forecast_index+7,,obs_index[5]],main='4m temp. 7 days forecast',xlab='Temperature',xlim=xlim)
        abline(v= z[forecast_index+7,5],col='red')
        xlim<- range(c(temp[forecast_index+7,,obs_index[10]],z[forecast_index+7,10]),na.rm = TRUE)
        hist(temp[forecast_index+7,,obs_index[10]],main='9m temp. 7 days forecast',xlab='Temperature',xlim=xlim)
        abline(v= z[forecast_index+7,10],col='red')
      }
      if(length(which(forecast_index == 1)) > 13){
        xlim<- range(c(temp[forecast_index+14,,obs_index[1]],z[forecast_index+14,1]),na.rm = TRUE)
        hist(temp[forecast_index+14,,obs_index[1]],main='0.1m temp. 14 days forecast',xlab='Temperature',xlim=xlim)
        abline(v= z[forecast_index+14,1],col='red')
        xlim<- range(c(temp[forecast_index+14,,obs_index[5]],z[forecast_index+14,5]),na.rm = TRUE)
        hist(temp[forecast_index+14,,obs_index[5]],main='4m temp. 14 days forecast',xlab='Temperature',xlim=xlim)
        abline(v= temp[forecast_index+14,5],col='red')
        xlim<- range(c(temp[forecast_index+14,,obs_index[10]],z[forecast_index+14,10]),na.rm = TRUE)
        hist(temp[forecast_index+14,,obs_index[10]],main='9m temp. 14 days forecast',xlab='Temperature',xlim=xlim)
        abline(v= z[forecast_index+14,10],col='red')
      }
    }
    dev.off()
    
  if(forecast_days == 16){

    full_time_past <- seq(full_time[1] - days(5), full_time[2], by = "1 day") # grid
    full_time_combined <- seq(full_time_past[1], full_time[length(full_time)], by = "1 day")
    full_time_plotting <- seq(full_time_past[1]-days(3), full_time[length(full_time)]+days(5), by = "1 day")
    
    
    obs_temp <- extract_temp_chain(fname = catwalk_fname,full_time_past,depths = the_depths_init,observed_depths_temp = TempObservedDepths,input_tz = 'EST5EDT', output_tz = reference_tzone)
    for(i in 1:length(obs_temp$obs[,1])){
      for(j in 1:length(obs_temp$obs[1,])){
        if(obs_temp$obs[i,j] == 0 | is.na(obs_temp$obs[i,j]) | is.nan(obs_temp$obs[i,j])){
          obs_temp$obs[i,j] = NA
        } 
      }
    }
    init_temps <- obs_temp$obs[1,]

    forecast_index <- which(forecasted == 1)[1]
    nlayers <- length(depths)
    
    focal_depths <- c(4,16,25)
    png( paste0(save_location,'/',pdf_file_name, "_management.png"),width = 12, height = 6,units = 'in',res=300)
    par(mfrow=c(1,2))
    
    #PLOT OF TURNOVER PROBABILITY

    prob_zero <- rep(NA,length(seq(3,18,1)))
    for(i in 3:18){
      prob_zero[i-2] = 100*length(which(temp[i,,4] - temp[i,,25] < 1))/length((temp[i,,obs_index[1]]))
    }
    
    plot(full_time_plotting,rep(-99,length(full_time_plotting)),ylim=c(0,100),xlab = 'date',ylab = '% chance')
    title('Turnover forecast',cex.main=0.9)
    
    points(full_time[3:18],prob_zero,type='o',ylim=c(0,100),xlab = 'date',ylab = 'Probablity of turnover')
    axis(1, at=full_time_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
    abline(v = full_time_past[length(full_time_past)])
    text(full_time_past[length(full_time_past)-2],80,'past')
    text(full_time[4],80,'future')
    #HISTORICAL AND FUTURE TEMPERATURE
    depth_colors <- c("firebrick4",
                      NA,
                      NA,
                      "firebrick1",
                      NA,
                      NA,
                      "DarkOrange1",
                      NA,
                      NA,
                      "gold",
                      NA,
                      NA,
                      "greenyellow",
                      NA,
                      NA,
                      "medium sea green",
                      NA,
                      NA,
                      "sea green",
                      NA,
                      NA,
                      "DeepSkyBlue4",
                      NA,
                      NA,
                      "blue2",
                      NA,
                      NA,
                      "blue4",
                      NA)
    
    plot(full_time_plotting,rep(-99,length(full_time_plotting)),ylim=c(-5,35),xlab = 'date',ylab = expression(~degree~C))
    title(paste0('Water temperature forecast'),cex.main=0.9)
    tmp_day <- full_time[-1][1]
    axis(1, at=full_time_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
    depth_colors_index = 0
    for(i in 1:length(depths)){
      if(length(which(depths[i]  == TempObservedDepths)) >= 1 ){
        depth_colors_index <- i
        points(full_time_past, obs_temp$obs[1:length(full_time_past),i],type='l',col=depth_colors[depth_colors_index],lwd=1.5)
        index <- which(obs_index[i]  == focal_depths)
        if(length(index) == 1){
          points(full_time[-1], temp_mean[-1,obs_index[i]],type='l',lty='dashed',col=depth_colors[depth_colors_index],lwd=1.5) 
          points(full_time[-1], temp_upper[-1,obs_index[i]],type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=1.5)
          points(full_time[-1], temp_lower[-1,obs_index[i]],type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=1.5)
        }
      }
    }
    
    abline(v = full_time_past[length(full_time_past)])
    text(full_time_past[length(full_time_past)-2],30,'past')
    text(full_time[4],30.1,'future')
    if(temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]] == temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]] |
       temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]] == temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]] |
       temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]] == temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]]){
      text(full_time_plotting[length(full_time_plotting)-3], temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]], '1m:1660', col='firebrick1')
      text(full_time_plotting[length(full_time_plotting)-2], temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]], '5m:1647', col='medium sea green')
      text(full_time_plotting[length(full_time_plotting)-1], temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]], '8m:1637', col='blue2')
    }else{
      text(full_time_plotting[length(full_time_plotting)-2], temp_mean[length(temp_mean[,focal_depths[1]]),focal_depths[1]], '1m:1660', col='firebrick1')
      text(full_time_plotting[length(full_time_plotting)-2], temp_mean[length(temp_mean[,focal_depths[2]]),focal_depths[2]], '5m:1647', col='medium sea green')
      text(full_time_plotting[length(full_time_plotting)-2], temp_mean[length(temp_mean[,focal_depths[3]]),focal_depths[3]], '8m:1637', col='blue2') 
    }
    
    legend("left",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m","8m", "9m"),
           text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                      "DeepSkyBlue4", "blue2", "blue4"), cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
    legend('topright', c('mean','confidence bounds'), lwd=1.5, lty=c('dashed','dotted'),bty='n',cex = 1)
    
    mtext(paste0('Falling Creek Reservoir\n',month(tmp_day),'/',day(tmp_day),'/',year(tmp_day)), side = 3, line = -2, outer = TRUE, font = 2)
    dev.off()

    jpeg(paste0(save_location,'/TO_',pdf_file_name, ".png"),width = 6, height = 6,units = 'in',res=300)
    plot(full_time_plotting,rep(-99,length(full_time_plotting)),ylim=c(0,100),xlab = 'date',ylab = '% chance')
    title('Turnover forecast',cex.main=0.9)
    points(full_time[3:18],prob_zero,type='o',ylim=c(0,100),xlab = 'date',ylab = 'Probablity of turnover')
    axis(1, at=full_time_plotting + hours(4),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)
    abline(v = full_time_past[length(full_time_past)])
    text(full_time_past[length(full_time_past)-2],80,'past')
    text(full_time[4],80,'future')
    abline(v = as.POSIXct('2018-10-21 20:00:00 EDT', tz = 'EST5EDT'),col='red')
    dev.off()
    
    if(push_to_git){
      setwd(save_location)
      
      file.copy(from = paste0(save_location,'/',pdf_file_name, "_management.png"), to = paste0(save_location,'/','Current_forecast.png'),overwrite=TRUE)
      system(paste0('git add ', pdf_file_name, ".pdf"))
      system(paste0('git add Current_forecast.png'))
      system('git commit -m "forecast and plots"')
      system('git push')
    }
  }
}

