###Information and extra lines that might be helpful###
#####
### This function aggregates historical and real time inflow data at FCR,
### converts flow to cms. This creates one output file named "inflow_postQAQC.csv"

#Inputs to function:
#  Historical flow data. Here, I need two historical files from github to fill all of the time from 2013 until diana was launched ('hist1' and hist2').
#  Diana data ('FCRweir.csv' from git)
#  Local timezone
#  Timezone of input files (just to be similar to other FLARE fucntions)

#Units:
#  flow = cms
#  wtr_temp = degC
#  Timestep = daily
#Missing data is assigned the prior days flow 

##These lines may be useful for getting this into FLARE
#
#     data_location <- paste0("your working directory")
#     diana_location <- paste0(data_location, "/", "diana_data") 

#     if(!file.exists(diana_location)){
#       setwd(data_location)
#       system("git clone -b diana-data --single-branch https://github.com/CareyLabVT/SCCData.git diana-data/")  
#     }
#
#
#     setwd(diana_location)
#     system(paste0("git pull"))
#####
inflow_qaqc <- function(fname,
                        cleaned_inflow_file,
                        local_tzone, 
                        input_file_tz,
                        working_directory){
  
  ##Step 1: Pull required data from GitHub ##
  diana_location <- fname[1]
  hist1_location <- fname[2]
  hist2_location <- fname[3]
  ##Step 2: Read in historical flow data, clean, subset to 2013 - April 22nd 2019, and aggregate to daily mean##
  #reformating timestamp a few times currently required, but I'm working to make that process cleaner.
  
  flownames <- c('TIMESTAMP', 'flow_cms', 'wtr_weir')
  
  hist1 <- read_csv(hist1_location, skip=1, col_names = F)
  hist1 <- hist1[,c(1:3)]
  colnames(hist1) <- flownames
  TIMESTAMP_in <- force_tz(hist1$TIMESTAMP, tzone = input_file_tz)
  hist1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
  hist1$TIMESTAMP <- as.Date(hist1$TIMESTAMP, format = '%Y-%m-%d')
  
  
  hist2 <- read_csv(hist2_location, skip=1, col_names = F)
  hist2 <- hist2[,c(3,7,8)]
  colnames(hist2) <- flownames
  TIMESTAMP_in <- force_tz(hist2$TIMESTAMP, tzone = input_file_tz)
  hist2$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
  hist2 = aggregate(list(flow_cms = hist2$flow_cms, wtr_weir = hist2$wtr_weir), list(TIMESTAMP = cut(hist2$TIMESTAMP, "1 day")), mean);
  hist2$TIMESTAMP <- as.POSIXct(hist2$TIMESTAMP, format = '%Y-%m-%d')
  hist2 = hist2[hist2$TIMESTAMP > as.POSIXct('2018-12-31') & hist2$TIMESTAMP < as.POSIXct('2019-04-22'),]
  
  hist_inflow <- rbind(hist1, hist2) #inflow from 2013-April 2019
  
  ##Step 2: Read in diana data, convert flow from PSI to CSM, calculations to account for building new weir in June 2019 (FCR Specific), and aggregate to daily mean.##
  diana <- read.csv(diana_location, skip=4, header=F)
  diana_headers <- read.csv(diana_location, skip=1, header = F, nrows= 1, as.is=T)
  colnames(diana) <- diana_headers
  inflow <- diana[,c(1,6,7)]
  colnames(inflow)[colnames(inflow)=="Lvl_psi"] <- "psi_corr"     #FCR specific
  inflow$TIMESTAMP <- as.POSIXct(inflow$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
  TIMESTAMP_in <- force_tz(inflow$TIMESTAMP, tzone = input_file_tz)
  inflow$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)
  
  inflow_pre <- inflow[inflow$TIMESTAMP< as.POSIXct('2019-06-06 09:30:00'),]  
  inflow_pre <- inflow_pre %>% mutate(inflow1 = (psi_corr )*0.70324961490205 - 0.1603375 + 0.03048) %>% 
    mutate(flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (inflow1 ^ 1.5) * 35.3147)) %>% 
    mutate(flow_cms = flow_cfs*0.028316847   )%>% 
    select(TIMESTAMP, flow_cms, wtr_weir) 
  
  inflow_post <- inflow[inflow$TIMESTAMP > as.POSIXct('2019-06-07 00:00:00'),]  
  inflow_post <- inflow_post %>%  mutate(head = (0.149*psi_corr)/0.293) %>% 
    mutate(flow_cms = 2.391* (head^2.5)) %>% 
    select(TIMESTAMP, flow_cms, wtr_weir)
  inflow <- rbind(inflow_pre, inflow_post)
  
  inflow = aggregate(list(flow_cms = inflow$flow_cms, wtr_weir = inflow$wtr_weir), list(TIMESTAMP = cut(inflow$TIMESTAMP, "1 day")), mean);
  inflow$TIMESTAMP <- as.POSIXct(inflow$TIMESTAMP, format = '%Y-%m-%d')
  
  #Convert flow from the new pressure transducer to the old pressure transducers, which compares better to manual measurements using salt slugs.
  inflow$flow_cms <-   -0.004732 + 0.713454*inflow$flow_cms #+ rnorm(1, 0, 0.002816)
  
  
  inflow_clean <- rbind(hist_inflow, inflow)
  
  inflow_clean$TIMESTAMP <- as.Date(inflow_clean$TIMESTAMP, format = '%Y-%m-%d')
  
  names(inflow_clean) <- c("time", "FLOW", "TEMP")
  #checks for NANs in flow. If NAN, assigns prior days flow. 
  # for(i in 2:nrow(inflow_clean)){
  #    inflow_clean$flow_cms[i] <- ifelse(is.na(inflow_clean$flow_cms[i]), inflow_clean$flow_cms[i-1], inflow_clean$flow_cms[i])
  
  #}
  
  
  write_csv(inflow_clean, cleaned_inflow_file)  
  
  
}


