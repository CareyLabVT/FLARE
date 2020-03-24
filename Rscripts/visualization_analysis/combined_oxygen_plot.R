combined_oxygen_plot <- function(with_oxy, without_oxy, forecast_location, push_to_git){

  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  nc <- nc_open(with_oxy) 
  forecasted <- ncvar_get(nc, "forecasted")
  oxy <- ncvar_get(nc, "OXY_oxy")
  t <- ncvar_get(nc,'time')
  local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
  full_time_local_withoxygen <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = local_tzone)
  nc_close(nc)
  
  oxy_sss_9m_mean <- rep(NA, dim(oxy)[1])
  oxy_sss_9m_low95 <- rep(NA, dim(oxy)[1])
  oxy_sss_9m_up95 <- rep(NA, dim(oxy)[1])
  for(i in 1:dim(oxy)[1]){
    oxy_sss_9m_mean[i] <- mean(oxy[i, , 19])
    oxy_sss_9m_low95[i] <- quantile(oxy[i, , 19], 0.025)
    oxy_sss_9m_up95[i] <- quantile(oxy[i, , 19], 0.975)
  }
  
  nc <- nc_open(without_oxy) 
  oxy <- ncvar_get(nc, "OXY_oxy")
  t <- ncvar_get(nc,'time')
  local_tzone <- ncatt_get(nc, 0)$time_zone_of_simulation
  full_time_local_without_oxygen <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = local_tzone)
  
  
  oxy_9m_mean <- rep(NA, dim(oxy)[1])
  oxy_9m_low95 <- rep(NA, dim(oxy)[1])
  oxy_9m_up95 <- rep(NA, dim(oxy)[1])
  for(i in 1:dim(oxy)[1]){
    oxy_9m_mean[i] <- mean(oxy[i, , 19])
    oxy_9m_low95[i] <- quantile(oxy[i, , 19], 0.025)
    oxy_9m_up95[i] <- quantile(oxy[i, , 19], 0.975)
  }
  
  nc_close(nc)
  
  oxy_status = c(rep("on",length(full_time_local_withoxygen)),rep("on",length(full_time_local_withoxygen)), rep("on",length(full_time_local_withoxygen)),
                 rep("off",length(full_time_local_without_oxygen)),rep("off",length(full_time_local_without_oxygen)),rep("off",length(full_time_local_without_oxygen)))
  
  d <- tibble(Oxygen = oxy_status, 
              day = c(rep(full_time_local_withoxygen, 3), rep(full_time_local_without_oxygen, 3)),
              temperature = c(oxy_sss_9m_mean, oxy_sss_9m_low95, oxy_sss_9m_up95, oxy_9m_mean, oxy_9m_low95, oxy_9m_up95), 
              Simulation = c(rep("mean", length(full_time_local_withoxygen)), rep("lower 95% CI", length(full_time_local_withoxygen)), rep("upper 95% CI", length(full_time_local_withoxygen)),
                             rep("mean", length(full_time_local_without_oxygen)), rep("lower 95% CI", length(full_time_local_without_oxygen)), rep("upper 95% CI", length(full_time_local_without_oxygen))))
  
  d$Simulation <- factor(d$Simulation, levels = c("mean", "lower 95% CI", "upper 95% CI"))
  
  p <- ggplot() +
    geom_line(data = d, aes(x = day, y = temperature, color = Oxygen, linetype = Simulation)) + 
    scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
    scale_x_datetime(limits = c(full_time_local_without_oxygen[2],last(full_time_local_without_oxygen)), breaks=seq((full_time_local_without_oxygen[2]),last(full_time_local_without_oxygen),"2 days")) +
    ylim(0, 500) +
    #xlim(first(full_time_local_without_oxygen) - (days(2)), last(full_time_local_without_oxygen)) +
    labs(x = "Day", y = expression("mmol m"^-3), title = "FCR 9 m: Oxygen forecast") +
    geom_vline(data = d, aes(xintercept = day[1])) +
    #annotate("text", x = full_time_local_without_oxygen[1], y = 450, label = "Past") +
    #annotate("text", x = full_time_local_without_oxygen[12], y = 450, label = "Future") +
    theme_bw() +
    theme(plot.margin = unit(c(0.1,0.2,0.1,0.1), "in"), legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0(forecast_location,"/oxygen9m_forecast.png"), device = "png",plot = p, width = 6.0, height = 4.5, units = "in",dpi = 150)
  
  if(push_to_git){
    setwd(forecast_location)
    system(paste0('git add ', paste0(forecast_location,"/oxygen9m_forecast.png")))
    system('git commit -m "oxygen plot"')
    system('git push')
  }
}


