####### calculate outliers
clean_weibull_estimates <- function(data_weibull){
  # for the SE's
  data_weibull <- filter(data_weibull, data_weibull$Threshold_SE < 10)
  data_weibull <- filter(data_weibull, data_weibull$Slope_SE < 0.3)
  
  return(data_weibull)
}

