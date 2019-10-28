
ConvertTimeZone <- function(in_time, city=NULL){
  ori_t <- as.POSIXct(in_time, tz='America/New_York')
  if (city %in% c('Los Angeles')){
    attributes(ori_t)$tzone <- 'America/Los_Angeles'
  }
  if (city %in% c('Chicago')){
    attributes(ori_t)$tzone <- 'America/Chicago'
  }
  return(ori_t)
}


ConvertDurationUnit <- function(starttime, endtime, round = TRUE, units='mins'){
  starttime <- as.POSIXct(starttime)
  endtime <- as.POSIXct(endtime)
  t_diff <- difftime(endtime, starttime, units = units)
  if (round == TRUE){
    t_diff <- round(t_diff)
  }
  return(as.numeric(t_diff))
}


