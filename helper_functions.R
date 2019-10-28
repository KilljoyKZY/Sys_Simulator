source('/depot/cai161/kouz/Uniform_Code/helper_time.R')

# station should be a dataframe with station ID, Lat, and Lon
CreateGrid <- function(station, grid_edge=3000, edge_buffer = 0.5, projected_sys = "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"){
  # load package
  packages_to_load <- c('gmodels', 'sp', 'rgdal', 'raster', 'rgeos')
  
  for (p in packages_to_load){
    library(p, lib.loc = '/home/kouz/Rlibs', character.only = TRUE)
  }
  library('dplyr')
  
  names(station) <- c("StationID","Latitude","Longitude")
  ####################### transform the station latlon to coord
  sta_coord <- station[,c("StationID","Longitude","Latitude")]
  coordinates(sta_coord) <- c("Longitude","Latitude")
  proj4string(sta_coord) <- CRS("+proj=longlat +datum=NAD83") ########## WGS84, Projection need to be modified for different cities
  
  sta_coord <- spTransform(sta_coord,CRS(projected_sys))
  sta_df <- as.data.frame(sta_coord)
  names(sta_df) <- c("StationID", "X", "Y")
  
  sta_df <- left_join(sta_df, station, by = 'StationID')
  
  grid <- raster(xmn=min(sta_df$X)-grid_edge*edge_buffer, xmx=max(sta_df$X)+grid_edge*edge_buffer,
                 ymn=min(sta_df$Y)-grid_edge*edge_buffer, ymx=max(sta_df$Y)+grid_edge*edge_buffer)
  
  # Choose its resolution. 
  res(grid) <- grid_edge
  # Make the grid have the same coordinate reference system (CRS) as the shapefile.
  proj4string(grid)<-projected_sys
  # Transform this raster into a polygon and you will have a grid, but without your own shapefile.
  gridpolygon <- rasterToPolygons(grid)
  # Assign ID to the Grid polygon
  gridpolygon <- spChFIDs(gridpolygon, as.character(c(1:nrow(gridpolygon))))
  #### intersect the grid and station
  grid_sta <- raster::intersect(gridpolygon, sta_coord)
  # use raster::intersect() if the intersect is masked
  
  grid_coord_df <- data.frame(coordinates(gridpolygon))
  colnames(grid_coord_df) <- c('X','Y')
  grid_coord_df$grid_id <- c(1:nrow(grid_coord_df))
  grid_coord_df <- grid_coord_df[,c(3,1,2)]
  
  geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  grid_latlon <- spTransform(gridpolygon,CRS(geo.prj))
  grid_latlon_df <- data.frame(coordinates(grid_latlon))
  colnames(grid_latlon_df) <- c("Longitude","Latitude")
  grid_latlon_df$grid_id <- c(1:nrow(grid_latlon_df))
  
  grid_df <- left_join(grid_coord_df, grid_latlon_df, by = 'grid_id')
  
  # add the corresponding grid to station
  sta_df$grid_id <- over(geometry(sta_coord), geometry(gridpolygon)) 
  result_list <- list('sta_df' = sta_df, 'grid_df' = grid_df, 'gridpolygon' = gridpolygon, 'grid_with_sta' = grid_sta)
  return(result_list)
}



AddTimeColToTrips <- function(trips_y){
  library('timeDate', lib.loc = '/home/kouz/Rlibs')
  trips_y$month <- apply(trips_y, 1, function(x) substr(x['start_time'],6,7))
  trips_y$date <- as.Date(trips_y$start_time)
  trips_y$weekday <- isWeekday(trips_y$date, wday = 1:5)
  trips_y$start_hour <- substr(trips_y$start_time, 12, 13)
  trips_y$end_hour <- substr(trips_y$end_time, 12, 13)
  trips_y <- trips_y[complete.cases(trips_y$start_station_id),]
  trips_y <- trips_y[complete.cases(trips_y$end_station_id),]
  return(trips_y)
}

#### Keep only the numbers of the ID (remove strings in)
KeepIDNum <- function(x){
  return(as.numeric(tail(strsplit(x, '_')[[1]], 1)))
}


CalTrueRate <- function(trips_y, city, month_list=c('9')){
  ############## Calculate the true rate
  for (m in month_list){
    print(paste('processing month:', m))
    sta_d <- read.csv(file = paste('/depot/cai161/kouz/', city, '_data/',city,'_station_merge_',m,'.csv',
                                   sep = ""), header=TRUE, stringsAsFactors = FALSE)
    if (city %in% c('Chicago')){
      sta_d <- sta_d[,c("id", "availableBikes","availableDocks", "Download.Time")]
      names(sta_d) <- c("sta_id", "availableBikes","availableDocks", "Download.Time")
    }
    if (city %in% c('New York', 'Los Angeles', 'Philadelphia')){
      sta_d <- sta_d[,c("station_id", "num_bikes_available","num_docks_available","Download.Time")]
      names(sta_d) <- c("sta_id", "availableBikes","availableDocks", "Download.Time")
    }
    
    if (city %in% c('Los Angeles')){
      sta_d$sta_id <- unlist(lapply(sta_d$sta_id, KeepIDNum))
    }
    if (city == 'Philadelphia'){
      sta_d$sta_id <- as.numeric(substr(sta_d$sta_id, 15, 18))
    }
    
    date_pattern <- paste("2018-0",m, sep = "")
    if (nchar(m)==2){
      date_pattern <- paste("2018-",m, sep = "")
    }
    
    trip_month <- trips_y[grep(date_pattern, trips_y$start_time),]
    
    trip_weekday <- trip_month[trip_month$weekday==TRUE,]
    trip_weekend <- trip_month[trip_month$weekday==FALSE,]
    
    weekday_start_ct <- aggregate(trip_weekday$start_time, list(sta_id=trip_weekday$start_station_id, hour=trip_weekday$start_hour),length)
    weekday_end_ct <- aggregate(trip_weekday$start_time, list(sta_id=trip_weekday$end_station_id, hour=trip_weekday$end_hour),length)
    weekend_start_ct <- aggregate(trip_weekend$start_time, list(sta_id=trip_weekend$start_station_id, hour=trip_weekend$start_hour),length)
    weekend_end_ct <- aggregate(trip_weekend$start_time, list(sta_id=trip_weekend$end_station_id, hour=trip_weekend$end_hour),length)
    
    num_weekday <- length(unique(trip_weekday$date))
    num_weekend <- length(unique(trip_weekend$date))
    
    # deal with the station data
    sta_d$has_bike <- ifelse(sta_d$availableBikes==0, 0, 1)
    sta_d$has_dock <- ifelse(sta_d$availableDocks==0, 0, 1)
    sta_d$Download.Time <- ConvertTimeZone(sta_d$Download.Time, city = city)
    sta_d$date <- substr(sta_d$Download.Time, 1, 10)
    sta_d$date <- as.Date(sta_d$date)
    
    sta_d$weekday <- isWeekday(sta_d$date, wday = 1:5)
    sta_d$hour <- substr(sta_d$Download.Time, 12, 13)
    sta_d$row_id <- c(1:nrow(sta_d))
    
    sta_wkd <- sta_d[sta_d$weekday==TRUE,]
    sta_wke <- sta_d[sta_d$weekday==FALSE,]
    
    sta_wkd_w_ct <- aggregate(sta_wkd$row_id,list(sta_id=sta_wkd$sta_id, hour=sta_wkd$hour),length)
    sta_wkd_w_avai <- aggregate(sta_wkd$has_bike,list(sta_id=sta_wkd$sta_id, hour=sta_wkd$hour),sum)
    sta_wke_w_ct <- aggregate(sta_wke$row_id,list(sta_id=sta_wke$sta_id, hour=sta_wke$hour),length)
    sta_wke_w_avai <- aggregate(sta_wke$has_bike,list(sta_id=sta_wke$sta_id, hour=sta_wke$hour),sum)
    
    # Weekday withdraw
    colnames(sta_wkd_w_ct) <- c("sta_id", "hour","num_t_slots")
    colnames(sta_wkd_w_avai) <- c("sta_id", "hour","avai_t_slots")
    df_wkd_w <- merge(sta_wkd_w_ct,sta_wkd_w_avai, by=c('sta_id','hour'))
    
    colnames(sta_wke_w_ct) <- c("sta_id", "hour","num_t_slots")
    colnames(sta_wke_w_avai) <- c("sta_id", "hour","avai_t_slots")
    df_wke_w <- merge(sta_wke_w_ct,sta_wke_w_avai, by=c('sta_id','hour'))
    
    colnames(weekday_start_ct) <- c("sta_id", "hour","monthly_trip_count")
    df_wkd_w <- merge(df_wkd_w, weekday_start_ct, by=c('sta_id','hour'), all.x = TRUE)
    df_wkd_w$monthly_trip_count[is.na(df_wkd_w$monthly_trip_count)] <- 0
    df_wkd_w$hourly_rate <- ifelse(df_wkd_w$avai_t_slots==0, 
                                   df_wkd_w$monthly_trip_count/num_weekday*df_wkd_w$num_t_slots/1,
                                   df_wkd_w$monthly_trip_count/num_weekday*df_wkd_w$num_t_slots/df_wkd_w$avai_t_slots)
    df_wkd_w$obs_hourly_rate <- df_wkd_w$monthly_trip_count/num_weekday
    write.csv(df_wkd_w, file = paste('/depot/cai161/kouz/',city,'_data/',city,'_wkd_w_hr_rate_',m,'.csv', sep = ""),row.names=FALSE)
    
    # Weekend withdraw
    colnames(weekend_start_ct) <- c("sta_id", "hour","monthly_trip_count")
    df_wke_w <- merge(df_wke_w, weekend_start_ct, by=c('sta_id','hour'), all.x = TRUE)
    df_wke_w$monthly_trip_count[is.na(df_wke_w$monthly_trip_count)] <- 0
    df_wke_w$hourly_rate <- ifelse(df_wke_w$avai_t_slots==0, 
                                   df_wke_w$monthly_trip_count/num_weekend*df_wke_w$num_t_slots/1,
                                   df_wke_w$monthly_trip_count/num_weekend*df_wke_w$num_t_slots/df_wke_w$avai_t_slots)
    df_wke_w$obs_hourly_rate <- df_wke_w$monthly_trip_count/num_weekend
    write.csv(df_wke_w, file =paste('/depot/cai161/kouz/',city,'_data/',city,'_wke_w_hr_rate_',m,'.csv', sep = ""),row.names=FALSE)
  }
}
  

  
FindStationLocFromTrips <- function(trips){
  ####### extract and save the lat-lon of stations
  d_sub = trips[,c('start_station_id', 'start_station_lat','start_station_lon')]
  d_sub2 = trips[,c('end_station_id', 'end_station_lat','end_station_lon')]
  names(d_sub) <- c('station_id', 'lat', 'lon')
  names(d_sub2) <- c('station_id', 'lat', 'lon')
  d_sub = rbind(d_sub, d_sub2)
  sta_loc = d_sub[!duplicated(d_sub$station_id),]
  return(sta_loc)
}

FindUniqueStations <- function(trips){
  ####### extract and save the lat-lon of stations
  d_sub = trips[,c('start_station_id')]
  d_sub2 = trips[,c('end_station_id')]
  # names(d_sub) <- c('station_id')
  # names(d_sub2) <- c('station_id')
  d_sub = c(d_sub, d_sub2)
  sta_loc = d_sub[!duplicated(d_sub)]
  sta_loc = sta_loc[complete.cases(sta_loc)]
  return(sta_loc)
}



GenerateGridCorr <- function(sta_df, trips_y, model_month = '09', weekday_idx = TRUE){
  ########################## Generate grid correlation --------------------------------------
  trip_month <- trips_y[trips_y$month==model_month,]
  trip_month <- trips_y[trips_y$weekday==weekday_idx,]
  
  grid_match <- sta_df[,c('StationID', 'grid_id')]
  names(grid_match) <- c('start_station_id', 'start_grid')
  
  trip_month <- left_join(trip_month, grid_match, by='start_station_id')
  names(grid_match) <- c('end_station_id', 'end_grid')
  trip_month <- left_join(trip_month, grid_match, by='end_station_id')
  
  grid_corr_count <- aggregate(trip_month$start_time, list(start_grid=trip_month$start_grid,
                                                        end_grid=trip_month$end_grid,
                                                        start_hour=trip_month$start_hour),length)
  names(grid_corr_count)[4] <- 'count'
  return(grid_corr_count)
}

# New York depot location:  
# LA depot location:  source 

### Calcuate the distance from grid centers to depot
manhattan.dist<-function(lon1, lat1, lon2, lat2){
  v_dist<-distm(c(lon1, lat1), c(lon1, lat2), fun = distHaversine)
  h_dist<-distm(c(lon1, lat2), c(lon2, lat2), fun = distHaversine)
  return(v_dist+h_dist)
}

DistanceToDepot <- function(city, grid_df){
  library(codep)
  library(geosphere)
  if (city == 'Chicago'){
    depot_loc <- c(41.890082, -87.658458) 
  }
  if (city == 'Philadelphia'){
    depot_loc <- c(39.975209, -75.144636) # 1330 N 5th St, Philadelphia, PA 19122  Bicycle Transit Systems
  }
  if (city == 'New York'){
    depot_loc <- c(40.669695, -73.995103) # source https://patch.com/new-york/gowanus/citi-bike-opens-gowanus-operations-facility
  }
  if (city == 'Los Angeles'){
    depot_loc <- c(34.058895, -118.236342) # source https://thesource.metro.net/2017/11/15/new-metro-bike-hub-at-union-station-is-now-open/
  }
  
  grid_df$dist_to_depot <- 0
  
  for (i in 1:nrow(grid_df)){
    lon2 <- grid_df$Longitude[i]
    lat2 <- grid_df$Latitude[i]
    grid_df$dist_to_depot[i] <- manhattan.dist(depot_loc[2],depot_loc[1],lon2, lat2)
  }
  
  grid_df$duration_to_depot <- grid_df$dist_to_depot/1609.344/25*60
  return(grid_df)
}


### Find station initial status
StationInitialStatus <- function(city, m='9', selected_date = '2018-09-17'){
  print(paste('processing month:', m))
  sta_df <- read.csv(paste('/depot/cai161/kouz/', city, '_data/', city, '_sta_df.csv',sep = ""), stringsAsFactors = FALSE)
  sta_d <- read.csv(file = paste('/depot/cai161/kouz/', city, '_data/',city,'_station_merge_',9,'.csv',
                                 sep = ""), header=TRUE, stringsAsFactors = FALSE)
  if (city %in% c('Chicago')){
    sta_d <- sta_d[,c("id", "availableBikes","availableDocks", "Download.Time")]
    names(sta_d) <- c("sta_id", "availableBikes","availableDocks", "Download.Time")
  }
  if (city %in% c('New York', 'Los Angeles', 'Philadelphia')){
    sta_d <- sta_d[,c("station_id", "num_bikes_available","num_docks_available","Download.Time")]
    names(sta_d) <- c("sta_id", "availableBikes","availableDocks", "Download.Time")
  }
  sta_d$Download.Time <- ConvertTimeZone(sta_d$Download.Time, city = city)
  sta_d$date <- substr(sta_d$Download.Time, 1, 10)
  sta_d_1 <- sta_d[sta_d$date==selected_date,]
  sta_d_initial <- sta_d_1[sta_d_1$Download.Time==min(sta_d_1$Download.Time),] 
  
  if (city %in% c('Los Angeles')){
    sta_d_initial$sta_id <- unlist(lapply(sta_d_initial$sta_id, KeepIDNum))
  }
  if (city == 'Philadelphia'){
    sta_d_initial$sta_id <- as.numeric(substr(sta_d_initial$sta_id, 15, 18))
  }
  
  other_sta <- sta_df$StationID[!sta_df$StationID %in% sta_d_initial$sta_id]
  
  sta_d_initial$totalDocks <- sta_d_initial$availableBikes + sta_d_initial$availableDocks
  sta_d_initial <- sta_d_initial[,c("sta_id","availableBikes", "totalDocks")]
  names(sta_d_initial) <- c("sta_id", "Bikes_fina", "Docks")
  for (s_id in other_sta){
    sta_d_initial[nrow(sta_d_initial)+1,] <- c(s_id, 0, 15)
  }
  
  return(sta_d_initial)
}


###### generate grid distance matrix
GridDistMatrix <- function(grid_df){
  # dist_mat <- matrix(0L, nrow = nrow(grid_df), ncol = nrow(grid_df))
  dist_mat_manh <- matrix(0L, nrow = nrow(grid_df), ncol = nrow(grid_df))
  
  for (i in 1:nrow(grid_df)){
    # print(i)
    for (j in 1:nrow(grid_df)){
      xi <- grid_df$X[grid_df$grid_id==i]
      yi <- grid_df$Y[grid_df$grid_id==i]
      xj <- grid_df$X[grid_df$grid_id==j]
      yj <- grid_df$Y[grid_df$grid_id==j]
      # dist_mat[i,j] <- sqrt(((xi- xj)^2 + (yi - yj)^2))
      dist_mat_manh[i,j] <- (abs(xi- xj) + abs(yi - yj))
    }
  }
  return(dist_mat_manh)
}


