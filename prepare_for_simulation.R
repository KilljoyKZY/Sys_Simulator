# setwd('C:/Users/kouz/Desktop/R_projects/Divvy_project/Uniform_Code_all_cities') # set the working directory to be where you store the 'helper_functions.R'
source('/depot/cai161/kouz/Uniform_Code/helper_functions.R')
source('/depot/cai161/kouz/Uniform_Code/simulation_generate_trips_func.R')
source('/depot/cai161/kouz/Uniform_Code/helper_time.R')

# city <- 'New York'
# city <- 'Los Angeles'
city <- 'Chicago'
# city <- 'Philadelphia'
model_month = '09'
weekday_idx = TRUE

if (city == 'New York'){
  trips_y <- read.csv(paste('/depot/cai161/data/Bike_Share_Data/bike_share_trip_data/preprocessed_trips/',
                            city,'/',city,'_trips_201809.csv', sep = ""),
                      header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
} else {
  trips_y <- read.csv(paste('/depot/cai161/data/Bike_Share_Data/bike_share_trip_data/preprocessed_trips/',
                            city,'/',city,'_trips_2018.csv', sep = ""),
                      header=TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
}

trips_y <- AddTimeColToTrips(trips_y)

trips_y$trip_duration <- ConvertDurationUnit(trips_y$start_time, trips_y$end_time)

m <- '9'
date_pattern <- paste("2018-0",m, sep = "")
if (nchar(m)==2){
  date_pattern <- paste("2018-",m, sep = "")
}

trip_month <- trips_y[grep(date_pattern, trips_y$start_time),]

if ('start_station_lat' %in% names(trips_y)){ # "New York", "Los Angeles" have lat lon
  station <- FindStationLocFromTrips(trips_y)
} else {
  station <- read.csv(paste('/depot/cai161/kouz/',city,'_data/',city, '_sta_loc.csv', sep=''), 
                      header=TRUE, stringsAsFactors = FALSE)
}

########################### --- Create Grids -------##############################################################
station <- station[complete.cases(station),]
created_grid_ls <- CreateGrid(station) # projected_sys <- '+proj=lcc +lat_1=-14.60181 +lon_0=-179.2311 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0'

sta_df <- created_grid_ls$sta_df # dataframe for the stations (lat lon and projected location of stations)
grid_df <- created_grid_ls$grid_df # dataframe for the grids (lat lon and projected location of stations),
# note this all the grids for the created rectangula, some grids do not have stations in it.
gridpolygon <- created_grid_ls$gridpolygon # the polygon for all the grids (rectangula)
grid_with_sta <- created_grid_ls$grid_with_sta # the polygon for the grids with stations (rectangula)

geo_path <- paste('/depot/cai161/kouz/',city,'_data/grid_polygon', sep='')
if (!dir.exists(geo_path)){
  dir.create(geo_path)
}
writeOGR(obj=gridpolygon, dsn = geo_path, layer='gridpolygon', driver = 'ESRI Shapefile')

geo_path_sta <- paste('/depot/cai161/kouz/',city,'_data/grid_polygon_with_sta', sep='')
if (!dir.exists(geo_path_sta)){
  dir.create(geo_path_sta)
}
writeOGR(obj=grid_with_sta, dsn = geo_path_sta, layer='grid_with_sta', driver = 'ESRI Shapefile')

sta_in_month <- FindUniqueStations(trip_month)
# plot(grid_with_sta)
sta_df_m <- sta_df[sta_df$StationID %in% sta_in_month,]
in_grid_ls <- unique(sta_df_m$grid_id)
grid_df <- grid_df[grid_df$grid_id %in% in_grid_ls,]
grid_df$grid_id_original <- grid_df$grid_id
grid_df$grid_id <- c(1:nrow(grid_df))
grid_df$grid_id2 <- c(1:nrow(grid_df))

sta_df_m <- left_join(sta_df_m, grid_df[,c('grid_id2', 'grid_id_original')], by=c('grid_id'='grid_id_original'))
sta_df_m$grid_id <- sta_df_m$grid_id2
sta_df_m$grid_id2 <- NULL
grid_df$grid_id2 <- NULL

write.csv(sta_df_m, 
          file =paste('/depot/cai161/kouz/',city,'_data/',city,'_sta_df.csv',
                      sep = ""),row.names=FALSE)
write.csv(grid_df, 
          file =paste('/depot/cai161/kouz/',city,'_data/',city,'_grid_df.csv',
                      sep = ""),row.names=FALSE)

## Calculate true demand and save
CalTrueRate(trips_y = trips_y, city=city, month_list = c('9'))

grid_corr_count <- GenerateGridCorr(sta_df_m, trips_y, model_month = model_month, weekday_idx = weekday_idx)
write.csv(grid_corr_count, file =paste('/depot/cai161/kouz/',city,'_data/',city,'_grid_corr_count_',model_month,'.csv', sep = ""),row.names=FALSE)

dist_to_depot <- DistanceToDepot(city=city, grid_df=grid_df)
write.csv(dist_to_depot,
          file =paste('/depot/cai161/kouz/',city,'_data/',city,'_grid_manhattan_duration_to_depot.csv', sep = ""),row.names=FALSE)
# Station initial status
selected_date = '2018-09-17'
sta_d_initial <- StationInitialStatus(city, m='9', selected_date)
write.csv(sta_d_initial,
          file =paste('/depot/cai161/kouz/',city,'_data/',city,
                      '_station_initial_status_',selected_date,'.csv', sep = ""),row.names=FALSE)

dist_mat_manh <- GridDistMatrix(grid_df)  ## distance matrix
write.csv(dist_mat_manh, 
          file =paste('/depot/cai161/kouz/',city,'_data/',city,'_grid_distance_matrix_manhattan.csv',
                      sep = ""),row.names=FALSE)

## Generate the simulated trips:
GenerateSimuTrips(city, sta_df_m, trips_y, model_month = '09', weekday_idx = TRUE, true_or_obs = 'true')



