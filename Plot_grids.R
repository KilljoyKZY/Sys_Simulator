# setwd('C:/Users/kouz/Desktop/R_projects/Divvy_project/Uniform_Code_all_cities') # set the working directory to be where you store the 'helper_functions.R'
source('/depot/cai161/kouz/Uniform_Code/helper_functions.R')
source('/depot/cai161/kouz/Uniform_Code/simulation_generate_trips_func.R')
source('/depot/cai161/kouz/Uniform_Code/helper_time.R')

# city <- 'New York'
# city <- 'Los Angeles'
# city <- 'Chicago'
city <- 'Philadelphia'
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

base_route <- paste('/depot/cai161/kouz/',city,'_data/', sep = "")
fig_route <- paste(base_route,'figures/', sep = "")
if (!dir.exists(fig_route)){
  dir.create(fig_route)
}

png(file=paste(fig_route,'grids.png', sep = ""),
    width = 600, height = 600, res = 100)
plot(grid_with_sta)
points(sta_df$X, sta_df$Y, col='red', pch=16, cex=0.5)
dev.off()


