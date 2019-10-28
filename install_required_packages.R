
packages_to_install <- c('googleway', 'gmodels', 'sp', 'rgdal', 'dplyr',
                         'raster', 'rgeos', 'timeDate')


for (p in packages_to_install){
  install.packages(p, repos='http://cran.r-project.org', lib = '/home/kouz/Rlibs')
}

# Load the installed package:
# library('iterators', lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
