## Load in functions
source('/depot/cai161/kouz/Uniform_Code/simul_gene_bikes_station_based_func.R')
source('/depot/cai161/kouz/Uniform_Code/simul_gene_bikes_dockless_func.R')
source('/depot/cai161/kouz/Uniform_Code/simul_gene_bikes_dockless_random_park_func.R')

# city <- 'New York'
city <- 'Los Angeles'
# city <- 'Philadelphia'

simul_gene_bikes_station_based(city = city)
simul_gene_bikes_dockless(city = city)
simul_gene_bikes_dockless_random_park(city = city)
