simul_gene_bikes_dockless <- function(city, env='local'){
  # install.packages('units', dependencies = TRUE, repos='http://cran.r-project.org', lib = "/depot/cai161/kouz/Rlibs/")
  # install.packages('sf', dependencies = TRUE, repos='http://cran.r-project.org', lib = "/depot/cai161/kouz/Rlibs/")
  # install.packages('timeDate', dependencies = TRUE, repos='http://cran.r-project.org', lib = "/depot/cai161/kouz/Rlibs/")
  # install.packages('caret',dependencies = TRUE, repos='http://cran.r-project.org', lib = "/depot/cai161/kouz/Rlibs/")
  # install.packages('dplyr',dependencies = TRUE, repos='http://cran.r-project.org', lib = "/depot/cai161/kouz/Rlibs/")
  # install.packages('lubridate',dependencies = TRUE, repos='http://cran.r-project.org', lib = "/depot/cai161/kouz/Rlibs/")
  # install.packages('fitdistrplus',dependencies = TRUE, repos='http://cran.r-project.org', lib = "/depot/cai161/kouz/Rlibs/")
  # 
  # 
  # library(sf, lib.loc = "/depot/cai161/kouz/Rlibs/")
  # library(timeDate, lib.loc = "/depot/cai161/kouz/Rlibs/")
  # library(caret, lib.loc = "/depot/cai161/kouz/Rlibs/")
  # library(fitdistrplus, lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
  
  if (env=='local'){
    library(dplyr) #, lib.loc = "/depot/cai161/kouz/Rlibs/")
    library(lubridate) #, lib.loc = "/depot/cai161/kouz/Rlibs/")
  } else {
    library(dplyr, lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
    library(lubridate, lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
  }
  
  library('iterators', lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
  library('foreach', lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
  library('doMC', lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
  
  # loop through all scenarios
  base_route <- paste('/depot/cai161/kouz/',city,'_data/', sep = "")
  
  scenario <- 'own'
  model_month <- '09'
  true_or_obs <- 'true'
  weekday_idx <- TRUE
  
  sys_type <- 'dockless'
  
  if (!dir.exists(paste(base_route, '/output/', sys_type, sep = ""))){
    dir.create(paste(base_route, '/output/', sys_type, sep = ""))
  }
  
  truck_speed <- 23.7 # mile/hr
  truck_cap <- 25
  grid_cap_rate <- 1.5
  initial_bike_cap <- 10
  bike_load_unit_time <- 1/2
  rack_ratio <- 1/2
  basic_stats <- read.csv(paste(base_route, city,
                                '_basic_stats.csv',sep = ""), stringsAsFactors = FALSE)
  bike_speed <- basic_stats$avg_bike_speed[1] #feet/min 
  
  # base_route <- "/depot/cai161/kouz/Divvy_data/"
  # base_route <- "/depot/cai161/kouz/Phi_data/Phi_"
  # 
  dist_to_depot <- read.csv(paste(base_route, city, "_grid_manhattan_duration_to_depot.csv",sep = ""), stringsAsFactors = FALSE)
  
  # dist_to_depot$duration_to_depot <- round(dist_to_depot$dist_to_depot/1609.344/truck_speed*60)
  
  dist_mat <- read.csv(paste(base_route,city, '_grid_distance_matrix_manhattan.csv',sep = ""), stringsAsFactors = FALSE)
  dist_mat$X <- NULL
  dist_mat <- data.matrix(dist_mat)
  
  features <- read.csv(paste(base_route, city ,'_grid_df.csv',sep = ""), stringsAsFactors = FALSE)
  sta_df <- read.csv(paste(base_route, city, '_sta_df.csv',sep = ""), stringsAsFactors = FALSE)
  names(sta_df)[1] <- 'sta_id'
  
  selected_date <- '2018-09-17'
  sta_d_initial <- read.csv(file=paste(base_route, city, '_station_initial_status_',selected_date,'.csv',sep = ""),stringsAsFactors = FALSE)
  
  sta_d_initial <- left_join(sta_d_initial, sta_df[c('sta_id', 'grid_id')], by='sta_id')
  sta_d_initial <- sta_d_initial[complete.cases(sta_d_initial$grid_id),]
  
  grid_bike <- aggregate(sta_d_initial$Bikes_fina, list(grid_id=sta_d_initial$grid_id), sum)
  names(grid_bike)[2] <- 'Bikes_fina'
  grid_dock <- aggregate(sta_d_initial$Docks, list(grid_id=sta_d_initial$grid_id), sum)
  names(grid_dock)[2] <- 'Docks'
  grid_sta <- aggregate(sta_d_initial$sta_id, list(grid_id=sta_d_initial$grid_id), length)
  names(grid_sta)[2] <- 'X1_stns'
  
  scen_df <- left_join(grid_sta, grid_bike, by='grid_id')
  scen_df <- left_join(scen_df, grid_dock, by='grid_id')
  
  rm(grid_bike, grid_dock, grid_sta)
  
  # colnames(scen_df)[1] <- 'grid_id'
  # scen_df <- left_join(scen_df, features[,c('grid_id','num_rack')], by='grid_id')
  # scen_df$num_rack <- round(scen_df$num_rack*rack_ratio)
  
  # scen_df <- left_join(features[,c('grid_id','num_rack')], scen_df, by='grid_id')
  
  scen_df$X1_stns[is.na(scen_df$X1_stns)] <- 0
  scen_df$Bikes_fina[is.na(scen_df$Bikes_fina)] <- 0
  scen_df$Docks[is.na(scen_df$Docks)] <- 0
  
  scen_df$num_rack <- 0
  scen_df <- scen_df[order(scen_df$grid_id),]
  
  #### create a dataframe to store the bike record--------------------------------
  bike_df <- data.frame(bike_id=numeric(),
                        initial_grid=numeric(),
                        bike_status=character(), # "dock", "rack", "street" "trip"
                        current_grid=numeric(),
                        trip_count=numeric(),
                        stringsAsFactors = FALSE)
  bike_df_backup <- bike_df
  
  ################################ simulate the generated trips
  
  # scen_df$dock_rack <- scen_df$Docks+scen_df$num_rack
  scen_df$avai_bike <- 0
  scen_df$avai_dock <- scen_df$Docks
  
  scen_df$avai_rack <- scen_df$num_rack
  
  scen_df$bike_street <- 0
  
  scen_df$b_in_dock <- scen_df$Docks - scen_df$avai_dock
  scen_df$b_in_rack <- scen_df$num_rack - scen_df$avai_rack
  
  scen_df$max_dock <- scen_df$avai_dock
  scen_df$max_rack <- scen_df$avai_rack
  scen_df$max_bike_street <- scen_df$bike_street
  scen_df$fail_trip <- 0
  
  # scen_df$grid_id_original <- scen_df$grid_id
  # scen_df$grid_id <- c(1:nrow(scen_df))
  
  scen_df_backup <- scen_df
  # current_grid_id <- scen_df_backup$grid_id[scen_df_backup$Docks>0]
  
  
  
  # sim_trips_df <- read.csv(paste(base_route, 'own/output/simulated_trips_own_',model_month,'_',
  #                                ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  # trip_agg <- aggregate(sim_trips_df$trip_id, list(grid_id=sim_trips_df$start_grid_id), length)
  
  registerDoMC(10)
  # now simulate for 10 times
  # for (n_sim in 1:10){
  foreach(n_sim = 1:10) %dopar% {
    # print(n_sim)
    bike_df <- bike_df_backup
    bike_df_reb <- bike_df
    
    scen_df <- scen_df_backup
    ## with rebalance record
    scen_df_reb <- scen_df
    
    ################ read in previous simulated trips -------------------------------
    sim_trips_df <- read.csv(paste(base_route, '/output/simulated_trips_own_',model_month,'_',
                                   ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
    
    # add ending time for the sim_trips
    for (i in 1:nrow(sim_trips_df)){
      # if (i%%1000==0){
      #   print(i)
      # }
      trip_id <- sim_trips_df$trip_id[i]
      start_grid <- sim_trips_df$start_grid_id[i]
      end_grid <- sim_trips_df$end_grid_id[i]
      start_h <- sim_trips_df$hour[i]
      start_m <- sim_trips_df$min[i]
      m_sum <- start_m + sim_trips_df$duration[i]
      sim_trips_df$end_hour[i] <- start_h + floor(m_sum/60)
      sim_trips_df$end_min[i] <- m_sum - floor(m_sum/60)*60
    }
    
    sim_trips_df$end_min <- ifelse(sim_trips_df$end_hour>23, 59, sim_trips_df$end_min)
    sim_trips_df$end_hour <- ifelse(sim_trips_df$end_hour>23, 23, sim_trips_df$end_hour)
    
    rm(trip_id,start_grid,end_grid,start_h,start_m,m_sum)
    sim_trips_df$status <- 'pending'
    sim_trips_df$pickup_type <- 'none'
    sim_trips_df$parking_type <- 'none' # dock, rack, 
    sim_trips_df$bike_id <- NA
    sim_trips_df$reroute_count <- 0
    sim_trips_df$reroute_dist <- 0 ### Add this col to record the reroute distance when station is full
    
    sim_trips_df_reb <- sim_trips_df
    
    
    
    ##################################################################### Now consider rebalance ################################################
    
    #############################################################################################################################################
    scen_df_reb$reb_status <- "none"
    #### create a dataframe to store the rebalance record--------------------------------
    reb_df <- data.frame(reb_id=numeric(),
                         grid=numeric(),
                         reb_type=numeric(), # in 1 or out 2
                         job_status=numeric(), # requested 1, arrived 2, finished 3, returned 4
                         request_h=numeric(),
                         request_m=numeric(),
                         arrive_h=numeric(),
                         arrive_m=numeric(),
                         finish_h=numeric(),
                         finish_m=numeric(),
                         return_h=numeric(),
                         return_m=numeric(),
                         num_bike=numeric(),
                         truck_id=numeric(),
                         stringsAsFactors = FALSE)
    
    # create a df for truck
    truck_df <- data.frame(truck_id=numeric(),
                           status=numeric(), # idle 1, out 2
                           stringsAsFactors = FALSE)
    
    ############## simulation with rebalance
    for (h in 0:23){
      # print(h)
      for (m in 0:59){
        # print(m)
        start_sub <- sim_trips_df_reb[(sim_trips_df_reb$hour==h)&(sim_trips_df_reb$min==m),]
        if (nrow(start_sub)>0){
          for (i in 1:nrow(start_sub)){
            trip_id <- start_sub$trip_id[i]
            grid_id <- which(scen_df_reb$grid_id == start_sub$start_grid_id[i])[1]
            if (scen_df_reb$avai_bike[grid_id]>0){
              bike_rank <- sample(c(1:scen_df_reb$avai_bike[grid_id]),1)
              if (bike_rank<=scen_df_reb$b_in_dock[grid_id]){
                scen_df_reb$b_in_dock[grid_id] <- scen_df_reb$b_in_dock[grid_id] - 1
                scen_df_reb$avai_dock[grid_id] <- scen_df_reb$avai_dock[grid_id] + 1
                scen_df_reb$max_dock[grid_id] <- max(scen_df_reb$avai_dock[grid_id],scen_df_reb$max_dock[grid_id])
                bike_sub <- bike_df_reb[(bike_df_reb$current_grid==grid_id)&(bike_df_reb$bike_status=='dock'),]
                pick_type <- 'dock'
              } else if (bike_rank - scen_df_reb$b_in_dock[grid_id] <= scen_df_reb$b_in_rack[grid_id]){
                scen_df_reb$b_in_rack[grid_id] <- scen_df_reb$b_in_rack[grid_id] - 1
                scen_df_reb$avai_rack[grid_id] <- scen_df_reb$avai_rack[grid_id] + 1
                scen_df_reb$max_rack[grid_id] <- max(scen_df_reb$avai_rack[grid_id],scen_df_reb$max_rack[grid_id])
                bike_sub <- bike_df_reb[(bike_df_reb$current_grid==grid_id)&(bike_df_reb$bike_status=='rack'),]
                pick_type <- 'rack'
              } else {
                scen_df_reb$bike_street[grid_id] <- scen_df_reb$bike_street[grid_id] - 1
                bike_sub <- bike_df_reb[(bike_df_reb$current_grid==grid_id)&(bike_df_reb$bike_status=='street'),]
                pick_type <- 'street'
              }
              scen_df_reb$avai_bike[grid_id] <- scen_df_reb$avai_bike[grid_id] - 1
              if (nrow(bike_sub)==1){
                use_b_id <- bike_sub$bike_id[1]
              } else {
                use_b_id <- sample(bike_sub$bike_id, 1)
              }
              sim_trips_df_reb$bike_id[sim_trips_df_reb$trip_id==trip_id] <- use_b_id
              bike_df_reb$bike_status[bike_df_reb$bike_id==use_b_id] <- 'trip'
              bike_df_reb$trip_count[bike_df_reb$bike_id==use_b_id] <- bike_df_reb$trip_count[bike_df_reb$bike_id==use_b_id] + 1
              sim_trips_df_reb$status[sim_trips_df_reb$trip_id==trip_id] <- 'fulfill'
              sim_trips_df_reb$pickup_type[sim_trips_df_reb$trip_id==trip_id] <- pick_type
            } else {
              ############ if no bike, generate a bike
              b_id <- nrow(bike_df_reb)+1
              b_status <- 'trip'
              bike_df_reb[nrow(bike_df_reb) + 1, ] <- list(b_id,
                                                           grid_id,
                                                           b_status,
                                                           grid_id,
                                                           1)
              sim_trips_df_reb$bike_id[sim_trips_df_reb$trip_id==trip_id] <- b_id
              
              sim_trips_df_reb$status[sim_trips_df_reb$trip_id==trip_id] <- 'fulfill'
              sim_trips_df_reb$pickup_type[sim_trips_df_reb$trip_id==trip_id] <- 'generate'
            }
          }
        }
        ###### ------------------------------------------------------ bike return
        end_sub <- sim_trips_df_reb[(sim_trips_df_reb$end_hour==h)&(sim_trips_df_reb$end_min==m)&(sim_trips_df_reb$status=='fulfill'),]
        if (nrow(end_sub)>0){
          for (i in 1:nrow(end_sub)){
            trip_id <- end_sub$trip_id[i]
            grid_id <- which(scen_df_reb$grid_id == end_sub$end_grid_id[i])[1]
            use_b_id <- end_sub$bike_id[i]
            bike_df_reb$current_grid[bike_df_reb$bike_id==use_b_id] <- grid_id
            if (scen_df_reb$avai_dock[grid_id]>0){
              park_type <- 'dock'
              scen_df_reb$avai_dock[grid_id] <- scen_df_reb$avai_dock[grid_id] - 1
              scen_df_reb$b_in_dock[grid_id] <- scen_df_reb$b_in_dock[grid_id] + 1
              
              # } else {
              #   dist_ls <- dist_mat[grid_id, ]
              #   sorted_dist = dist_ls[order(dist_ls)]
              #   for (i in 2:length(sorted_dist)){
              #     n_id <- as.numeric(substring(names(sorted_dist)[i], 2))
              #     if (scen_df_reb$avai_dock[n_id]>0){
              #       sim_trips_df_reb$reroute_count[sim_trips_df_reb$trip_id==trip_id] <- sim_trips_df_reb$reroute_count[sim_trips_df_reb$trip_id==trip_id] + 1
              #       sim_trips_df_reb$reroute_dist[sim_trips_df_reb$trip_id==trip_id] <- sorted_dist[i] +
              #         sim_trips_df_reb$reroute_dist[sim_trips_df_reb$trip_id==trip_id]
              #       r_time <- round(sorted_dist[i]/bike_speed)
              #       
              #       end_h <- sim_trips_df_reb$end_hour[sim_trips_df_reb$trip_id==trip_id]
              #       end_m <- sim_trips_df_reb$end_min[sim_trips_df_reb$trip_id==trip_id]
              #       m_sum <- end_m + r_time
              #       sim_trips_df_reb$end_hour[sim_trips_df_reb$trip_id==trip_id] <- end_h + floor(m_sum/60)
              #       sim_trips_df_reb$end_min[sim_trips_df_reb$trip_id==trip_id] <- m_sum - floor(m_sum/60)*60
              #       sim_trips_df_reb$end_hour[sim_trips_df_reb$trip_id==trip_id] <- 
              #         ifelse(sim_trips_df_reb$end_hour[sim_trips_df_reb$trip_id==trip_id]>23, 23, sim_trips_df_reb$end_hour[sim_trips_df_reb$trip_id==trip_id])
              #       sim_trips_df_reb$end_min[sim_trips_df_reb$trip_id==trip_id] <- 
              #         ifelse(sim_trips_df_reb$end_hour[sim_trips_df_reb$trip_id==trip_id]>23, 59, sim_trips_df_reb$end_min[sim_trips_df_reb$trip_id==trip_id])
              #       sim_trips_df_reb$end_grid_id[sim_trips_df_reb$trip_id==trip_id] <- n_id
              #       break
              #     }
              #   }
              # }
            } else if (scen_df_reb$avai_rack[grid_id]>0){
              park_type <- 'rack'
              scen_df_reb$b_in_rack[grid_id] <- scen_df_reb$b_in_rack[grid_id] + 1
              scen_df_reb$avai_rack[grid_id] <- scen_df_reb$avai_rack[grid_id] - 1
            } else {
              scen_df_reb$bike_street[grid_id] <- scen_df_reb$bike_street[grid_id] + 1
              scen_df_reb$max_bike_street[scen_df_reb$grid_id == grid_id] <-
                max(scen_df_reb$max_bike_street[scen_df_reb$grid_id == grid_id],
                    scen_df_reb$bike_street[scen_df_reb$grid_id ==grid_id])
              park_type <- 'street'
            }
            
            bike_df_reb$bike_status[bike_df_reb$bike_id==use_b_id] <- park_type
            scen_df_reb$avai_bike[grid_id] <- scen_df_reb$avai_bike[grid_id] + 1
            sim_trips_df_reb$parking_type[sim_trips_df_reb$trip_id==trip_id] <- park_type
            
            
            ## test needs of rebalance
            if ((scen_df_reb$avai_bike[grid_id] >= grid_cap_rate * scen_df_reb$Docks[grid_id]) &
                (scen_df_reb$reb_status[grid_id] == 'none') &
                (scen_df_reb$avai_bike[grid_id] >= initial_bike_cap)) {
              # if ((scen_df_reb$avai_dock[grid_id] == 0) &
              #     (scen_df_reb$reb_status[grid_id] == 'none') &
              #     (scen_df_reb$avai_bike[grid_id] >= initial_bike_cap)) {
              scen_df_reb$reb_status[grid_id] <- 'requested'
              route_time <-
                dist_to_depot$duration_to_depot[dist_to_depot$grid_id == grid_id]
              m_sum <- m + route_time
              next_h <- h + floor(m_sum / 60)
              next_m <- m_sum - floor(m_sum / 60) * 60
              next_m <- ifelse(next_h > 23, 59, next_m)
              next_h <- ifelse(next_h > 23, 23, next_h)
              avai_truck <- truck_df[truck_df$status == 1, ]
              if (nrow(avai_truck) > 0) {
                if (nrow(avai_truck)==1){
                  use_truck_id <- avai_truck$truck_id[1]
                } else {
                  use_truck_id <- sample(avai_truck$truck_id, 1)
                }
                truck_df$status[truck_df$truck_id == use_truck_id] <- 2
              } else {
                use_truck_id <- nrow(truck_df) + 1
                truck_df[nrow(truck_df) + 1, ] <- c(use_truck_id, 2)
              }
              reb_id <- nrow(reb_df) + 1
              reb_df[nrow(reb_df) + 1,] <- list(reb_id,
                                                grid_id,
                                                2,
                                                1,
                                                h,
                                                m,
                                                next_h,
                                                next_m,
                                                0,
                                                0,
                                                0,
                                                0,
                                                0,
                                                use_truck_id)
            }
          }
        }
        ##### check truck arrival
        reb_sub <- reb_df[(reb_df$job_status==1)&(reb_df$arrive_h==h)&(reb_df$arrive_m==m),]
        if (nrow(reb_sub)>0){
          for (i in 1:nrow(reb_sub)){
            reb_id <- reb_sub$reb_id[i]
            grid_id <- reb_sub$grid[i]
            if (reb_sub$reb_type[i]==2){
              bike_to_move <- min(scen_df_reb$avai_bike[grid_id] - initial_bike_cap, truck_cap)
              # if (scen_df_reb$Bikes_fina[grid_id]>=initial_bike_cap){
              #   bike_to_move <- min(scen_df_reb$avai_bike[grid_id] - scen_df_reb$Bikes_fina[grid_id], truck_cap)
              # } else {
              #   bike_to_move <- scen_df_reb$avai_bike[grid_id] - initial_bike_cap 
              # }
              bike_to_move <- ifelse(bike_to_move<0, 0, bike_to_move)
              move_time <- round(bike_load_unit_time*bike_to_move)
              m_sum <- m + move_time
              next_h <- h + floor(m_sum/60)
              next_m <- m_sum - floor(m_sum/60)*60
              reb_df$finish_m[reb_id] <- ifelse(next_h>23, 59, next_m)
              reb_df$finish_h[reb_id] <- ifelse(next_h>23, 23, next_h)
            } 
            reb_df$job_status[reb_id] <- 2
          }
        }
        ########### load bike or unload
        reb_sub <-
          reb_df[(reb_df$job_status == 2) &
                   (reb_df$finish_h == h) & (reb_df$finish_m == m), ]
        if (nrow(reb_sub) > 0) {
          for (i in 1:nrow(reb_sub)) {
            reb_id <- reb_sub$reb_id[i]
            grid_id <- reb_sub$grid[i]
            use_truck_id <- reb_sub$truck_id[i]
            if (reb_sub$reb_type[i] == 1) {
              move_bike_ls <-
                bike_df_reb$bike_id[bike_df_reb$bike_status == paste('truck_', use_truck_id, sep = "")]
              bike_to_move <- length(move_bike_ls)
              for (br in 1:length(move_bike_ls)) {
                if (br <= scen_df_reb$avai_dock[grid_id]) {
                  b_status <- "dock"
                } else if (br - scen_df_reb$avai_dock[grid_id] <= scen_df_reb$avai_rack[grid_id]) {
                  b_status <- "rack"
                } else {
                  b_status <- "street"
                }
                b_id <- move_bike_ls[br]
                bike_df_reb$bike_status[bike_df_reb$bike_id == b_id] <-
                  b_status
                bike_df_reb$current_grid[bike_df_reb$bike_id == b_id] <-
                  grid_id
              }
              move_bike_df <-
                bike_df_reb[bike_df_reb$bike_id %in% move_bike_ls, ]
              b_to_dock <-
                nrow(move_bike_df[move_bike_df$bike_status == 'dock', ])
              b_to_rack <-
                nrow(move_bike_df[move_bike_df$bike_status == 'rack', ])
              b_street <-
                nrow(move_bike_df[move_bike_df$bike_status == 'street', ])
              scen_df_reb$avai_bike[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$avai_bike[scen_df_reb$grid_id == grid_id] + bike_to_move
              scen_df_reb$avai_dock[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$avai_dock[scen_df_reb$grid_id == grid_id] - b_to_dock
              scen_df_reb$b_in_dock[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$b_in_dock[scen_df_reb$grid_id == grid_id] + b_to_dock
              scen_df_reb$avai_rack[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$avai_rack[scen_df_reb$grid_id == grid_id] - b_to_rack
              scen_df_reb$b_in_rack[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$b_in_rack[scen_df_reb$grid_id == grid_id] + b_to_rack
              scen_df_reb$bike_street[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$bike_street[scen_df_reb$grid_id == grid_id] + b_street
              scen_df_reb$max_bike_street[scen_df_reb$grid_id == grid_id] <-
                max(scen_df_reb$max_bike_street[scen_df_reb$grid_id == grid_id],
                    scen_df_reb$bike_street[scen_df_reb$grid_id ==
                                              grid_id])
              
            } else {
              if (scen_df_reb$Bikes_fina[grid_id] >= initial_bike_cap) {
                bike_to_move <-
                  min(scen_df_reb$avai_bike[grid_id] - scen_df_reb$Bikes_fina[grid_id],
                      truck_cap)
              } else {
                bike_to_move <- scen_df_reb$avai_bike[grid_id] - initial_bike_cap
              }
              bike_to_move <- ifelse(bike_to_move < 0, 0, bike_to_move)
              if (bike_to_move <= scen_df_reb$bike_street[grid_id]) {
                bf_street <- bike_to_move
                bf_rack <- 0
                bf_dock <- 0
              } else if (bike_to_move - scen_df_reb$bike_street[grid_id] <= scen_df_reb$b_in_rack[grid_id]) {
                bf_street <- scen_df_reb$bike_street[grid_id]
                bf_rack <-
                  bike_to_move - scen_df_reb$bike_street[grid_id]
                bf_dock <- 0
              } else {
                bf_street <- scen_df_reb$bike_street[grid_id]
                bf_rack <- scen_df_reb$b_in_rack[grid_id]
                bf_dock <-
                  bike_to_move - scen_df_reb$bike_street[grid_id] - scen_df_reb$b_in_rack[grid_id]
              }
              
              if (bf_street > 0){
                move_bike_s_cand <-
                  bike_df_reb$bike_id[(bike_df_reb$bike_status == 'street') &
                                        (bike_df_reb$current_grid == grid_id)]
                if (length(move_bike_s_cand)==1){
                  move_bike_s <- move_bike_s_cand[1]
                } else {
                  move_bike_s <-
                    sample(move_bike_s_cand,
                           bf_street,
                           replace = FALSE,
                           prob = NULL)
                  
                } 
                bike_df_reb$bike_status[bike_df_reb$bike_id %in% move_bike_s] <-
                  paste('truck_', use_truck_id, sep = "")
                bike_df_reb$current_grid[bike_df_reb$bike_id %in% move_bike_s] <- 0
              }
              
              if (bf_rack > 0){
                move_bike_r_cand <-
                  bike_df_reb$bike_id[(bike_df_reb$bike_status == 'rack') &
                                        (bike_df_reb$current_grid == grid_id)]
                if (length(move_bike_r_cand)==1){
                  move_bike_r <- move_bike_r_cand[1]
                } else {
                  move_bike_r <-
                    sample(move_bike_r_cand,
                           bf_rack,
                           replace = FALSE,
                           prob = NULL)
                }
                
                bike_df_reb$bike_status[bike_df_reb$bike_id %in% move_bike_r] <-
                  paste('truck_', use_truck_id, sep = "")
                bike_df_reb$current_grid[bike_df_reb$bike_id %in% move_bike_r] <- 0
              }
              
              if (bf_dock > 0) {
                move_bike_d_cand <-
                  bike_df_reb$bike_id[(bike_df_reb$bike_status == 'dock') &
                                        (bike_df_reb$current_grid == grid_id)]
                if (length(move_bike_d_cand)==1){
                  move_bike_d <- move_bike_d_cand[1]
                } else {
                  move_bike_d <-
                    sample(move_bike_d_cand,
                           bf_dock,
                           replace = FALSE,
                           prob = NULL)
                }
                
                bike_df_reb$bike_status[bike_df_reb$bike_id %in% move_bike_d] <-
                  paste('truck_', use_truck_id, sep = "")
                bike_df_reb$current_grid[bike_df_reb$bike_id %in% move_bike_d] <- 0
              }
              
              
              
              
              scen_df_reb$avai_bike[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$avai_bike[scen_df_reb$grid_id == grid_id] - bike_to_move
              scen_df_reb$avai_dock[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$avai_dock[scen_df_reb$grid_id == grid_id] + bf_dock
              scen_df_reb$b_in_dock[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$b_in_dock[scen_df_reb$grid_id == grid_id] - bf_dock
              scen_df_reb$avai_rack[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$avai_rack[scen_df_reb$grid_id == grid_id] + bf_rack
              scen_df_reb$b_in_rack[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$b_in_rack[scen_df_reb$grid_id == grid_id] - bf_rack
              scen_df_reb$bike_street[scen_df_reb$grid_id == grid_id] <-
                scen_df_reb$bike_street[scen_df_reb$grid_id == grid_id] - bf_street
              scen_df_reb$max_dock[scen_df_reb$grid_id == grid_id] <-
                max(scen_df_reb$max_dock[scen_df_reb$grid_id == grid_id],
                    scen_df_reb$avai_dock[scen_df_reb$grid_id ==
                                            grid_id])
              scen_df_reb$max_rack[scen_df_reb$grid_id == grid_id] <-
                max(scen_df_reb$max_rack[scen_df_reb$grid_id == grid_id],
                    scen_df_reb$avai_rack[scen_df_reb$grid_id ==
                                            grid_id])
              scen_df_reb$max_bike_street[scen_df_reb$grid_id == grid_id] <-
                max(scen_df_reb$max_bike_street[scen_df_reb$grid_id == grid_id],
                    scen_df_reb$bike_street[scen_df_reb$grid_id ==
                                              grid_id])
              
              route_time <-
                dist_to_depot$duration_to_depot[dist_to_depot$grid_id == grid_id]
              m_sum <- m + route_time
              next_h <- h + floor(m_sum / 60)
              next_m <- m_sum - floor(m_sum / 60) * 60
              reb_df$return_m[reb_df$reb_id == reb_id] <-
                ifelse(next_h > 23, 59, next_m)
              reb_df$return_h[reb_df$reb_id == reb_id] <-
                ifelse(next_h > 23, 23, next_h)
              reb_df$num_bike[reb_df$reb_id == reb_id] <- bike_to_move
            }
            reb_df$job_status[reb_id] <- 3
            scen_df_reb$reb_status[grid_id] <- "none"              ##################################################################################
            
          }
        }
        
        ########### back to depot
        reb_sub <- reb_df[(reb_df$job_status==3)&(reb_df$return_h==h)&(reb_df$return_m==m),]
        if (nrow(reb_sub)>0){
          for (i in 1:nrow(reb_sub)){
            reb_id <- reb_sub$reb_id[i]
            grid_id <- reb_sub$grid[i]
            use_truck_id <- reb_sub$truck_id[i]
            if (reb_sub$reb_type[i]==2){
              move_bike_ls <- bike_df_reb$bike_id[bike_df_reb$bike_status==paste('truck_',use_truck_id,sep = "")]
              bike_df_reb$bike_status[bike_df_reb$bike_id %in% move_bike_ls] <- 'depot'
              bike_df_reb$current_grid[bike_df_reb$bike_id %in% move_bike_ls] <- 0
            } 
            reb_df$job_status[reb_id] <- 4
            truck_df$status[truck_df$truck_id==use_truck_id] <- 1
          }
        }
        
      }
    }
    
    write.csv(sim_trips_df_reb, paste(base_route, '/output/', sys_type,'/no_rack_simulated_trips_reb_scen_', scenario,'_',model_month,'_',
                                      ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), row.names = FALSE)
    write.csv(scen_df_reb, paste(base_route, '/output/', sys_type, '/no_rack_grid_simulation_record_reb_scen_', scenario,'_',model_month,'_',
                                 ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), row.names = FALSE)
    write.csv(bike_df_reb, paste(base_route, '/output/', sys_type, '/no_rack_bike_status_reb_scen_', scenario,'_',model_month,'_',
                                 ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), row.names = FALSE)
    write.csv(reb_df, paste(base_route, '/output/', sys_type, '/no_rack_rebalance_record_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), row.names = FALSE)
    write.csv(truck_df, paste(base_route, '/output/', sys_type,'/no_rack_truck_record_reb_scen_', scenario,'_',model_month,'_',
                              ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), row.names = FALSE)
  }
}




