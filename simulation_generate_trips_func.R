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
# library(dplyr, lib.loc = "/depot/cai161/kouz/Rlibs/")
# library(lubridate, lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
# library('iterators', lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
# library('foreach', lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
# library('doMC', lib.loc = "/depot/cai161/data/Bike_Share_Data/2016_trip_data/Rlibs")
# 
GenerateSimuTrips <- function(city, sta_df, trips_y, model_month = '09', weekday_idx = TRUE, true_or_obs = 'true'){
  ########################## Generate grid correlation --------------------------------------
  in_grid_ls <- unique(sta_df$grid_id)
  trip_month <- trips_y[trips_y$month==model_month,]
  trip_month <- trip_month[trip_month$weekday==weekday_idx,]
  
  grid_match <- sta_df[,c('StationID', 'grid_id')]
  names(grid_match) <- c('start_station_id', 'start_grid')
  
  trip_month <- left_join(trip_month, grid_match, by='start_station_id')
  names(grid_match) <- c('end_station_id', 'end_grid')
  trip_month <- left_join(trip_month, grid_match, by='end_station_id')
  
  trip_month <- trip_month[complete.cases(trip_month$start_grid),]
  trip_month <- trip_month[complete.cases(trip_month$end_grid),]
  
  base_route <- paste('/depot/cai161/kouz/',city,'_data/', sep = "")
  
  dist_mat <- read.csv(paste(base_route, city,
                             '_grid_distance_matrix_manhattan.csv',sep = ""), stringsAsFactors = FALSE)
  dist_mat$X <- NULL
  dist_mat <- data.matrix(dist_mat)
  
  w_idx <- ifelse(weekday_idx, "wkd", 'wke')
  rate <- read.csv(paste(base_route, city,'_', w_idx,'_w_hr_rate_',as.numeric(model_month),'.csv',sep = ""), header=TRUE, stringsAsFactors = FALSE)
  rate$hourly_rate[is.na(rate$hourly_rate)] <- 0
  sta_grid_match <- sta_df[,c(1,6)]
  names(sta_grid_match) <- c('sta_id', 'grid_id')
  rate <- left_join(rate, sta_grid_match, by='sta_id')
  # rate$adj_rate <- rate$avai_t_slots/rate$num_t_slots
  # rate$obs_hourly_rate <- rate$hourly_rate*rate$adj_rate
  # rate <- rate[complete.cases(rate$grid_id),]
  rate$rate <- ifelse(rate$hour<6,rate$obs_hourly_rate, rate$hourly_rate)
  
  grid_rate <- aggregate(rate$rate, list(grid_id=rate$grid_id, hour=rate$hour), sum)
  colnames(grid_rate)[3] <- "rate"
  
  w_rate <- grid_rate
  rm(grid_rate)
  w_rate <- w_rate[w_rate$grid_id %in% in_grid_ls,]
  
  names(w_rate)[3] <- 'hourly_w_rate'
  # fill in a very small number for 0 hourly rate
  w_rate$hourly_w_rate <- ifelse(w_rate$hourly_w_rate==0, 0.00000000001, w_rate$hourly_w_rate)
  
  
  trip_m_wkd <- trip_month
  
  trip_m_wkd$grid_dist <- 0
  for (i in 1:nrow(trip_m_wkd)){
    trip_m_wkd$grid_dist[i] <- dist_mat[trip_m_wkd$start_grid[i], trip_m_wkd$end_grid[i]]
  }
  
  single_trips <- trip_m_wkd[trip_m_wkd$grid_dist>0,]
  # single_trips$trip_duration <- as.numeric(gsub(",", "", single_trips$trip_duration))
  # single_trips$trip_duration <- single_trips$trip_duration/60
  single_trips <- single_trips[complete.cases(single_trips$trip_duration),]
  single_trips$speed <- single_trips$grid_dist/single_trips$trip_duration
  avg_speed <- mean(single_trips$speed)
  print(avg_speed)
  rm(single_trips)
  round_trips <- trip_m_wkd[trip_m_wkd$start_grid==trip_m_wkd$end_grid,]
  round_trips <- round_trips[complete.cases(round_trips$trip_duration),]
  perct_round <- nrow(round_trips)/nrow(trip_m_wkd)
  
  # round_trips$trip_duration <- as.numeric(gsub(",", "", round_trips$trip_duration))
  # round_trips$trip_duration <- round_trips$trip_duration/60
  basic_stats <- data.frame(city=character(), avg_bike_speed=numeric(),
                            pct_round_trips=numeric(), total_trips=numeric(),
                            num_days = numeric(), trips_per_day=numeric(), stringsAsFactors = FALSE)
  basic_stats[nrow(basic_stats)+1,] <- c(city, avg_speed, perct_round, nrow(trip_m_wkd),
                                         length(unique(trip_m_wkd$date)),
                                         nrow(trip_m_wkd)/length(unique(trip_m_wkd$date)))
  write.csv(basic_stats, paste(base_route, city,
                               '_basic_stats.csv',sep = ""), row.names = FALSE)
  
  grid_corr_count <- read.csv(paste('/depot/cai161/kouz/',city,'_data/',city,'_grid_corr_count_',model_month,'.csv', sep = ""), stringsAsFactors = FALSE)
  
  grid_corr_count$hour <- as.numeric(grid_corr_count$start_hour)
  
  # registerDoMC(10)
  # now simulate for 10 times
  for (n_sim in 1:10){
  # for (n_sim in c(2,9)){
    print(n_sim)
    sim_trips_df <- data.frame(hour=integer(), min=numeric(), start_grid_id=integer(), stringsAsFactors = FALSE)
    for (i in 1:nrow(w_rate)){
      # if (i%%1000==0){
      #   print(i)
      # }
      grid_id <- w_rate$grid_id[i]
      hour <- w_rate$hour[i]
      m_rate <- w_rate$hourly_w_rate[i]/60
      t <- 0
      while (t < 60){
        t <- t - log(runif(1)) / m_rate # https://zhuanlan.zhihu.com/p/28730584
        if (t < 60){
          sim_trips_df[nrow(sim_trips_df)+1,] <- c(hour,floor(t),grid_id)
        }
      }
    }

    ####### simulation to determine the end destination
    sim_trips_df$pred_end_grid <- NA
    for (i in 1:nrow(sim_trips_df)){
      # if (i%%10000==0){
      #   print(i)
      # }
      h <- sim_trips_df$hour[i]
      sub_pop <- grid_corr_count[grid_corr_count$hour==h,]
      sub_pop <- sub_pop[sub_pop$start_grid==sim_trips_df$start_grid_id[i],]
      if (nrow(sub_pop) > 1){
        sub_pop$prob = sub_pop$count/sum(sub_pop$count)
        sim_trips_df$pred_end_grid[i] <- sample(sub_pop$end_grid, size = 1, replace = TRUE, prob = sub_pop$prob)
      } else if (nrow(sub_pop) == 1){
        sim_trips_df$pred_end_grid[i] <- sub_pop$end_grid[1]
      } else {
        sim_trips_df$pred_end_grid[i] <- sim_trips_df$start_grid_id[i]
        print(paste('No history record',sim_trips_df$start_grid_id[i], h))
      }
    }
    sim_trips_df <- sim_trips_df[order(sim_trips_df$hour, sim_trips_df$min),]

    colnames(sim_trips_df)[4] <- "end_grid_id"

    ######### fill in an estimated duration

    sim_trips_df$grid_dist <- 0
    for (i in 1:nrow(sim_trips_df)){
      # if (i%%10000==0){
      #   print(i)
      # }
      start_id <- sim_trips_df$start_grid_id[i]
      end_id <- sim_trips_df$end_grid_id[i]
      sim_trips_df$grid_dist[i] <- dist_mat[start_id,end_id]
    }

    sim_trips_df$duration <- 0
    for (i in 1:nrow(sim_trips_df)){
      # if (i%%1000==0){
      #   print(i)
      # }
      if (sim_trips_df$grid_dist[i]!=0){
        sim_trips_df$duration[i] <- sim_trips_df$grid_dist[i]/avg_speed
      } else {
        sim_trips_df$duration[i] <- sample(round_trips$trip_duration, size = 1, replace = TRUE)
      }
    }

    sim_trips_df$duration <- round(sim_trips_df$duration)
    sim_trips_df$trip_id <- c(1:nrow(sim_trips_df))

    write.csv(sim_trips_df, paste(base_route, '/output/simulated_trips_own_',model_month,'_',
                                  ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'sim',n_sim,'.csv', sep = ""), row.names = FALSE)

  }
}



