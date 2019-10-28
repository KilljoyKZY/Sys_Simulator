library(dplyr)
library(ggpubr)
library(reshape2)
library(Rmisc)
# library("ggpubr", lib.loc="~/R/R-3.5.1/library")

while (!is.null(dev.list()))  dev.off()

# city <- 'Los Angeles'
# city <- 'Philadelphia'
# city <- 'New York'
city <- 'Chicago'

scenario <- 'own'
model_month <- '09'
weekday_idx <- TRUE
true_or_obs <- 'true'

base_route <- paste('/depot/cai161/kouz/',city,'_data/', sep = "")

type_com <- data.frame(scenario=character(), num_bike=numeric(), total_duration=numeric(), total_truck_duration=numeric(), 
                       reroute_count=numeric(), total_reroute_dist=numeric(), max_st_bike=numeric(),
                       current_bikes=numeric(), bike_utilization=numeric(), stringsAsFactors = FALSE)


for (n_sim in 1:10){
  sys_type <- 'station'
  bike_st <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_bike_status_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  trip_st <- read.csv(paste(base_route, '/output/', sys_type,'/no_rack_simulated_trips_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  reb_st <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_rebalance_record_reb_scen_', scenario,'_',model_month,'_',
                           ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  scen_st <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_grid_simulation_record_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  
  sys_type <- 'dockless'
  bike_dl <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_bike_status_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  trip_dl <- read.csv(paste(base_route, '/output/', sys_type,'/no_rack_simulated_trips_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  reb_dl <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_rebalance_record_reb_scen_', scenario,'_',model_month,'_',
                           ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  scen_dl <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_grid_simulation_record_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  
  
  sys_type <- 'dockless_random'
  bike_rd <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_bike_status_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  trip_rd <- read.csv(paste(base_route, '/output/', sys_type,'/no_rack_simulated_trips_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  reb_rd <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_rebalance_record_reb_scen_', scenario,'_',model_month,'_',
                           ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  scen_rd <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_grid_simulation_record_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  
  reb_st$t1 = reb_st$request_h*60+reb_st$request_m
  reb_st$t2 = reb_st$arrive_h*60+reb_st$arrive_m
  reb_st$travel_t = (reb_st$t2 - reb_st$t1)*2
  
  reb_dl$t1 = reb_dl$request_h*60+reb_dl$request_m
  reb_dl$t2 = reb_dl$arrive_h*60+reb_dl$arrive_m
  reb_dl$travel_t = (reb_dl$t2 - reb_dl$t1)*2
  
  reb_rd$t1 = reb_rd$request_h*60+reb_rd$request_m
  reb_rd$t2 = reb_rd$arrive_h*60+reb_rd$arrive_m
  reb_rd$travel_t = (reb_rd$t2 - reb_rd$t1)*2
  
  fulfill_trips_st <- trip_st[trip_st$status=='fulfill',]
  total_dur_st <- sum(fulfill_trips_st$duration)
  
  fulfill_trips_dl <- trip_dl[trip_dl$status=='fulfill',]
  total_dur_dl <- sum(fulfill_trips_dl$duration)
  
  fulfill_trips_rd <- trip_rd[trip_rd$status=='fulfill',]
  total_dur_rd <- sum(fulfill_trips_rd$duration)
  
  scen_dl$max_all_bikes = scen_dl$max_bike_street + scen_dl$Docks
  scen_rd$max_all_bikes = scen_rd$max_bike_street + scen_rd$Docks
  
  type_com[nrow(type_com)+1,] <- list('Station', nrow(bike_st), total_dur_st, sum(reb_st$travel_t),
                                      sum(trip_st$reroute_count), sum(trip_st$reroute_dist),
                                      max(scen_st$max_bike_street), sum(scen_st$Bikes_fina),
                                      mean(bike_st$trip_count))
  type_com[nrow(type_com)+1,] <- list('Dockless', nrow(bike_dl), total_dur_dl, sum(reb_dl$travel_t),
                                      sum(trip_dl$reroute_count), sum(trip_dl$reroute_dist),
                                      max(scen_dl$max_all_bikes), sum(scen_dl$Bikes_fina),
                                      mean(bike_dl$trip_count))
  type_com[nrow(type_com)+1,] <- list('Hybrid', nrow(bike_dl), total_dur_dl, sum(reb_dl$travel_t),
                                      sum(trip_dl$reroute_count), sum(trip_dl$reroute_dist),
                                      max(scen_dl$max_bike_street), sum(scen_dl$Bikes_fina),
                                      mean(bike_dl$trip_count))
  type_com[nrow(type_com)+1,] <- list('Hybrid_Random', nrow(bike_rd), total_dur_rd, sum(reb_rd$travel_t),
                                      sum(trip_rd$reroute_count), sum(trip_rd$reroute_dist),
                                      max(scen_rd$max_bike_street), sum(scen_rd$Bikes_fina),
                                      mean(bike_rd$trip_count))
}


truck_speed <- 23.7 # mile/hr
type_com$total_truck_distance <- type_com$total_truck_duration*truck_speed/60

tgc2 <- summarySE(type_com, measurevar="num_bike", groupvars=c("scenario"))
tgc_dur2 <- summarySE(type_com, measurevar="total_duration", groupvars=c("scenario"))
tgc_reb2 <- summarySE(type_com, measurevar="total_truck_distance", groupvars=c("scenario"))
tgc_recount2 <- summarySE(type_com, measurevar="reroute_count", groupvars=c("scenario"))
tgc_redist2 <- summarySE(type_com, measurevar="total_reroute_dist", groupvars=c("scenario"))
tgc_maxb2 <- summarySE(type_com, measurevar="max_st_bike", groupvars=c("scenario"))
tgc_currbike2 <- summarySE(type_com, measurevar="current_bikes", groupvars=c("scenario"))
tgc_bike_util2 <- summarySE(type_com, measurevar="bike_utilization", groupvars=c("scenario"))

tgc_join2 <- left_join(tgc2, tgc_dur2, by='scenario')
tgc_join2 <- left_join(tgc_join2, tgc_reb2, by='scenario')
tgc_join2 <- left_join(tgc_join2, tgc_recount2, by='scenario')
tgc_join2 <- left_join(tgc_join2, tgc_redist2, by='scenario')
tgc_join2 <- left_join(tgc_join2, tgc_maxb2, by='scenario')
tgc_join2 <- left_join(tgc_join2, tgc_currbike2, by='scenario')
tgc_join2 <- left_join(tgc_join2, tgc_bike_util2, by='scenario')


# # Standard error of the mean

#### Compare the bike supply of current system vs simulation 

tgc_current <- tgc_join2[,c(33, 35)]
tgc_current <- tgc_current[1,]
names(tgc_current) <- c('num_bike', 'se')
tgc_current$scenario <- 'Current'
tgc_simul <- tgc_join2[,c(3, 5)]
tgc_simul <- tgc_simul[3,]
names(tgc_simul) <- c('num_bike', 'se')
tgc_simul$scenario <- 'Simulation'

tgc <- rbind(tgc_current, tgc_simul)

fig_route <- paste(base_route,'figures/', sep = "")
if (!dir.exists(fig_route)){
  dir.create(fig_route)
}

theme_set(theme_pubr())

png(file=paste(fig_route,'needed_bikes_current.png', sep = ""),
    width = 500, height = 500, res = 150)
ggplot(tgc, aes(x=scenario, y=num_bike)) +
  geom_bar(stat="identity", fill="orange") +
  geom_errorbar(aes(ymin=num_bike-se, ymax=num_bike+se), width=.2)+
  ylab('Number of bikes')+
  xlab('Scenario')+
  ggtitle(city)
dev.off()



tgc_join2$scenario <- c("D", 'HP', 'HR', 'S')
tgc_join2$scenario = factor(tgc_join2$scenario, levels=c("S", 'HP', 'HR', 'D'))

write.csv(tgc, file = paste(base_route, city, '_compare_current_and_simul.csv', sep = ""))
write.csv(tgc_join2, file = paste(base_route, city, '_compare_sys_types.csv', sep = ""))

# p1 <- ggplot(tgc_join2, aes(x=scenario, y=num_bike)) + 
#   geom_bar(stat="identity", fill="orange") +
#   geom_errorbar(aes(ymin=num_bike-se.x, ymax=num_bike+se.x), width=.2)+
#   geom_text(aes(label=round(num_bike)), position=position_dodge(width=0.9), vjust=+2)+
#   ylab('Number of bikes')+
#   xlab('System type')+
#   ggtitle(city)+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# p2 <- ggplot(tgc_join2, aes(x=scenario, y=bike_utilization)) + 
#   geom_bar(stat="identity", fill="skyblue") +
#   geom_errorbar(aes(ymin=bike_utilization-se.y.y.y.y, ymax=bike_utilization+se.y.y.y.y), width=.2)+
#   geom_text(aes(label=round(bike_utilization, 2)), position=position_dodge(width=0.9), vjust=+2)+
#   ylab('\n Bike utilization\n')+
#   xlab('System type')+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# p3 <- ggplot(tgc_join2, aes(x=scenario, y=total_truck_distance)) + 
#   geom_bar(stat="identity", fill="rosybrown1") +
#   geom_errorbar(aes(ymin=total_truck_distance-se.x.x, ymax=total_truck_distance+se.x.x), width=.2)+
#   geom_text(aes(label=round(total_truck_distance, 1)), position=position_dodge(width=0.9), vjust=0, hjust=-0.1)+
#   ylab('Rebalance truck\n travel mileage')+
#   xlab('System type')+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# p4 <- ggplot(tgc_join2, aes(x=scenario, y=reroute_count)) + 
#   geom_bar(stat="identity", fill="navajowhite") +
#   geom_errorbar(aes(ymin=reroute_count-se.y.y, ymax=reroute_count+se.y.y), width=.2)+
#   geom_text(aes(label=round(reroute_count, 1)), position=position_dodge(width=0.9), vjust=0, hjust=-0.1)+
#   ylab('Number of trips\n requiring rerouting\n to return')+
#   xlab('System type')+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# p5 <- ggplot(tgc_join2, aes(x=scenario, y=max_st_bike)) + 
#   geom_bar(stat="identity", fill="sienna1") +
#   geom_errorbar(aes(ymin=max_st_bike-se.y.y.y, ymax=max_st_bike+se.y.y.y), width=.2)+
#   geom_text(aes(label=round(max_st_bike, 1)),position=position_dodge(width=0.9), vjust=0, hjust=-0.1)+
#   ylim(NA, max(tgc_join2$max_st_bike)*1.2)+
#   ylab('Max bikes parked  \non street in one grid')+
#   xlab('System type')
# 
# png(file=paste(fig_route,'type_comparison.png', sep = ""), 
#     width = 400, height = 900, res = 100)
# ggarrange(p1, p2, p3, p4,p5, ncol = 1, nrow = 5, labels = c('(a2)', '(b2)', '(c2)', '(d2)', '(e2)'),
#           hjust = 0, vjust=0.7, font.label = list(size = 8, face = "bold", color ="black"))
# dev.off()




p3 <- ggplot(tgc_join2, aes(x=scenario, y=total_truck_distance)) + 
  geom_bar(stat="identity", fill="rosybrown1") +
  geom_errorbar(aes(ymin=total_truck_distance-se.x.x, ymax=total_truck_distance+se.x.x), width=.2)+
  geom_text(aes(label=round(total_truck_distance, 1)), position=position_dodge(width=0.9), vjust=0, hjust=-0.1)+
  ylab('')+
  xlab('')+
  ggtitle(city)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p4 <- ggplot(tgc_join2, aes(x=scenario, y=reroute_count)) + 
  geom_bar(stat="identity", fill="navajowhite") +
  geom_errorbar(aes(ymin=reroute_count-se.y.y, ymax=reroute_count+se.y.y), width=.2)+
  geom_text(aes(label=round(reroute_count, 1)), position=position_dodge(width=0.9), vjust=0, hjust=-0.1)+
  ylab('')+
  xlab('')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p5 <- ggplot(tgc_join2, aes(x=scenario, y=max_st_bike)) + 
  geom_bar(stat="identity", fill="sienna1") +
  geom_errorbar(aes(ymin=max_st_bike-se.y.y.y, ymax=max_st_bike+se.y.y.y), width=.2)+
  geom_text(aes(label=round(max_st_bike, 1)),position=position_dodge(width=0.9), vjust=0, hjust=-0.1)+
  ylim(NA, max(tgc_join2$max_st_bike)*1.2)+
  ylab('')+
  xlab('')

png(file=paste(fig_route,'type_comparison_short.png', sep = ""), 
    width = 400, height = 700, res = 110)
ggarrange(p3, p4,p5, ncol = 1, nrow = 3,
          hjust = 0, vjust=0.7, font.label = list(size = 8, face = "bold", color ="black"))
dev.off()

while (!is.null(dev.list()))  dev.off()



# plot the number of bikes over supplied
features <- read.csv(paste(base_route, city ,'_grid_df.csv',sep = ""), stringsAsFactors = FALSE)
bike_ut <- features[,c('grid_id','Longitude','Latitude')]
sys_type <- 'station'
for (n_sim in 1:10){
  
  bike_df <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_bike_status_reb_scen_', scenario,'_',model_month,'_',
                 ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)
  bike_agg <- aggregate(bike_df$bike_id,list(grid_id=bike_df$initial_grid),length)
  names(bike_agg)[2] <- paste('rate',n_sim)
  bike_ut <- left_join(bike_ut, bike_agg, by='grid_id')
  bike_ut[,c(paste('rate',n_sim))][is.na(bike_ut[,c(paste('rate',n_sim))])] <- 0
}

bike_ut$avg_rate <- rowMeans(bike_ut[,c(4:13)])
# write.csv(bike_ut, paste(base_route, 'own/output/bike_utilization_by_grid/bike_utilization_by_grid_scen_', scenario,'_',model_month,'_',
# ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'sim',n_sim,'.csv', sep = ""),row.names = FALSE)

curr_scen <- read.csv(paste(base_route, '/output/', sys_type, '/no_rack_grid_simulation_record_reb_scen_', scenario,'_',model_month,'_',
                            ifelse(weekday_idx, 'weekday','weekend'), '_',true_or_obs,'_',sys_type,'sim',n_sim,'.csv', sep = ""), stringsAsFactors = FALSE)

curr_scen <- left_join(curr_scen, bike_ut, by='grid_id')

curr_scen$b_diff <- curr_scen$Bikes_fina - curr_scen$avg_rate

curr_scen$b_over <- ifelse(curr_scen$b_diff>0, curr_scen$b_diff, 0)
curr_scen$b_short <- ifelse(curr_scen$b_diff<0, -curr_scen$b_diff, 0)

range(curr_scen$b_diff)

########### Density
png(file=paste(fig_route,'bike_supply_density.png', sep = ""), 
    width = 800, height = 500, res = 180)
ggplot(curr_scen, aes(x=b_diff)) + 
  geom_density()+
  # ggtitle(paste("Scenario: ", scenario, sep=""),
  #         subtitle = paste("Month: ",model_month, ', ', ifelse(weekday_idx, 'weekday','weekend'), sep=""))+
  xlab("Oversupply (+) and shortage (-) of bikes in the grid")+
  ylab("Density")
dev.off()

png(file=paste(fig_route,'bike_oversupply.png', sep = ""), 
    width = 1100, height = 1000, res = 170)
ggplot(curr_scen,aes(x=Longitude, y=Latitude, colour = b_over)) +
  geom_point() +
  scale_colour_gradient(name="Bike oversupply", low = "white", high = "red")+
  theme(legend.position="right",legend.direction = "vertical")+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank()
  )
dev.off()

png(file=paste(fig_route,'bike_shortage.png', sep = ""), 
    width = 1100, height = 1000, res = 170)
ggplot(curr_scen,aes(x=Longitude, y=Latitude, colour = b_short)) +
  geom_point() +
  scale_colour_gradient(name="Bike shortage", low = "white", high = "purple")+
  theme(legend.position="right",legend.direction = "vertical")+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank()
  )
dev.off()







