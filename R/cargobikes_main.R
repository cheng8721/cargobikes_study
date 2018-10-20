rm(list=ls())

library(ggplot2)
library(Imap) # gdist()
library(leaflet) # maps
library(geosphere)
library(dplyr)
library(TSP)

dir <- "~/cargo_bikes_study"
setwd(dir)
source("cargobikes_fn.R")


### LOAD
raw <- read.csv("data/raw.csv", stringsAsFactors = F, header=T)
names(raw) <- c("month", "weekend_date", "date", "pkg_barcode_num", "stop_start_time", "stop_end_time", "lat", "lon", "commercial_residential", "dispatch_loop_position_num", "stop_type", "consignee_address1","consignee_address2","consignee_address3", "consignee_postcode","service_level_code", "service_level", "weight", "delivery_info", "dwell_time")
#raw <- read.csv("data/UPS_data_aggregated.csv", stringsAsFactors = F, header=T)
#names(ups_dat) <- c("date", "lat", "lon", "weight", "package_count", "consignee_address1","consignee_address2","consignee_address3", "consignee_postcode", "departure_time")


### CLEAN
raw$stop_start_time <- as.POSIXct(paste(raw$date, "00:00:00"), format = "%d/%m/%y %H:%M:%S") + 3600*raw$stop_start_time #needed only to compute dwell time
raw$stop_end_time <- as.POSIXct(paste(raw$date, "00:00:00"), format = "%d/%m/%y %H:%M:%S") + 3600*raw$stop_end_time #needed only to compute dwell time
raw$dwell_time <- raw$dwell_time*60 #convert hours into minutes
#raw$dwell_time <- as.numeric(difftime(raw$stop_end_time, raw$stop_start_time, units="mins")) #CHANGE this --> use Rayden data
raw$date <- as.Date(raw$date, format="%d/%m/%y")
raw$weight <- round(raw$weight, 1)
raw <- raw[raw$weight<180,] #suspect one outlier with weight==256.8 ...CHECK!
#raw <- raw[raw$lat<=1.473965 & raw$lat>=1.237098 & raw$lon<=104.042086 & raw$lon>=103.605287,] #eliminate points outside singapore
raw$nodeID <- 1:nrow(raw)
raw$nodeID <- as.character(raw$nodeID)


### INPUTS
max_nboxes_hub <- 9 #max number of boxes carried by hub
max_boxweight <- 205 #180 kg in the box + 25 kg front bike
tot_load_share <- 0.7 #70% of max filling
boxweight <- max_boxweight*tot_load_share #max weight a bike-tour can carry
bike_tour_length <- 40000 #(in meters) max tour length is 50km, we assume 40 km since we use euclidean distances 
hub_cut <- 2000 #(in meters) max dist allowed betwwen any 2 points within same hub cluster
avg_bike_speed <- 400 #(meters/min) average bike speed
bike_dwelltime_frac <- 0.6 #if ==1 then the dwell time bike == dwell time truck; if with bike the dwell time is reduced by 50%, then bike_dwelltime_frac <- 0.5






### DEMAND GENERATION
dat <- generate_demand(data=raw, xweight=1, simulated="Y", ndel=500, nrep=100) # original data contains 64 days, about 300 deliveries per day
dat$dwell_time <- dat$dwell_time*bike_dwelltime_frac # FOR BIKES


### SCHEDULING
res_tour <- data.frame()
res_hub <- data.frame()

for (i in 1:length(unique(dat$date))) {
  print(i)
  
  # select date
  tmp <- dat[dat$date==unique(dat$date)[i],]
  
  # compute day schedule
  day_schedule <- day_scheduling(dat, max_nboxes_hub, boxweight, tot_load_share, bike_tour_length, hub_cut, avg_bike_speed)
  res_hub <- rbind(res_hub, day_schedule$day_res_hub)
  res_bike <- rbind(res_bike, day_schedule$day_res_bike)
  
}













#map_orders <- leaflet(data=tmp) %>% #plot all delivery points for a given day
#  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE) ) %>%
#  addCircleMarkers(~lon, ~lat, stroke = FALSE, fillOpacity = 0.8, radius=5)
#for (i in 1:max(results_tours$tourID)) {
#  map_orders <- map_orders %>% addPolylines(lat = results_tours[results_tours$tourID==i,"lat"], lng = results_tours[results_tours$tourID==i,"lon"]) 
#}
#map_orders


### SAVE
results_tours[,] <- lapply(results_tours[,], as.character)
write.csv(results_tours, file="RYTLEtours_bikes.csv", row.names =F)

results_hubs[,] <- lapply(results_hubs[,], as.character)
write.csv(results_hubs, file="RYTLEtours_hubs.csv", row.names =F)

















