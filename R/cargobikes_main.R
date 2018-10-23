rm(list=ls())

library(ggplot2)
library(Imap) # gdist()
library(leaflet) # maps
library(geosphere)
library(dplyr)
library(TSP)

dir <- "/Users/giacomodc/Dropbox/cargobikes_study"
setwd(dir)
source(paste0(dir,"/R/cargobikes_fn.R"))


### LOAD
raw <- read.csv("data/raw.csv", stringsAsFactors = F, header=T)
names(raw) <- c("month", "weekend_date", "date", "pkg_barcode_num", "stop_start_time", "stop_end_time", "lat", "lon", "commercial_residential", "dispatch_loop_position_num", "stop_type", "consignee_address1","consignee_address2","consignee_address3", "consignee_postcode","service_level_code", "service_level", "weight", "delivery_info", "dwell_time")


### CLEAN
raw$stop_start_time <- as.POSIXct(paste(raw$date, "00:00:00"), format = "%d/%m/%y %H:%M:%S") + 3600*raw$stop_start_time #needed only to compute dwell time
raw$stop_end_time <- as.POSIXct(paste(raw$date, "00:00:00"), format = "%d/%m/%y %H:%M:%S") + 3600*raw$stop_end_time #needed only to compute dwell time
raw$dwell_time <- raw$dwell_time*60 #convert hours to minutes
raw$date <- as.Date(raw$date, format="%d/%m/%y")
raw$weight <- round(raw$weight, 1)
raw <- raw[raw$weight<180,] #suspect one outlier with weight==256.8 ...CHECK!
raw$nodeID <- 1:nrow(raw)
raw$nodeID <- as.character(raw$nodeID)


### INPUTS
max_nboxes_hub <- 9 #max number of boxes carried by hub
max_boxweight <- 205 #180 kg in the box + 25 kg front bike
tot_load_share <- 0.7 #70% of max filling
boxweight <- max_boxweight*tot_load_share #max weight a bike-tour can carry
bike_tour_length <- 40000 #(in meters) max bike tour length is 50km, we assume 40 km since we use euclidean distances 
hub_cut <- 2000 #(in meters) max dist allowed betwwen any 2 points within same hub cluster
avg_bike_speed <- 250 #(meters/min) average bike speed
bike_dwelltime_frac <- 0.6 #if ==1 then the dwell time bike == dwell time truck; if with bike the dwell time is reduced by 50%, then bike_dwelltime_frac <- 0.5
mode <- "bike" #truck



### SCENARIOS
nrep = 100 #no. of simulations for each scenario (==number of days simulated)
vector_xweight = seq(from=0.5, to=4, by=0.25)
vector_ndel = seq(from=100, to=700, by=50)
scenarios <- data.frame("ndel"=rep(vector_ndel, each=length(vector_xweight)), "xweight"=vector_xweight)

res_ALLscenarios_hub <- data.frame()
res_ALLscenarios_tour_scehdule <- data.frame()
res_ALLscenarios_tour_stats <- data.frame()

for (k in 1:nrow(scenarios)) {
  print(k)
  xweight <- scenarios[k,"xweight"]
  ndel <- scenarios[k,"ndel"]
  
  ### DEMAND GENERATION
  dat <- generate_demand(data=raw, xweight=xweight, simulated="Y", ndel=ndel, nrep=nrep) # original data contains 64 days, ~300 deliveries per day
  if (mode=="bike") dat$dwell_time <- dat$dwell_time*bike_dwelltime_frac
  
  ### SCHEDULING
  res_hub <- data.frame()
  res_tour_schedule <- data.frame()
  res_tour_stats <- data.frame()
  for (i in 1:length(unique(dat$date))) {
    #print(i)
    
    # select day
    tmp <- dat[dat$date==unique(dat$date)[i],]
    
    # compute day schedule
    res <- day_scheduling(tmp, max_nboxes_hub, boxweight, bike_tour_length, hub_cut, avg_bike_speed)
    
    res_hub <- rbind(res_hub, res$day_hub)
    res_tour_schedule <- rbind(res_tour_schedule, res$day_tour_schedule)
    res_tour_stats <- rbind(res_tour_stats, res$day_tour_stats)
  }
  
  res_hub$xweight <- xweight
  res_hub$ndel <- ndel
  res_tour_schedule$xweight <- xweight
  res_tour_schedule$ndel <- ndel
  res_tour_stats$xweight <- xweight
  res_tour_stats$ndel <- ndel
  
  res_ALLscenarios_hub <- rbind(res_ALLscenarios_hub, res_hub)
  res_ALLscenarios_tour_scehdule <- rbind(res_ALLscenarios_tour_scehdule, res_tour_schedule)
  res_ALLscenarios_tour_stats <- rbind(res_ALLscenarios_tour_stats, res_tour_stats)
  
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

















