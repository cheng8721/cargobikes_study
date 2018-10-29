
# This code simulates two urban goods distribution systems: (1) truck-based delivery and (2) mobile depot-based delivery with cargo bikes
# Both systems faces a given demand scenario (set of selivery points located in the CBD of Singapore)

rm(list=ls())

library(ggplot2)
library(Imap) # gdist()
library(leaflet) # maps
library(geosphere)
library(dplyr)
library(TSP)

input_data <- "data/raw.csv"
source(paste0(getwd(),"/R/cargobikes_fn.R"))


### INPUT DATA
raw <- read.csv(input_data, stringsAsFactors = F, header=T)
names(raw) <- c("month", "weekend_date", "date", "pkg_barcode_num", "stop_start_time", "stop_end_time", "lat", "lon", "commercial_residential", 
                "dispatch_loop_position_num", "stop_type", "consignee_address1","consignee_address2","consignee_address3", "consignee_postcode",
                "service_level_code", "service_level", "weight", "delivery_info", "dwell_time")
raw$stop_start_time <- as.POSIXct(paste(raw$date, "00:00:00"), format = "%d/%m/%y %H:%M:%S") + 3600*raw$stop_start_time #needed only to compute dwell time
raw$stop_end_time <- as.POSIXct(paste(raw$date, "00:00:00"), format = "%d/%m/%y %H:%M:%S") + 3600*raw$stop_end_time #needed only to compute dwell time
raw$dwell_time <- raw$dwell_time*60 #convert hours to minutes
raw$date <- as.Date(raw$date, format="%d/%m/%y")
raw$weight <- round(raw$weight, 1)
raw$nodeID <- as.character(1:nrow(raw))



### INPUTS PARAMETERS
# depot
depot_loc <- data.frame("lon"=103.967465, "lat"=1.331026) # 22 Changi South Ave 2, UPS House Singapore, Singapore

# cargo bike
max_boxweight <- 205 #180 kg in the box + 25 kg front bike
tot_load_share <- 0.7 #70% of max filling
boxweight <- max_boxweight*tot_load_share #max weight carried
bike_max_tour_length <- 40000 #(in meters) max bike tour length is 50km, we assume 40 km since we use euclidean distances
bike_max_tour_time <- 10*60 #minutes
bike_avg_speed <- 250 #(meters/min) average bike speed
bike_dwelltime <- 0.6 #if ==1 then the dwell time bike == dwell time truck; if with bike the dwell time is reduced by 50%, then bike_dwelltime_frac <- 0.5

# hub
max_nboxes_hub <- 9 #max number of boxes carried by hub
hub_cut <- 2000 #(in meters) max dist allowed betwwen any 2 points within same hub cluster

# truck
truck_avg_speed <- 750 #(meters/min) average truck speed (==45 km/h)
truck_max_weight <- 600 #kg
truck_max_tour_length <- 500*1000 #meters
truck_max_tour_time <- 10*60 #minutes

# demand scenarios
nrep = 2 #no. of days to simulate (per each scenario)
vector_xweight = seq(from=0.5, to=4, by=0.25)
#vector_xweight = c(0.5,1)
vector_ndel = seq(from=50, to=1000, by=50)
#vector_ndel = c(100, 300)
scenarios <- data.frame("ndel"=rep(vector_ndel, each=length(vector_xweight)), "xweight"=vector_xweight)



### RUN
res_ALLscenarios_bike <- list()
res_ALLscenarios_truck <- list()

for (k in 1:nrow(scenarios)) {
  
  print(k)
  
  # generate demand scenarios
  xweight <- scenarios[k,"xweight"]
  ndel <- scenarios[k,"ndel"]
  dat <- generate_demand(data=raw, xweight=xweight, simulated="Y", ndel=ndel, nrep=nrep) # original data contains 64 days, ~300 deliveries per day
  
  # perform scheduling
  res_bike <- list("hub"=data.frame(), "tour_schedule"=data.frame(), "tour_stats"=data.frame())
  res_truck <- list("tour_schedule"=data.frame(), "tour_stats"=data.frame())
 
  for (i in 1:length(unique(dat$date))) {
    
    # select day
    tmp <- dat[dat$date==unique(dat$date)[i],]
    
    # mode: bike
    tmp_adj <- adjust_demand(tmp, dwell_factor=bike_dwelltime, max_weight=boxweight) #adjust demand
    res <- day_scheduling(tmp=tmp_adj, depot_loc=depot_loc, max_weight=boxweight, max_tour_length=bike_max_tour_length, max_tour_time=bike_max_tour_time, avg_speed=bike_avg_speed, hub="Y", max_nboxes_hub, hub_cut) #simulate bikes
    res_bike$hub <- rbind(res_bike$hub, res$day_hub)
    res_bike$tour_schedule <- rbind(res_bike$tour_schedule, res$day_tour_schedule)
    res_bike$tour_stats <- rbind(res_bike$tour_stats, res$day_tour_stats)
    
    # mode: truck
    tmp_adj <- adjust_demand(tmp, dwell_factor=1, max_weight=truck_max_weight)
    res <- day_scheduling(tmp=tmp_adj, depot_loc=depot_loc, max_weight=truck_max_weight, max_tour_length=truck_max_tour_length, max_tour_time=truck_max_tour_time, avg_speed=truck_avg_speed, hub="N")
    res_truck$tour_schedule <- rbind(res_truck$tour_schedule, res$day_tour_schedule)
    res_truck$tour_stats <- rbind(res_truck$tour_stats, res$day_tour_stats)
    
  }
  
  # add demand scenario features
  res_bike <- mapply(cbind, res_bike, "xweight"=xweight, SIMPLIFY=F)
  res_bike <- mapply(cbind, res_bike, "ndel"=ndel, SIMPLIFY=F)
  res_bike <- mapply(cbind, res_bike, "scenario"=k, SIMPLIFY=F)
  res_truck <- mapply(cbind, res_truck, "xweight"=xweight, SIMPLIFY=F)
  res_truck <- mapply(cbind, res_truck, "ndel"=ndel, SIMPLIFY=F)
  res_truck <- mapply(cbind, res_truck, "scenario"=k, SIMPLIFY=F)
  
  # save results
  res_ALLscenarios_bike[[k]] <- res_bike
  res_ALLscenarios_truck[[k]] <- res_truck
}



### SAVE
save(res_ALLscenarios_bike, file="results/res_bike.RData")
save(res_ALLscenarios_truck, file="results/res_truck.RData")




