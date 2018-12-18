
# This code simulates two urban goods distribution systems: (1) truck-based delivery and (2) mobile depot-based delivery with cargo bikes
# Both systems faces a given demand scenario (set of selivery points located in the CBD of Singapore)

rm(list=ls())

library(ggplot2)
library(Imap) # gdist()
library(leaflet) # maps
library(geosphere)
library(dplyr)
library(TSP)
library(parallel) #
library(metaSEM) # vec2symMat()
library(adagio) # mknapsack()
library(cluster) # daisy() distance matrix for categorical variables
library(combinat) #combn()
library(maxmatching) #maxmatching()

input_data <- "data/raw_merged.csv"
input_distmatrix <- "data/dist_matrix.csv"
setwd("/Users/giacomodc/Dropbox/cargobikes_study")
source(paste0(getwd(),"/R/cargobikes_fn.R"))


### INPUT DATA
# delivery orders
raw <- read.csv(input_data, stringsAsFactors = F, header=T)
names(raw) <- c("UPS_tourID", "UPS_routeID", "date", "UPS_merge", "postcode", "start_time", "end_time", "lat", "lon", "weight", "n_del_merged", 
                "consignee_address1","consignee_address2","consignee_address3", "service_level", "UPS_departure_time", "UPS_stopID", "UPS_stop_type",
                "mapped_lat", "mapped_lon", "UPS_traveltime", "UPS_basetime", "UPS_traveldist", "new_start_time", "dwell_time", "weekday", "UPS_vehicle_type", 
                "UPS_CO2emissions")
raw <- raw[raw$UPS_stop_type=="end/start",] #eliminate extra rows added for depot stops at the start and end of tours
raw <- raw[,-grep("^UPS", names(raw))] #eliminate columns used to re-compute UPS tours
raw$start_time <- decimal_to_time(vtime=raw$start_time, date=raw$date)
raw$end_time <- decimal_to_time(vtime=raw$end_time, date=raw$date)
raw$dwell_time <- raw$dwell_time*60 #convert hours to minutes
raw$date <- as.Date(raw$date, format=datetime_format(raw$date))
raw$weight <- round(raw$weight, 1)
raw$nodeID <- as.character(1:nrow(raw))
raw$postcode <- as.character(raw$postcode)
raw[which(nchar(raw$postcode)==5),"postcode"] <- paste0("0", raw[which(nchar(raw$postcode)==5),"postcode"]) #if the postcode is 5-characters long, a "0" is attached at the start of string
raw$new_start_time <- decimal_to_time(vtime=raw$new_start_time, date=raw$date)
raw$priority <- ifelse(raw$service_level %in% c("Express", "Express Plus"), "high", "low")

# correct lat/lon
coord <- read.csv("data/postal_geoCoord_mapping.csv", stringsAsFactors = F, header=T)
coord[,c("pc_lat", "pc_lon")] <- do.call(rbind, strsplit(coord$Coord, ","))
coord <- coord[,c("PostalCode", "pc_lat", "pc_lon")]
names(coord)[1] <- "postcode"
coord[which(nchar(coord$postcode)==5),"postcode"] <- paste0("0", coord[which(nchar(coord$postcode)==5),"postcode"])
#table(raw$postcode %in% coord$postcode) check all postcodes are mapped in coord
raw <- merge(raw, coord, by="postcode")
raw$diffcoord <- sapply(1:nrow(raw), FUN=function(x) distm(raw[x,c("lon", "lat")], raw[x,c("pc_lon", "pc_lat")]))
raw[raw$diffcoord>100,c("lat", "lon")] <- raw[raw$diffcoord>100,c("pc_lat", "pc_lon")]
raw[,c("lat", "lon")] <- lapply(raw[,c("lat", "lon")], as.numeric)

# save data for dashboard
#raw[,] <- lapply(raw[, ], as.character)
#write.csv(raw[,c("date","lat", "lon", "consignee_address1", "consignee_address3", "postcode", "service_level", "weight", "dwell_time", "nodeID", "priority")], file="/Users/giacomodc/Dropbox/cargobikes_dashboard/data/data.csv", row.names = F)

# distance matrix
full_pdmat <- read.csv(input_distmatrix, stringsAsFactors = F, header=T, row.names = 1, check.names=F) # matrix of postcodes distances
rownames(full_pdmat) <- colnames(full_pdmat)
full_pdmat <- as.matrix(full_pdmat)



### INPUTS PARAMETERS
# depot
depot <- data.frame("lon"=103.967465, "lat"=1.331026, "postcode"="486064", "nodeID"="depot", stringsAsFactors = F) # 22 Changi South Ave 2, UPS House Singapore, Singapore

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
n_hubs <- 2 #number of hubs

# truck
truck_avg_speed <- 750 #(meters/min) average truck speed (==45 km/h)
truck_max_weight <- 600 #kg
truck_max_tour_length <- 500*1000 #meters
truck_max_tour_time <- 10*60 #minutes

# time
start_time <- "07:00:00"
express_time <- "12:00:00"

# demand scenarios
nrep = 50 #no. of days to simulate (per each scenario)
#vector_xweight = seq(from=0.5, to=3, by=0.5)
vector_xweight = c(0.5, 1, 2, 5)
vector_ndel = seq(from=25, to=1000, by=25)
  #nrep = 2
  #vector_xweight = c(0.5,1, 1.5)
  #vector_ndel = c(100, 300)
scenarios <- data.frame("ndel"=rep(vector_ndel, each=length(vector_xweight)), "xweight"=vector_xweight)




### RUN
numCores <- detectCores()


sim_fun <- function(k, sim=c("Y", "N")) {
    #sim="S"
    #k=36
    
  # generate demand scenarios
  xweight <- scenarios[k,"xweight"]
  ndel <- scenarios[k,"ndel"]
  dat <- generate_demand(data=raw, simulated=sim, xweight=xweight, ndel=ndel, nrep=nrep) # original data contains 63 days, ~120 deliveries per day (min: 57, max: 210)
  
  # perform scheduling
  res_bike <- list("hub"=data.frame(), "tour_schedule"=data.frame(), "tour_stats"=data.frame(), "vehicle_stats"=data.frame())
  res_truck <- list("tour_schedule"=data.frame(), "tour_stats"=data.frame(), "vehicle_stats"=data.frame())
 
  for (i in 1:length(unique(dat$date))) {
    #print(i)
    # select day
    tmp <- dat[dat$date==unique(dat$date)[i],]
    tmp <- tmp[order(tmp$postcode),] #order by postcode
    
    # postcodes distance matrix (asymmetric)
    postcodes <- c(unique(tmp$postcode), depot$postcode)
    pdmat <- full_pdmat[which(colnames(full_pdmat) %in% postcodes),which(colnames(full_pdmat) %in% postcodes)]
    
    # mode: bike
    tmp_adj <- adjust_demand(tmp, dwell_factor=bike_dwelltime, max_weight=boxweight) #adjust demand
    
      #tmp=tmp_adj
      #max_weight=boxweight
      #max_tour_length=bike_max_tour_length
      #max_tour_time=bike_max_tour_time
      #avg_speed=bike_avg_speed
      #hub="Y"
      #weight_priority=0
      #dist_calculation_method="googleAPI"
      
    res <- day_scheduling(tmp=tmp_adj, pdmat=pdmat, depot=depot, max_weight=boxweight, max_tour_length=bike_max_tour_length, 
                          max_tour_time=bike_max_tour_time, express_time, start_time, avg_speed=bike_avg_speed, hub="Y", 
                          max_nboxes_hub, n_hubs, weight_priority=0.2, dist_calculation_method="googleAPI") #simulate bikes
    #print("bike OK")
    res_bike$hub <- rbind(res_bike$hub, res$day_hub)
    res_bike$tour_schedule <- rbind(res_bike$tour_schedule, res$day_tour_schedule)
    res_bike$tour_stats <- rbind(res_bike$tour_stats, res$day_tour_stats)
    res_bike$vehicle_stats <- rbind(res_bike$vehicle_stats, res$day_vehicle_stats)
    
    
    # mode: truck
    tmp_adj <- adjust_demand(tmp, dwell_factor=1, max_weight=truck_max_weight)
    
      #tmp=tmp_adj
      #max_weight=truck_max_weight
      #max_tour_length=truck_max_tour_length 
      #max_tour_time=truck_max_tour_time
      #avg_speed=truck_avg_speed
      #hub="N"
      #weight_priority=0
      #dist_calculation_method="googleAPI"
    
    res <- day_scheduling(tmp=tmp_adj, pdmat=pdmat, depot=depot, max_weight=truck_max_weight, max_tour_length=truck_max_tour_length, 
                          max_tour_time=truck_max_tour_time, express_time, start_time,
                          avg_speed=truck_avg_speed, hub="N", weight_priority=0.2, dist_calculation_method="googleAPI")
    #print("truck OK")
    res_truck$tour_schedule <- rbind(res_truck$tour_schedule, res$day_tour_schedule)
    res_truck$tour_stats <- rbind(res_truck$tour_stats, res$day_tour_stats)
    res_truck$vehicle_stats <- rbind(res_truck$vehicle_stats, res$day_vehicle_stats)
    
  }
  
  # add demand scenario features
  res_bike <- mapply(cbind, res_bike, "xweight"=xweight, SIMPLIFY=F)
  res_bike <- mapply(cbind, res_bike, "ndel"=ndel, SIMPLIFY=F)
  res_bike <- mapply(cbind, res_bike, "scenario"=k, SIMPLIFY=F)
  res_truck <- mapply(cbind, res_truck, "xweight"=xweight, SIMPLIFY=F)
  res_truck <- mapply(cbind, res_truck, "ndel"=ndel, SIMPLIFY=F)
  res_truck <- mapply(cbind, res_truck, "scenario"=k, SIMPLIFY=F)
  
  # save results
  return(list("res_bike"=res_bike, "res_truck"=res_truck))
  
  }



system.time({ #simulated
  res_scenarios <- mclapply(1:nrow(scenarios), sim_fun, sim="Y", mc.cores = numCores)
})
save(res_scenarios, file=paste0("results/SIMres_",Sys.Date(),".RData"))


system.time({ #real
  res_scenarios <- mclapply(1, sim_fun, sim="N", mc.cores = numCores)
})
save(res_scenarios, file=paste0("results/REALres_",Sys.Date(),".RData"))



