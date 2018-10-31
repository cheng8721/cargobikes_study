

generate_demand <- function(data, xweight, simulated, ndel, nrep) {
##return set of deliveries for nrep number of days-------------------------------------
 #data      = set of deliveries from which to sample from
 #xweight   = multiplicative factor for weights (e.g. if ==2 then weight=weight*2)
 #simulated = if "N" then original data is returned, if "Y" simulated days are returned
 #ndel      = if simulated, number of delivery points per day (sampled from data)
 #nrep      = if simulated, number of days generated
##
  data$weight <- data$weight*xweight
  if (simulated=="N") return(data) else {
    sim_data <- data[replicate(nrep, sample(1:nrow(data), size=ndel, replace=F)),c("lat", "lon", "commercial_residential", "consignee_address1", "consignee_address3", "consignee_postcode", "service_level", "weight", "delivery_info", "dwell_time", "nodeID")]
    date <- rep(as.Date(as.Date("2010-9-06"):(as.Date("2010-9-06")+nrep-1), origin="1970-01-01"), each=ndel)
    sim_data <- cbind(date, sim_data)
    return(sim_data)
  }
}

adjust_demand <- function(tmp, dwell_factor, max_weight) {
### adjust demand according to given mode
  # tmp
  # dwell_factor
  # max_weight
###
  
  # adjust dwell time
  tmp$dwell_time <- tmp$dwell_time*dwell_factor
  
  # adjust set of delivery points
  while (any(tmp$weight>max_weight)) { #if the weight of a single delivery exceed mode capacity, the deliverty point is split in 2
    excess_weight <- tmp[tmp$weight>max_weight,"weight"][1] - max_weight #find excess weight
    tmp[nrow(tmp)+1,] <- tmp[tmp$weight>max_weight,][1,] #add new delivery point
    tmp[tmp$weight>max_weight,"weight"][1] <- max_weight #adjust original del point to max capacity
    tmp[nrow(tmp),"weight"] <- excess_weight #adjust weight of new delivery point
  }
  
  return(tmp)
}




tour_assignment <- function(tmp_cut, tmp, max_weight, max_tour_length, max_tour_time, start_tour, avg_speed, hub) {
  stats <- data.frame()
  schedule <- data.frame()
  for (k in 1:max(tmp_cut)) {
    tmp_tour <- tmp[which(tmp_cut==k),]
    if (all(is.na(tmp_tour$tourID))) { #cluster not yet assigned
      if (sum(tmp_tour$weight) <= max_weight) { #tot cluster weight < weight capacity
        
        #select start_tour location (depot or hub)
        if (hub=="N") start <- start_tour[,c("lon", "lat")] else start <- start_tour[start_tour$hubID==unique(tmp_tour$hubID),c("lon", "lat")] 
          
        #compute tour length and time
        tour <- solve_tsp(tmp_tour, start)
        tourlength <- tour$tour_length 
        tourtraveltime <- tourlength/avg_speed 
        tourdwelltime <- sum(tmp_tour$dwell_time)
        tourtottime <- tourtraveltime + tourdwelltime

        if (tourlength <=  max_tour_length & tourtottime <= max_tour_time) { #tot tour length <= max allowed length & tot tour time <= max allowed time
          tourID <- ifelse(all(is.na(tmp$tourID)), 1, max(tmp$tourID, na.rm=T)+1)
          tmp[which(tmp_cut==k),"tourID"] <- tourID #assign cluster to tour ID
          tour_stats <- data.frame("hubID"=ifelse(hub=="N", NA, unique(tmp_tour$hubID)),
                                   "tourID"=tourID,
                                   "tour_travel_distance"=tourlength,
                                   "tour_travel_time"=tourtraveltime,
                                   "tour_dwell_time"=tourdwelltime,
                                   "tour_tot_time"=tourtottime,
                                   "tour_ndeliveries"=nrow(tmp_tour),
                                   "tour_n_express"=sum(tmp_tour$service_level %in% c("Express", "Express Plus")), stringsAsFactors = F)
          stats <- rbind(stats, tour_stats)
          tour_schedule <- data.frame("hubID"=ifelse(hub=="N", NA, unique(tmp_tour$hubID)),
                                      "tourID"=tourID,
                                      "nodeID"=tour$path_labels,
                                      "tourSEQ"=1:length(tour$path_labels), stringsAsFactors = F)
          schedule <- rbind(schedule, tour_schedule)
        }
      }
    }
  }
  return(list("assignment"=tmp$tourID, "schedule"=schedule,"stats"=stats))
  }





solve_tsp <- function(tmp_tour, start) {
  
  # distance matrix
  dmat <- rbind(tmp_tour[,c("lon", "lat")], start[,c("lon", "lat")])
  dmat <- distm(dmat)
  dmat <- as.dist(dmat) #in meters
  tsp <- TSP(dmat, labels=c(tmp_tour$nodeID, "start"))
  
  # solve TSP
  tour <- solve_TSP(tsp, method="nearest_insertion")
  START <- which(labels(tsp)=="start")
  
  return(list("path_labels"=c("hub", labels(cut_tour(tour, START)), "hub"), 
              "tour_length"=tour_length(tour)))
}
  
  #m <- as.matrix(tsp)
  #hub <- which(labels(tsp)=="hub")
  
  #atsp <- ATSP(m[-hub,-hub])
  #atsp <- insert_dummy(atsp, label="HUB")
  #HUB <- which(labels(atsp)=="HUB")
  #atsp[HUB,] <- c(m[-hub,hub], 0)
  #atsp[,HUB] <- c(m[hub,-hub], 0)
  
  #resTOUR <- solve_TSP(atsp, method="nearest_insertion")
  
  #return(list("path_labels"=c("hub", labels(cut_tour(resTOUR, HUB)), "hub"), 
  #            "tour_length"=tour_length(resTOUR)))
  





day_scheduling <- function(tmp, depot_loc, max_weight, max_tour_length, max_tour_time, avg_speed, hub, max_nboxes_hub, hub_cut) {
###schedule delivery tours for bikes and assign them to hubs 
  #tmp              = demand for single day
  #depot_loc
  #max_weight
  #max_tour_length
  #avg_speed
  #hub
  #max_nboxes_hub
  #hub_cut
###-------------------------------------

  RESULTS <- list()
  
  # distance matrix
  dmat <- tmp[,c("lon", "lat")] 
  dmat <- distm(dmat)
  dmat <- as.dist(dmat) #in meters
  
  # hierarchical clustering
  hc <- hclust(dmat, method="complete")
  
  # hub assignment
  if (hub=="Y") {
    tmp$hubID <- tmp_cut <- cutree(hc, h=hub_cut)
    hub_centers <- merge(aggregate(lat ~ hubID, data = tmp, FUN = mean), aggregate(lon ~ hubID, data = tmp, FUN = mean))
    start_tour <- hub_centers
  } else {
    tmp_cut <- rep(1, nrow(tmp))
    start_tour <- depot_loc
  }
  
  # tour assignment
  tmp$tourID <- NA
  tour_schedule <- data.frame()
  tour_stats <- data.frame()
  while (any(is.na(tmp$tourID))) {
    tour <- tour_assignment(tmp_cut, tmp, max_weight, max_tour_length, max_tour_time, start_tour, avg_speed, hub)
    tmp$tourID <- tour$assignment
    tour_schedule <- rbind(tour_schedule, tour$schedule)
    tour_stats <- rbind(tour_stats, tour$stats)
    #print(max(tmp_cut))
    if (max(tmp_cut)<nrow(tmp)) tmp_cut <- cutree(hc, k=max(tmp_cut)+1) else break
  }
  tour_schedule$date <- tour_stats$date <- unique(tmp$date)
  
  # hub trips
  if (hub=="Y") {
    hub_centers$date <- unique(tmp$date)
    
    # compute hub refill trips and respective travel distance and time from depot
    hub_centers$trips <- aggregate(tourID ~ hubID, data=tmp, FUN= function(x) {return(ceiling(length(unique(x))/max_nboxes_hub))})[,2]
    hub_centers$depot_dist <- apply(hub_centers[,c("lon", "lat")], MARGIN=1, FUN= function(x){distm(x, depot_loc)}) #meters
    hub_centers$tot_travel_dist <- hub_centers$depot_dist*hub_centers$trips*2 #meters
    hub_centers$tot_travel_time <- hub_centers$tot_travel_dist/750 #minutes (750 is the avg truck speed in meters/min --CHECK!)
    RESULTS[["day_hub"]] <- hub_centers
  }
  
  RESULTS[["day_tour_schedule"]] <- tour_schedule
  RESULTS[["day_tour_stats"]] <- tour_stats
  
  return(RESULTS)
  
  ### FLEET SIZE
  #summary_tours_day <- unique(day_res_tour[,c("date", "hub", "tourID", "tour_travel_distance", "tour_travel_time", "tour_dwell_time", "tour_no_express")])
  #summary_tours_day <- summary_tours_day[order(-summary_tours_day$tour_no_express),]
  #summary_tours_day$tot_tour_time <- summary_tours_day$tour_travel_time + summary_tours_day$tour_dwell_time
  
}







#leaflet(data=tmp_tour) %>% #plot all delivery points for a given day
#  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE) ) %>%
#  addCircleMarkers(~lon, ~lat, stroke = FALSE, fillOpacity = 0.8, radius=5) %>%
#  addPolylines(data=results_bikes[results_bikes$tourID==i,], lat = ~lat, lng = ~lon, weight=2, color = "red") 


