

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






bike_assignment <- function(tmp_cut, tmp, boxweight, bike_tour_length, hub_centers) {
  for (k in 1:max(tmp_cut)) {
    if (all(is.na(tmp[which(tmp_cut==k),"tourID"]))) { #cluster not yet assigned
      if (sum(tmp[which(tmp_cut==k),"weight"]) <= boxweight) { #tot cluster weight < bike capacity
        if (nrow(tmp[which(tmp_cut==k),])>1) tourlength <- bike_tsp(tmp_tour=tmp[which(tmp_cut==k),], hub_centers)$tour_length else tourlength <- 0
        if (tourlength <=  bike_tour_length) { #tot tour length < max allowed length
          tourID <- ifelse(all(is.na(tmp$tourID)), 1, max(tmp$tourID, na.rm=T)+1)
          tmp[which(tmp_cut==k),"tourID"] <- tourID #assign bike
        }
      }
    }
  }
  return(tmp$tourID)
  }





bike_tsp <- function(tmp_tour, hub_centers) {
  
  # compute distance matrix
  dmat <- rbind(tmp_tour[,c("lon", "lat")], hub_centers[hub_centers$hub==unique(tmp_tour$hub),c("lon", "lat")])
  dmat <- distm(dmat)
  dmat <- as.dist(dmat) #in meters
  tsp <- TSP(dmat, labels=c(tmp_tour$nodeID, "hub"))
  
  m <- as.matrix(tsp)
  hub <- which(labels(tsp)=="hub")
  
  atsp <- ATSP(m[-hub,-hub])
  atsp <- insert_dummy(atsp, label="HUB")
  HUB <- which(labels(atsp)=="HUB")
  atsp[HUB,] <- c(m[-hub,hub], 0)
  atsp[,HUB] <- c(m[hub,-hub], 0)
  
  resTOUR <- solve_TSP(atsp, method="nearest_insertion")
  
  return(list("path_labels"=c("hub", labels(cut_tour(resTOUR, HUB)), "hub"), 
              "tour_length"=tour_length(resTOUR)))
  
}




day_scheduling <- function(tmp, max_nboxes_hub, boxweight, bike_tour_length, hub_cut, avg_bike_speed) {
###return delivery orders for a day with hub and bike assignments-------------------------------------
  #tmp              = single data data
  #max_nboxes_hub   = max no. of boxes a hub can contain
  #boxweight        = max weight a box can carry
  #bike_tour_length = max length of single bike tour
  #hub_cut          = 
  #avg_bike_speed   = average bike travel speed

  tmp$hub <- NA
  
  #leaflet(data=tmp) %>% #plot all delivery points for a given day
  #  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE) ) %>%
  #  addCircleMarkers(~lon, ~lat, stroke = FALSE, fillOpacity = 0.8, radius=5)
  
  # distance matrix
  dmat <- tmp[,c("lon", "lat")] 
  dmat <- distm(dmat)
  dmat <- as.dist(dmat) #in meters
  
  # hierarchical clustering
  hc <- hclust(dmat, method="complete")
  
  # hub assignment
  tmp$hub <- tmp_cut <- cutree(hc, h=hub_cut)
  hub_centers <- merge(aggregate(lat ~ hub, data = tmp, FUN = mean), aggregate(lon ~ hub, data = tmp, FUN = mean))
  hub_centers$date <- unique(tmp$date)
  
  # tour assignment
  tmp$tourID <- NA
  while (any(is.na(tmp$tourID))) {
    tmp$tourID <- bike_assignment(tmp_cut, tmp, boxweight, bike_tour_length, hub_centers)
    #print(max(tmp_cut))
    if (max(tmp_cut)<nrow(tmp)) tmp_cut <- cutree(hc, k=max(tmp_cut)+1) else break
  }
  
  # compute hub refill trips
  hub_centers$refill_trips <- NA
  for (j in 1:max(tmp$hub)) {
    hub_centers[hub_centers$hub==j,"refill_trips"] <- ceiling(length(unique(tmp[tmp$hub==j,"tourID"]))/max_nboxes_hub)
  }
  # hub_centers is an output
  
  
  ### TSP
  day_tour_schedule <- data.frame()
  day_tour_stats <- data.frame()
  tmp$tourID <- as.character(tmp$tourID)
  for (j in 1:length(unique(tmp$tourID))) {
    tmp_tour <- tmp[tmp$tourID==unique(tmp$tourID)[j],]
    if (nrow(tmp_tour)>1) tour_tsp <- bike_tsp(tmp_tour, hub_centers) else tour_tsp <- list("path_labels"=tmp_tour$nodeID, "tour_length"=0)
    
    # save results
    tour_schedule <- data.frame("date"=unique(tmp_tour$date),
                                "hubID"=unique(tmp_tour$hub),
                                "tourID"=j,
                                "nodeID"=tour_tsp$path_labels,
                                "tourSEQ"=1:length(tour_tsp$path_labels), stringsAsFactors = F
                                )
    tour_stats <- data.frame("date"=unique(tmp_tour$date),
                             "hubID"=unique(tmp_tour$hub),
                             "tourID"=j,
                             "tour_travel_distance"=tour_tsp$tour_length,
                             "hub_travel_distance"=(nrow(hub_centers)+sum(hub_centers$refill_trips))*18000*2,
                             "tour_travel_time"=(tour_tsp$tour_length/avg_bike_speed),
                             "hub_travel_time"=(nrow(hub_centers)+sum(hub_centers$refill_trips))*20*2,
                             "tour_dwell_time"=sum(tmp_tour$dwell_time),
                             "tour_no_deliveries"=nrow(tmp_tour),
                             "tour_no_express"=sum(tmp_tour$service_level!="Saver"), stringsAsFactors = F
                             )
    
    day_tour_schedule <- rbind(day_tour_schedule, tour_schedule)
    day_tour_stats <- rbind(day_tour_stats, tour_stats)
  }
  
  ### FLEET SIZE
  #summary_tours_day <- unique(day_res_tour[,c("date", "hub", "tourID", "tour_travel_distance", "tour_travel_time", "tour_dwell_time", "tour_no_express")])
  #summary_tours_day <- summary_tours_day[order(-summary_tours_day$tour_no_express),]
  #summary_tours_day$tot_tour_time <- summary_tours_day$tour_travel_time + summary_tours_day$tour_dwell_time
  
  return(list("day_hub"=hub_centers, "day_tour_schedule"=day_tour_schedule, "day_tour_stats"=day_tour_stats))
  
}


