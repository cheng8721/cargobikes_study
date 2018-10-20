

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






bike_assignment <- function(cut, tmp_dat, boxweight, bike_tour_length, hub_centers) {
  for (i in 1:max(cut)) {
    if (all(is.na(tmp_dat[which(cut==i),"tourID"]))) { #cluster not yet assigned
      if (sum(tmp_dat[which(cut==i),"weight"]) <= boxweight) { #tot cluster weight < bike capacity
        if (nrow(tmp_dat[which(cut==i),])>1) tour_length <- bike_tsp(tmp_tour=tmp_dat[which(cut==i),], hub_centers)$tour_length else tour_length <- 0
        if (tour_length <=  bike_tour_length) { #tot tour length < max allowed length
          tour <<- tour +1
          tmp_dat[which(cut==i),"tourID"] <- tour #assign bike
        }
        
      }
    }
  }
  return(tmp_dat$tourID)
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
  
  tour <- solve_TSP(atsp, method="nearest_insertion")
  
  return(list("path_labels"=c("hub", labels(cut_tour(tour, HUB)), "hub"), 
              "tour_length"=tour_length(tour)))
  
}




day_scheduling <- function(tmp, max_nboxes_hub, boxweight, tot_load_share, bike_tour_length, hub_cut, avg_bike_speed) {
##return delivery orders for a day with hub and bike assignments-------------------------------------
 #
 #
 #
 #
 #
 #
 #
 #
 #
 #
 #
 #
  tour <- 0
  tmp$hub <- tmp$tourID <- NA

  day_res_tour <- data.frame()
  
  #leaflet(data=tmp) %>% #plot all delivery points for a given day
  #  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE) ) %>%
  #  addCircleMarkers(~lon, ~lat, stroke = FALSE, fillOpacity = 0.8, radius=5)
  
  # distance matrix
  dmat <- tmp[,c("lon", "lat")] 
  dmat <- distm(dmat)
  dmat <- as.dist(dmat) #in meters
  
  # compute hierarchical clustering
  hc <- hclust(dmat, method="complete")
  
  # hub assignment
  tmp$hub <- tmp_cut <- cutree(hc, h=hub_cut)
  hub_centers <- merge(aggregate(lat ~ hub, data = tmp, FUN = mean), aggregate(lon ~ hub, data = tmp, FUN = mean))
  hub_centers$date <- unique(tmp$date)
  
  # tour assignment
  tmp$tourID <- bike_assignment(cut=tmp_cut, tmp_dat=tmp, boxweight, bike_tour_length, hub_centers) #initial bike assignment
  while (any(is.na(tmp$tourID))) {
    tmp_cut <- cutree(hc, k=max(tmp_cut)+1)
    #print(max(tmp_cut))
    tmp$tourID <- bike_assignment(cut=tmp_cut, tmp_dat=tmp, boxweight, bike_tour_length, hub_centers)
  }
  
  # compute hub refill trips
  hub_centers$refill_trips <- NA
  for (j in 1:max(tmp$hub)) {
    hub_centers[hub_centers$hub==j,"refill_trips"] <- ceiling(length(unique(tmp[tmp$hub==j,"bike"]))/max_nboxes_hub)
  }
  # hub_centers is an output
  
  
  ### TSP
  tmp$tourID <- as.character(tmp$tourID)
  tourID <- 0
  day_res_tour <- data.frame()
  for (j in 1:length(unique(tmp$tourID))) {
    tmp_tour <- tmp[tmp$tourID==unique(tmp$tourID)[j],]
    #if (nrow(tmp_tour)==1) path_labels <- c("hub", tmp_tour$nodeID, "hub") else {
    #  path_labels <- bike_tsp(tmp_tour, hub_centers)$path_labels
    #}
    tour_tsp <- bike_tsp(tmp_tour, hub_centers)
    
    # print results
    tourID <- tourID+1
    res <- data.frame("nodeID"=tour_tsp$path_labels, "lat"=NA, "lon"=NA, "tourID"=tourID, "tourSEQ"=1:length(tour_tsp$path_labels), 
                      "date"=unique(tmp_tour$date), "hub"=unique(tmp_tour$hub), "tour_travel_distance"=tour_tsp$tour_length,
                      "tour_travel_time"=(tour_tsp$tour_length/avg_bike_speed), "tour_dwell_time"=sum(tmp_tour$dwell_time), "tour_no_express"=sum(tmp_tour$service_level!="Saver"), stringsAsFactors = F)
    res[res$nodeID=="hub",c("lat", "lon")] <- hub_centers[hub_centers$hub==unique(tmp_tour$hub),c("lat", "lon")]
    for (k in 1:nrow(res)) {
      if (res[k,"nodeID"]=="hub") next else{
        res[k,c("lat", "lon")] <- dat[dat$nodeID==res[k,"nodeID"],c("lat", "lon")]
      }
    }
    day_res_tour <- rbind(day_res_tour, res)
  }
  
  ### FLEET SIZE
  #summary_tours_day <- unique(day_res_tour[,c("date", "hub", "tourID", "tour_travel_distance", "tour_travel_time", "tour_dwell_time", "tour_no_express")])
  #summary_tours_day <- summary_tours_day[order(-summary_tours_day$tour_no_express),]
  #summary_tours_day$tot_tour_time <- summary_tours_day$tour_travel_time + summary_tours_day$tour_dwell_time
  
  return("day_res_hub"=hub_centers, "day_res_tour"=day_res_tour)
  
}


