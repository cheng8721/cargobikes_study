
datetime_format <- function(date) ifelse(all(grepl("/", date)), "%d/%m/%Y", "%Y-%m-%d")

decimal_to_time <- function(vtime, date) {
  format <- paste(datetime_format(date), "%H:%M:%S")
  as.POSIXct(paste(date, "00:00:00"), format = format) + 3600*vtime
}




generate_demand <- function(data, simulated=c("Y", "N"), xweight=NULL, ndel=NULL, nrep=NULL) {
  ##return set of deliveries for nrep number of days-------------------------------------
  #data      = set of deliveries from which to sample from
  #xweight   = multiplicative factor for weights (e.g. if ==2 then weight=weight*2)
  #simulated = if "N" then original data is returned, if "Y" simulated days are returned
  #ndel      = if simulated, number of delivery points per day (sampled from data)
  #nrep      = if simulated, number of days generated
  
  if (simulated=="N") {
    return(data[,c("date","lat", "lon", "consignee_address1", "consignee_address3", "postcode", "service_level", "weight", "dwell_time", "nodeID", "priority")])
  } else {
    data$weight <- data$weight*xweight
    sim_data <- data[replicate(nrep, sample(1:nrow(data), size=ndel, replace=F)),c("lat", "lon", "consignee_address1", "consignee_address3", "postcode", "service_level", "weight", "dwell_time", "nodeID", "priority")]
    date <- rep(as.Date(as.Date("2010-9-06"):(as.Date("2010-9-06")+nrep-1), origin="1970-01-01"), each=ndel)
    date <- as.character(date, stringsAsFactors = F)
    sim_data <- cbind.data.frame(date, sim_data, stringsAsFactors = FALSE)
    return(sim_data)
  }
}



adjust_demand <- function(tmp, dwell_factor, max_weight) {
  ### adjust demand according to a given mode
  
  # adjust dwell time
  tmp$dwell_time <- tmp$dwell_time*dwell_factor
  
  # adjust weights which exceed the mode capacity
  while (any(tmp$weight>max_weight)) { #if the weight of a single delivery exceed mode capacity, the deliverty point is split in 2
    excess_weight <- tmp[tmp$weight>max_weight,"weight"][1] - max_weight #find excess weight
    excess_dwell_time <- tmp[tmp$weight>max_weight,"dwell_time"][1]*(excess_weight/tmp[tmp$weight>max_weight,"weight"][1])
    
    tmp[nrow(tmp)+1,] <- tmp[tmp$weight>max_weight,][1,] #add new delivery
    tmp[nrow(tmp),"weight"] <- excess_weight #adjust weight of new delivery point
    tmp[nrow(tmp),"dwell_time"] <- excess_dwell_time
    tmp[nrow(tmp),"nodeID"] <- as.character(max(as.integer(tmp$nodeID))+1)
    
    tmp[tmp$weight>max_weight,c("weight", "dwell_time")][1,] <- c(max_weight, tmp[tmp$weight>max_weight,"dwell_time"][1]-excess_dwell_time)
  }
  return(tmp)
  }




tour_assignment <- function(tmp_cut, tmp, pdmat, max_weight, max_tour_length, max_tour_time, max_tour_time_express, start_tour, avg_speed, hub, dist_calculation_method, tour_stats, tour_schedule) {
  
  OLDtourID <- tmp$tourID
  tmp$tourID <- tmp_cut #tmp need to be updated with the "proposed" tour assignment
  tourID_list <- as.character(unique(tmp_cut[which(is.na(OLDtourID))]))
  
  ts <- find_feasible_tours(tourID_list, tmp, max_weight, max_tour_time, max_tour_time_express, max_tour_length, hub, start_tour, depot, dist_calculation_method, pdmat, avg_speed)
  
  if (is.null(ts)) return(list("assignment"=OLDtourID, "stats"=tour_stats, "schedule"=tour_schedule))
  
  # update tourID
  startID <- ifelse(all(is.na(OLDtourID)), 1, max(OLDtourID, na.rm=T)+1)
  tourID_map <- data.frame("oldID"=ts$tourID, "newID"=startID:(startID+nrow(ts)-1))
  tmp[!(tmp$tourID %in% ts$tourID),"tourID"] <- NA #set to NA the tours that either were found feasible in previous iterations or that are not feasible
  tmp$tourID <- sapply(1:length(tmp$tourID), function(x) if (!is.na(tmp[x,"tourID"])) tourID_map[tourID_map$oldID==tmp[x,"tourID"],"newID"] else NA ) #update the tourID according to tourID_map
  tmp[which(!is.na(OLDtourID)),"tourID"] <- OLDtourID[!is.na(OLDtourID)]
  
  # update tour_stats
  ts$tourID <- sapply(1:nrow(ts), function(x) tourID_map[tourID_map$oldID==ts[x,"tourID"],"newID"]) #update tourIDs in the new tour_stats
  if (nrow(tour_stats)==0) tour_stats <- ts[,-which(names(ts)=="tour_labels")] else {
    tour_stats <- rbind(tour_stats, ts[,names(tour_stats)]) #merge the old and new tour_stats
  }
  
  # update tour_schedule
  new_tour_schedule <- data.frame()
  for (x in 1:nrow(ts)) new_tour_schedule <- rbind(new_tour_schedule, data.frame("nodeID"=unlist(strsplit(ts[x,"tour_labels"], "_")), "hubID"=ts[x,"hubID"], "tourID"=ts[x,"tourID"], "tourSEQ"=1:length(unlist(strsplit(ts[x,"tour_labels"], "_"))), stringsAsFactors = F ))
  if (nrow(tour_schedule)==0) tour_schedule <- new_tour_schedule else {
    tour_schedule <- rbind(tour_schedule, new_tour_schedule[,names(tour_schedule)])
  }
  
  return(list("assignment"=tmp$tourID, "stats"=tour_stats, "schedule"=tour_schedule))
}





find_feasible_tours <- function(tourID_list, tmp, max_weight, max_tour_time, max_tour_time_express, max_tour_length, hub, start_tour, depot, dist_calculation_method, pdmat, avg_speed) {
  
  ts <- data.frame("tourID"=tourID_list, stringsAsFactors = F) # initialize tour_stats
  ts$tour_tot_weight <- sapply(1:nrow(ts), function(x) sum(tmp[tmp$tourID %in% as.numeric(unlist(strsplit(ts[x,"tourID"], "_"))),"weight"]))
  ts$tour_dwell_time <- sapply(1:nrow(ts), function(x) sum(tmp[tmp$tourID %in% as.numeric(unlist(strsplit(ts[x,"tourID"], "_"))),"dwell_time"]))
  ts$tour_ndeliveries <- sapply(1:nrow(ts), function(x) sum(tmp$tourID %in% as.numeric(unlist(strsplit(ts[x,"tourID"], "_")))))
  ts$tour_n_express <- sapply(1:nrow(ts), function(x) sum(tmp[tmp$tourID %in% as.numeric(unlist(strsplit(ts[x,"tourID"], "_"))),"priority"]=="high"))
  ts$hubID <- sapply(1:nrow(ts), function(x) unique(tmp[tmp$tourID %in% as.numeric(unlist(strsplit(ts[x,"tourID"], "_"))),"hubID"]))
  ts$date <- unique(tmp$date)
  
  feasible_tours <- (ts[,"tour_tot_weight"]<=max_weight) & (ts[,"tour_dwell_time"]<=ifelse(ts[,"tour_n_express"]==0, max_tour_time, max_tour_time_express))
  if (any(feasible_tours)) ts <- ts[feasible_tours,] else return(NULL)
  
  tsp_sol <- sapply(1:nrow(ts), function(x) {
    
    tmp_tour <- tmp[tmp$tourID %in% as.numeric(unlist(strsplit(ts[x,"tourID"], "_"))),]
    
    tmp_tour_withstart <- tmp_tour
    if (hub=="N") {
      start <- "depot"
      tmp_tour_withstart[nrow(tmp_tour_withstart)+1,c("lat", "lon", "nodeID", "postcode")] <- start_tour[,c("lat", "lon", "nodeID", "postcode")]
    } else {
      start <- start_tour[start_tour$hubID==unique(tmp_tour$hubID),"nodeID"]
      if (!(start %in% tmp_tour$nodeID)) tmp_tour_withstart <- rbind(tmp_tour_withstart, tmp[tmp$nodeID==start,])
    }
    
    if (dist_calculation_method=="googleAPI") dmat_tour <- distance_matrix(tmp=tmp_tour_withstart, method = "googleAPI", pdmat=pdmat, symmetric = F, weight_priority=0)
    if (dist_calculation_method=="euclidean") dmat_tour <- distance_matrix(tmp=tmp_tour_withstart, method = "euclidean", pdmat=pdmat, symmetric = F, weight_priority=0)
    
    tour <- solve_tsp(dm=dmat_tour, start)
    tour_travel_distance <- tour$tour_length
    tour_travel_time <- tour_travel_distance/avg_speed 
    c(tour_travel_distance, tour_travel_time, paste(tour$path_labels, collapse="_"))
  })
  
  ts[,c("tour_travel_distance", "tour_travel_time", "tour_labels")] <- t(tsp_sol)
  ts[,c("tour_travel_distance", "tour_travel_time")] <- lapply(ts[,c("tour_travel_distance", "tour_travel_time")], as.numeric)
  
  ts$tour_tot_time <- ts$tour_dwell_time + as.numeric(ts$tour_travel_time)
  
  feasible_tours <- (ts[,"tour_tot_time"] <= ifelse(ts[,"tour_n_express"]==0, max_tour_time, max_tour_time_express) & ts[,"tour_travel_distance"] <= max_tour_length)
  if (any(feasible_tours)) ts <- ts[feasible_tours,] else return(NULL)
  
  return(ts)
}





solve_tsp <- function(dm, start) {
  if (!isSymmetric(dm)) mat <- ATSP(dm) else mat <- TSP(dm)
  tour <- solve_TSP(mat, method="nearest_insertion")
    #note: we can improve this solution further, see pg. 13 of hahsler2007
  if (length(labels(tour))==1) tour_labels <- labels(tour) else tour_labels <- labels(cut_tour(tour, start))
  return(list("path_labels"=c(start, tour_labels, start), 
              "tour_length"=tour_length(tour)))
  }
  
  



distance_matrix <- function(tmp, method, pdmat=NULL, symmetric=NULL, fun=NULL, weight_priority=0) {
  #takes as main inputs the dataframe of delivery locations (tmp) and the distance matrix of postal codes (pdmat)
  #returns a matrix of distances between each pair of nodes
  #two methods are available:
  #   "euclidean" uses lon and lat columns in tmp and computes euclidean distances, by default is a symmetric matrix
  #   "googleAPI" maps each node to its postal code and uses the pdmat matrix to retrieve the pairwise distances
  #       - if symmetric is T then before mapping nodes to postcodes, the pdmat is first converted into a symmetrix matrix by either taking pairwise min or mean
  #       - if symmetric is F then pdmat is not left as a asymmetric matrix, and the resulting distance matrix is also asymmetric
  
  if (!(method %in% c("euclidean", "googleAPI"))) stop("method must be 'euclidean' or 'googleAPI' ")
  if (weight_priority<0 | weight_priority>1) stop("weight should be in [0,1]")
  
  if (method=="euclidean") dmat <- euclidean_dmat(tmp)
  
  if (method=="googleAPI") {
    pdmat_tmp <- pdmat[unique(tmp$postcode), unique(tmp$postcode), drop=FALSE] # subset pdmat by keeping only postcodes present in tmp, drop=F force pdmat to mantain the matrix format, even if after subsetting it it is transformed into a singleton (in the case all nodes ahve same postcode)
    if (symmetric) pdmat_tmp <- symmetric_dmat(dm=pdmat_tmp, fun=fun)
    postcode_freq <- as.data.frame(table(tmp$postcode), stringsAsFactors = F) #frequencies of postcodes associated with the nodes in tmp
    names(postcode_freq) <- c("postcode", "freq")
    dmat <-pdmat_tmp[rep(postcode_freq$postcode, postcode_freq$freq), rep(postcode_freq$postcode, postcode_freq$freq), drop=FALSE] 
    rownames(dmat) <- colnames(dmat) <- tmp$nodeID
    }
  
  if (weight_priority>0) {
    #weighted (w) average between dmat and priority type matrix
    scaled_dmat <- dmat/mean(dmat) #see pg.505 of book Hastie, Tibshirani and Friedman (2009)
    prdmat <- as.matrix(daisy(data.frame(tmp$priority)))
    if (all(prdmat==0)) scaled_prdmat <- prdmat else {#if all deliveries are of low or high priority, then the prdmat is a matrix of zeros, hence we cannot standardize it
      scaled_prdmat <- prdmat/mean(prdmat)
    } 
    dmat <- (1-weight_priority)*scaled_dmat + (weight_priority)*scaled_prdmat
  }
  
  return(dmat)
  }




euclidean_dmat <- function(dat) {
  #dat contains two columns called "lon" and "lat", with the longitude and latitude of each point
  #distm() turn data.frame of points coordinates into a symmetric distance matrix
  
  dmat <- distm(dat[,c("lon", "lat")])
  colnames(dmat) <- rownames(dmat) <- dat$nodeID
  return(dmat)
  }



symmetric_dmat <- function(dm, fun) {
  #make asymmetric matrix symmetric by:
  #   fun="min" taking the minimum of each pair of distances
  #   fun="mean" taking the mean of each pair of distances
  
  nn <- t(dm)
  lower <- dm[lower.tri(dm)]
  upper <- nn[lower.tri(nn)]
  if (fun=="min") vec <- pmin(lower, upper)
  if (fun=="mean") vec <- (lower + upper)/2
  symm <- vec2symMat(vec, diag = F, byrow = F)
  diag(symm) <- 0
  colnames(symm) <- rownames(symm) <- colnames(dm)
  return(symm)
  }




map_hub_to_node <- function(tmp, hub_centers) {
  #return the postcode of the closest node to each cluster center 
  sol <- lapply(1:max(tmp$hubID), function (x) {
    which_min_node <- which.min(distm(hub_centers[hub_centers$hubID==hub_centers$hubID[x],c("lon","lat")], tmp[tmp$hubID==hub_centers$hubID[x],c("lon","lat")]))
    tmp[tmp$hubID==hub_centers$hubID[x],c("postcode", "nodeID", "hubID", "lon", "lat")][which_min_node,]
    })
  sol <- do.call(rbind, sol)
  names(sol)[which(names(sol)=="postcode")] <- "postcode"
  return(sol)
  }



solve_knapsack <- function(vehicles, p, w) {
  k <- vehicles$cap
  if (length(w)==1) { # if there is only one tour, then we assign it to the vehicle with the lowest capacity
    tmp_vehicles <- vehicles[vehicles$cap>=w,]
    chosen_vehicles <- tmp_vehicles[which.min(tmp_vehicles$cap),"vehicleID"]
    vehicles[vehicles$vehicleID==chosen_vehicles,"cap"] <- vehicles[vehicles$vehicleID==chosen_vehicles,"cap"]-w
  } else { 
    if (any(k>=sum(w))) {# if all tours can be performed by one single vehicle then these are assigned to that vehicle. If more than one vehicle is suitable, then all tours are assigned to the vehicle which (1) can contain them all and (2) has minimum capacity
      tmp_vehicles <- vehicles[vehicles$cap>=sum(w),]
      chosen_vehicles <- tmp_vehicles[which.min(tmp_vehicles$cap),"vehicleID"]
      vehicles[vehicles$vehicleID==chosen_vehicles,"cap"] <- vehicles[vehicles$vehicleID==chosen_vehicles,"cap"]-sum(w)
    } else {
      tmp_vehicles <- vehicles[vehicles$cap>min(w),] # keep only the vehicles which can be used
      sol <- mknapsack(p, w, k=tmp_vehicles$cap)
      chosen_vehicles <- tmp_vehicles[sol$ksack,"vehicleID"]
      tmp <- data.frame("tour_tot_time"=w, "vehicleID"=chosen_vehicles)
      #tmp_tours_express$vehicleID <- chosen_vehicles
      agg <- aggregate(tour_tot_time ~ vehicleID,  data=tmp, FUN=sum)
      vehicles[1:nrow(agg),"cap"] <- vehicles[1:nrow(agg),"cap"]-agg$tour_tot_time
    }
  }
  vehicles$cap <- round(vehicles$cap) #make sure capacities are integer
  vehicles <- vehicles[order(vehicles$cap),]#sort vehicle table by cap
  
  return(list("chosen_vehicles"=chosen_vehicles, "vehicles"=vehicles))
} 






vehicle_assignment <- function(tours, start_time, express_time, max_tour_time) {
  tours$tourID <- as.character(tours$tourID)
  tours$vehicleID <- NA
  
  for (h in 1:max(tours$hubID)) {
    tmp_tours <- tours[tours$hubID==h,]
    vehicles <- data.frame("vehicleID"=1:nrow(tmp_tours), "cap"=NA)
    vehicles$cap <- as.integer(difftime(express_time, start_time, units = "mins"))
    
    # express deliveries
    if (any(tmp_tours$tour_n_express>0)) {
      tmp_tours_express <- tmp_tours[tmp_tours$tour_n_express>0,]
      p <- tmp_tours_express$tour_n_express
      w <- round(tmp_tours_express$tour_tot_time)
      if (any(w==0)) w[w==0] <- 1 #if one tour time is rounded to 0, then we set it at 1 (w must be a vector of positive integers)
      sol_knapsack <- solve_knapsack(vehicles, p, w)
      vehicles <- sol_knapsack$vehicles
      tours[tours$hubID==h & tours$tour_n_express>0,"vehicleID"] <- sol_knapsack$chosen_vehicles
    }
    
    # non-express deliveries
    if (any(tmp_tours$tour_n_express==0)) {
      tmp_tours_nonexpress <- tmp_tours[tmp_tours$tour_n_express==0,]
      vehicles$cap <- vehicles$cap + max_tour_time - as.numeric(difftime(express_time, start_time, units = "mins")) #add afternoon time
      p <- rep(1, nrow(tmp_tours_nonexpress))
      w <- round(tmp_tours_nonexpress$tour_tot_time)
      if (any(w==0)) w[w==0] <- 1 
      sol_knapsack <- solve_knapsack(vehicles, p, w)
      vehicles <- sol_knapsack$vehicles
      tours[tours$hubID==h & tours$tour_n_express==0,"vehicleID"] <- sol_knapsack$chosen_vehicles
    }
  }
  tours$vehicleID
}





tour_merge <- function(tour_stats, tour_schedule, tmp, max_weight, max_tour_time, max_tour_time_express, max_tour_length, hub, start_tour, depot, dist_calculation_method, pdmat, avg_speed) {
  
  if (nrow(tour_stats)==1) return(NULL)
  combinations <- data.frame(t(combn(tour_stats$tourID, 2))) 
  
  # only combinations which tours belong to same hubID are allowed
  same_hubID <- tour_stats[combinations[,1],"hubID"] == tour_stats[combinations[,2],"hubID"] 
  if (any(same_hubID)) combinations <- combinations[same_hubID,] else return(NULL)
  
  tourID_list <- as.character(paste(combinations[,1], combinations[,2], sep="_"))
  cts <- find_feasible_tours(tourID_list, tmp, max_weight, max_tour_time, max_tour_time_express, max_tour_length, hub, start_tour, depot, dist_calculation_method, pdmat, avg_speed)
  
  if (is.null(cts)) return(NULL) 
  
  cts$SUM_tour_tot_time <- sapply(1:nrow(cts), function(x) sum(tour_stats[tour_stats$tourID %in% as.numeric(unlist(strsplit(cts[x,"tourID"], "_"))),"tour_tot_time"])) ##SUM of the tour times!! not merged yet
  cts$SUM_tour_travel_distance <- sapply(1:nrow(cts), function(x) sum(as.numeric(tour_stats[tour_stats$tourID %in% as.numeric(unlist(strsplit(cts[x,"tourID"], "_"))),"tour_travel_distance"]))) ##SUM of the tour times!! not merged yet
  
  feasible_tours <- (cts[,"SUM_tour_travel_distance"] >= cts[,"tour_travel_distance"]) & (cts[,"SUM_tour_tot_time"] >= cts[,"tour_tot_time"])
  if (any(feasible_tours)) cts <- cts[feasible_tours,] else return(NULL)
  
  G1 <- igraph::graph(as.numeric(unlist(strsplit(cts$tourID, "_"))), directed = FALSE)
  #weights <- cts$SUM_tour_travel_distance - cts$tour_travel_distance #add weight to the bipartite graph
  #igraph::E(G1)$weight <- weights
  sol <- maxmatching(G1, weighted = FALSE)
  #sol <- maxmatching(G1, weighted =TRUE)
  
  if ("mate" %in% names(sol)) { ####### check from here!
    sol <- sol$matching
    matches <- sapply(1:nrow(t(matrix(sol, nrow = 2))), function(x) paste(t(matrix(sol, nrow = 2))[x,], collapse="_"))
    selected_tours <- cts$tourID %in%  matches
  } else {
    sol <- sol$matching
    selected_tours <- sapply(1:nrow(cts), function(x) {
      k <- sol[as.numeric(unlist(strsplit(cts[x,"tourID"], "_")))[1]]
      if (!is.na(k) & k==as.numeric(unlist(strsplit(cts[x,"tourID"], "_")))[2]) T else F
    })
  }
  
  if (any(selected_tours)) cts <- cts[selected_tours,] else return(NULL)
  
  # update tour_stats
  tour_stats <- rbind(tour_stats[!(tour_stats$tourID %in% as.numeric(unlist(strsplit(cts$tourID, "_")))),], cts[,names(tour_stats)]) 
  tourID_map <- data.frame("oldID"=tour_stats$tourID, "newID"=1:nrow(tour_stats), stringsAsFactors = F)
  tour_stats$tourID <- tourID_map$newID
  
  # update tour_schedule
  new_tour_schedule <- data.frame()
  for (x in 1:nrow(cts)) new_tour_schedule <- rbind(new_tour_schedule, data.frame("nodeID"=unlist(strsplit(cts[x,"tour_labels"], "_")), "hubID"=cts[x,"hubID"], "tourID"=cts[x,"tourID"], "tourSEQ"=1:length(unlist(strsplit(cts[x,"tour_labels"], "_")))))
  tour_schedule <- rbind(tour_schedule[!(tour_schedule$tourID %in% as.numeric(unlist(strsplit(cts$tourID, "_")))),], new_tour_schedule)
  tour_schedule$tourID <- sapply(1:nrow(tour_schedule), function(x) {
    ind <- grep(paste0("^",as.character(tour_schedule[x,"tourID"]), "$"), tourID_map$oldID)
    if (length(ind)==0) ind <- grep(paste0("^",as.character(tour_schedule[x,"tourID"]), "_"), tourID_map$oldID)
    if (length(ind)==0) ind <- grep(paste0("_",as.character(tour_schedule[x,"tourID"]), "$"), tourID_map$oldID)
    tourID_map[ind,"newID"]
  })
  
  # update tourID
  tmp$tourID <- sapply(1:nrow(tmp), function(x) {
    ind <- grep(paste0("^",as.character(tmp[x,"tourID"]), "$"), tourID_map$oldID)
    if (length(ind)==0) ind <- grep(paste0("^",as.character(tmp[x,"tourID"]), "_"), tourID_map$oldID)
    if (length(ind)==0) ind <- grep(paste0("_",as.character(tmp[x,"tourID"]), "$"), tourID_map$oldID)
    tourID_map[ind,"newID"]
  })
  
  return(list("assignment"=tmp$tourID, "stats"=tour_stats, "schedule"=tour_schedule))
}









day_scheduling <- function(tmp, pdmat, depot, max_weight, max_tour_length, max_tour_time, express_time, start_time, avg_speed, hub, max_nboxes_hub, n_hubs, weight_priority, dist_calculation_method=c("googleAPI", "euclidean")) {
  ###schedule delivery tours for bikes and assign them to hubs 
  ###-------------------------------------

  RESULTS <- list()
  
  format_datetime <- paste(datetime_format(unique(tmp$date)), "%H:%M:%S")
  start_time <- as.POSIXct(paste(unique(tmp$date), start_time), format = format_datetime) #time of start tour
  express_time <- as.POSIXct(paste(unique(tmp$date), express_time), format = format_datetime) #latest time for express deliveries
  max_tour_time_express <- difftime(express_time, start_time, units = "mins")
  
  # distance matrix
  if (dist_calculation_method=="googleAPI") dmat <- distance_matrix(tmp=tmp, pdmat=pdmat, method="googleAPI", symmetric=T, fun="min", weight_priority=weight_priority)
  if (dist_calculation_method=="euclidean") dmat <- distance_matrix(tmp=tmp, pdmat=pdmat, method="euclidean", weight_priority=weight_priority)

  # hierarchical clustering
  hc <- hclust(as.dist(dmat), method="complete")
  
  # hub assignment
  if (hub=="Y") {
    tmp$hubID <- tmp_cut <- cutree(hc, k=n_hubs) #cutree(hc, h=hub_cut)
    hub_centers <- merge(aggregate(lat ~ hubID, data = tmp, FUN = mean), aggregate(lon ~ hubID, data = tmp, FUN = mean))
    hubs <- map_hub_to_node(tmp, hub_centers)
    start_tour <- hubs
  } else {
    tmp$hubID <- 1
    tmp_cut <- rep(1, nrow(tmp))
    start_tour <- depot
  }
  
  # tour assignment
  tmp$tourID <- NA
  tour_schedule <- data.frame()
  tour_stats <- data.frame()
  while (any(is.na(tmp$tourID))) {
    tour <- tour_assignment(tmp_cut, tmp, pdmat, max_weight, max_tour_length, max_tour_time, max_tour_time_express, start_tour, avg_speed, hub, dist_calculation_method, tour_stats, tour_schedule)
    tmp$tourID <- tour$assignment
    tour_schedule <- tour$schedule
    tour_stats <- tour$stats
    #print(max(tmp_cut))
    if (max(tmp_cut)<nrow(tmp)) tmp_cut <- cutree(hc, k=max(tmp_cut)+1) else break
  }
  
  # tour merging
  if (nrow(tour_stats)>1) {
    merged <- tour_merge(tour_stats, tour_schedule, tmp, max_weight, max_tour_time, max_tour_time_express, max_tour_length, hub, start_tour, depot, dist_calculation_method, pdmat, avg_speed)
    while (!is.null(merged)) {
      tour_stats <- merged$stats
      tour_schedule <- merged$schedule
      tmp$tourID <- merged$assignment
      merged <- tour_merge(tour_stats, tour_schedule, tmp, max_weight, max_tour_time, max_tour_time_express, max_tour_length, hub, start_tour, depot, dist_calculation_method, pdmat, avg_speed)
    }
  }
  
  # add lat lon to tour_schedule
  tour_schedule <- merge(tour_schedule, rbind(tmp[,c("nodeID", "lat", "lon", "postcode")], depot), by="nodeID") #CHECK how to include lat/lon of depot in truck solution!
  tour_schedule <- tour_schedule[order(tour_schedule$hubID, tour_schedule$tourID, tour_schedule$tourSEQ),]
  tour_schedule$date <- tour_stats$date <- unique(tmp$date)
  
  # vehicle assignment
  tour_stats <- tour_stats[order(tour_stats$hubID, -tour_stats$tour_n_express),]
  if (hub=="N") tour_stats$hubID <- 1
  tour_stats$vehicleID <- vehicle_assignment(tours=tour_stats, start_time, express_time, max_tour_time)
  #vehicle_stats report
  vehicle_stats <- merge(
    aggregate(cbind("tot_weight"=tour_tot_weight, "tot_time"=tour_tot_time, "tot_ndel"=tour_ndeliveries)~vehicleID+hubID, data=tour_stats, FUN=sum),
    aggregate(cbind("avg_weight"=tour_tot_weight, "avg_time"=tour_tot_time, "avg_ndel"=tour_ndeliveries, "avg_loading"=tour_tot_weight/max_weight)~vehicleID+hubID, data=tour_stats, FUN=mean)
    )
  vehicle_stats$weight_capacity <- max_weight
  vehicle_stats$date <- unique(tmp$date)
  
  # hub trips
  if (hub=="Y") {
    hubs$date <- unique(tmp$date)
    
    # compute hub refill trips and respective travel distance and time from depot
    hubs$trips <- aggregate(tourID ~ hubID, data=tmp, FUN= function(x) {return(ceiling(length(unique(x))/max_nboxes_hub))})[,2]
    hubs$depot_dist <- sapply(hubs$postcode, FUN= function(x){pdmat[depot$postcode , x] + pdmat[x , depot$postcode]}) #meters
    hubs$tot_travel_dist <- hubs$depot_dist*hubs$trips #meters
    hubs$tot_travel_time <- hubs$tot_travel_dist/750 #minutes (750 is the avg truck speed in meters/min --CHECK!)
    hubs <- merge(hubs, aggregate(cbind("nvehicles"=vehicleID)~hubID, data=vehicle_stats, FUN=max))
    RESULTS[["day_hub"]] <- hubs
  }
  
  tour_stats <- tour_stats[order(tour_stats$tourID),]
  RESULTS[["day_tour_schedule"]] <- tour_schedule
  RESULTS[["day_tour_stats"]] <- tour_stats
  RESULTS[["day_vehicle_stats"]] <- vehicle_stats
  
  return(RESULTS)
}




