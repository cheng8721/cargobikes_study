
date_format <- function(date) ifelse(all(grepl("/", date)), "%d/%m/%Y", "%Y-%m-%d")

decimal_to_time <- function(vtime, date) {
  format_date <- paste(date_format(date), "%H:%M:%S")
  as.POSIXct(paste(date, "00:00:00"), format = format_date) + 3600*vtime
}


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
    sim_data <- data[replicate(nrep, sample(1:nrow(data), size=ndel, replace=F)),c("lat", "lon", "consignee_address1", "consignee_address3", "consignee_postcode", "service_level", "weight", "dwell_time", "nodeID")]
    date <- rep(as.Date(as.Date("2010-9-06"):(as.Date("2010-9-06")+nrep-1), origin="1970-01-01"), each=ndel)
    sim_data <- cbind(date, sim_data)
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




tour_assignment <- function(tmp_cut, tmp, pdmat, max_weight, max_tour_length, max_tour_time, start_tour, avg_speed, hub) {
  stats <- data.frame()
  schedule <- data.frame()
  for (k in unique(tmp_cut[which(is.na(tmp$tourID))])) {
    tmp_tour <- tmp[which(tmp_cut==k),]
    if (sum(tmp_tour$weight) <= max_weight) { #tot cluster weight < weight capacity
      
      # select start of tour node (depot or hub) and add it to the candidate set of nodes
      tmp_tour_withstart <- tmp_tour
      if (hub=="N") {
        start <- "depot"
        tmp_tour_withstart[nrow(tmp_tour_withstart)+1,c("lat", "lon", "nodeID", "consignee_postcode")] <- start_tour[,c("lat", "lon", "nodeID", "postcode")]
      } else {
        start <- start_tour[start_tour$hubID==unique(tmp_tour$hubID),"nodeID"]
        if (!(start %in% tmp_tour$nodeID)) tmp_tour_withstart <- rbind(tmp_tour_withstart, tmp[tmp$nodeID==start,])
      }
      
      #compute tour distance matrix
      dmat_tour <- distance_matrix(tmp=tmp_tour_withstart, pdmat, method = "googleAPI", symmetric = F)
        
      #compute tour length and time
      tour <- solve_tsp(dm=dmat_tour, start)
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
                                 "tour_n_express"=sum(tmp_tour$service_level %in% c("Express", "Express Plus")),
                                 "tour_tot_weight"=sum(tmp_tour$weight),
                                 stringsAsFactors = F)
        stats <- rbind(stats, tour_stats)
        tour_schedule <- data.frame("hubID"=ifelse(hub=="N", NA, unique(tmp_tour$hubID)),
                                    "tourID"=tourID,
                                    "nodeID"=tour$path_labels,
                                    "tourSEQ"=1:length(tour$path_labels), 
                                    stringsAsFactors = F)
        schedule <- rbind(schedule, tour_schedule)
      }
    }
    }
  return(list("assignment"=tmp$tourID, 
              "schedule"=schedule,
              "stats"=stats))
  }





solve_tsp <- function(dm, start) {
  if (!isSymmetric(dm)) mat <- ATSP(dm) else mat <- TSP(dm)
  tour <- solve_TSP(mat, method="nearest_insertion")
    #note: we can improve this solution further, see pg. 13 of hahsler2007
  if (length(labels(tour))==1) tour_labels <- labels(tour) else tour_labels <- labels(cut_tour(tour, start))
  return(list("path_labels"=c(start, tour_labels, start), 
              "tour_length"=tour_length(tour)))
  }
  
  



distance_matrix <- function(tmp, method, pdmat=NULL, symmetric=NULL, fun=NULL) {
  #takes as main inputs the dataframe of delivery locations (tmp) and the distance matrix of postal codes (pdmat)
  #returns a matrix of distances between each pair of nodes
  #two methods are available:
  #   "euclidean" uses lon and lat columns in tmp and computes euclidean distances, by default is a symmetric matrix
  #   "googleAPI" maps each node to its postal code and uses the pdmat matrix to retrieve the pairwise distances
  #       - if symmetric is T then before mapping nodes to postcodes, the pdmat is first converted into a symmetrix matrix by either taking pairwise min or mean
  #       - if symmetric is F then pdmat is not left as a asymmetric matrix, and the resulting distance matrix is also asymmetric
  
  if (!(method %in% c("euclidean", "googleAPI"))) stop("method must be 'euclidean' or 'googleAPI' ")
  
  if (method=="euclidean") dmat <- euclidean_dmat(tmp)
  
  if (method=="googleAPI") {
    pdmat_tmp <- pdmat[unique(tmp$consignee_postcode), unique(tmp$consignee_postcode), drop=FALSE] # subset pdmat by keeping only postcodes present in tmp, drop=F force pdmat to mantain the matrix format, even if after subsetting it it is transformed into a singleton (in the case all nodes ahve same postcode)
    if (symmetric) pdmat_tmp <- symmetric_dmat(dm=pdmat_tmp, fun=fun)
    postcode_freq <- as.data.frame(table(tmp$consignee_postcode), stringsAsFactors = F) #frequencies of postcodes associated with the nodes in tmp
    names(postcode_freq) <- c("postcode", "freq")
    dmat <-pdmat_tmp[rep(postcode_freq$postcode, postcode_freq$freq), rep(postcode_freq$postcode, postcode_freq$freq), drop=FALSE] 
    rownames(dmat) <- colnames(dmat) <- tmp$nodeID
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




find_postcode <- function(tmp, hub_centers) {
  #return the postcode of the closest node to each cluster center 
  names(hub_centers)[1] <- "nodeID"
  dm <- distance_matrix(tmp=rbind(tmp[,c("nodeID","lat","lon")], hub_centers), method="euclidean")
  dm <- dm[(nrow(dm)-nrow(hub_centers)+1):nrow(dm),1:(nrow(dm)-nrow(hub_centers)), drop=F]
  closest_nodeID <- sapply(1:nrow(dm), FUN=function(x) { #return the nodeID of the closest nodes which belong to the same cluster
    row <- dm[x,,drop=F]
    row <- row[,tmp[tmp$hubID==x,"nodeID"],drop=F]
    colnames(row)[which.min(row)]
    })
  postcodes <- tmp[tmp$nodeID %in% closest_nodeID,"consignee_postcode"]
  return(data.frame("postcode"=postcodes, "nodeID"=closest_nodeID, stringsAsFactors = F))
  }






day_scheduling <- function(tmp, pdmat, depot, max_weight, max_tour_length, max_tour_time, avg_speed, hub, max_nboxes_hub, hub_cut) {
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
  dmat <- distance_matrix(tmp=tmp, pdmat=pdmat, method="googleAPI", symmetric=T, fun="min") 
    ##CHECK: somehow hclust works also with asymmetric dmat. Why????? read: https://www.datacamp.com/community/tutorials/hierarchical-clustering-R

  # hierarchical clustering
  hc <- hclust(as.dist(dmat), method="complete")
  
  # hub assignment
  if (hub=="Y") {
    tmp$hubID <- tmp_cut <- cutree(hc, h=hub_cut)
    hub_centers <- merge(aggregate(lat ~ hubID, data = tmp, FUN = mean), aggregate(lon ~ hubID, data = tmp, FUN = mean))
    hub_centers[,c("postcode", "nodeID")] <- find_postcode(tmp, hub_centers)[,c("postcode", "nodeID")]
    start_tour <- hub_centers
  } else {
    tmp_cut <- rep(1, nrow(tmp))
    start_tour <- depot
  }
  
  # tour assignment
  tmp$tourID <- NA
  tour_schedule <- data.frame()
  tour_stats <- data.frame()
  while (any(is.na(tmp$tourID))) {
    tour <- tour_assignment(tmp_cut, tmp, pdmat, max_weight, max_tour_length, max_tour_time, start_tour, avg_speed, hub)
    tmp$tourID <- tour$assignment
    tour_schedule <- rbind(tour_schedule, tour$schedule)
    tour_stats <- rbind(tour_stats, tour$stats)
    #print(max(tmp_cut))
    if (max(tmp_cut)<nrow(tmp)) tmp_cut <- cutree(hc, k=max(tmp_cut)+1) else break
  }
  tour_schedule$date <- tour_stats$date <- unique(tmp$date)
  
  # vehicle assignment
  #tmp$vehicleID <- NA
  
  # hub trips
  if (hub=="Y") {
    hub_centers$date <- unique(tmp$date)
    
    # compute hub refill trips and respective travel distance and time from depot
    hub_centers$trips <- aggregate(tourID ~ hubID, data=tmp, FUN= function(x) {return(ceiling(length(unique(x))/max_nboxes_hub))})[,2]
    hub_centers$depot_dist <- sapply(hub_centers$postcode, FUN= function(x){pdmat[depot$postcode , x] + pdmat[x , depot$postcode]}) #meters
    hub_centers$tot_travel_dist <- hub_centers$depot_dist*hub_centers$trips #meters
    hub_centers$tot_travel_time <- hub_centers$tot_travel_dist/750 #minutes (750 is the avg truck speed in meters/min --CHECK!)
    RESULTS[["day_hub"]] <- hub_centers
  }
  
  RESULTS[["day_tour_schedule"]] <- tour_schedule
  RESULTS[["day_tour_stats"]] <- tour_stats
  
  return(RESULTS)
}














#library(ggmap)
#tmp$consignee_postcode <- paste0("0", tmp$consignee_postcode)
#tmp$address <- paste(tmp$consignee_postcode, "Singapore")
#for (i in 1:nrow(tmp)) {
#  geocode(tmp$address[i])
#}
  
#tmp[tmp$nodeID=="6344",]

#pal <- colorFactor(topo.colors(100), domain = factor(tmp$hubID))
#leaflet(data=tmp) %>% #plot all delivery points for a given day
#  addMarkers(lng=hub_centers$lon, lat=hub_centers$lat, popup = "Hub") %>%
#  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE) ) %>%
#  addCircleMarkers(~lon, ~lat, color=pal(tmp$hubID), stroke = FALSE, fillOpacity = 0.8, radius=5, popup = tmp$nodeID) 

#%>%
#  addPolylines(data=results_bikes[results_bikes$tourID==i,], lat = ~lat, lng = ~lon, weight=2, color = "red") 


