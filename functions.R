#### inwaterdistance ####
inwaterdistance <- function(origin, destination, transition){
  shortestPath(transition, st_coordinates(suppressWarnings(st_centroid(destination))), st_coordinates(origin), output="SpatialLines") %>% 
    st_as_sf() %>% 
    st_length() %>% 
    as.numeric()
}


#### nearestsites ####
nearestsites <- function(lease,prov,sites,n,distmat){
  # if("MF-0491"==lease$Lease_Identifier){browser()}
  if(length(lease$Lease_Identifier)==0){
    stop("Invalid lease identifier (lease error)")
  }
  if(!lease$Lease_Identifier %in% prov$Lease_Identifier){
    stop("Invalid lease identifier (prov error")
  }
  if(!lease$Lease_Identifier %in% row.names(distmat)){
    browser()
    stop("Invalid lease identifier (distmat error)")
  }
  
  print("calculating nearest sites")
  distances <- data.table(StnLocation=colnames(distmat),
                          distance=distmat[row.names(distmat)==as.character(lease$Lease_Identifier),]) 
  
  # if(ncol(distances)!=2){
  #   browser()
  # }
    
  sites %>%
    left_join(distances, by = "StnLocation") %>% 
    top_n(-n,distance) %>% 
    arrange(distance) %>% 
    mutate(StnLocation = paste0(StnLocation," (",round(distance/1000)," km)"))
}

#### basemap ####
basemap <- function(leases, incidentals, monitoring, monitoringsp){
  # browser()
  greenCrabIcon <- makeIcon(
    iconUrl = "GreenCrab.png",
    iconWidth = 50,
    iconHeight = 37
  )
  
  # this doesn't work, needs a web address
  html_legend <- paste0("<img src='",getwd(),"/",greenCrabIcon$iconUrl,"'>  Green Crab")
  
  
  sp <- monitoringsp[monitoringsp %in% names(monitoring)]
  
  leaflet(leases) %>%
    addTiles() %>%
    addPolygons() %>%
    addMarkers(data=incidentals$geometry, icon = greenCrabIcon, group = "Green Crab") %>%
    addMinicharts(st_coordinates(monitoring$geometry)[,1],st_coordinates(monitoring$geometry)[,2],
                  type="pie",
                  chartdata=as.data.frame(monitoring)[,sp]) %>%
    addControl(html = html_legend, position = "bottomright") %>% 
    addLayersControl(overlayGroups = c("Green Crab"),
                     options = layersControlOptions(collapsed = FALSE))
}

