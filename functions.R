#### inwaterdistance ####
inwaterdistance <- function(origin, destination, transition){
  shortestPath(transition, st_coordinates(suppressWarnings(st_centroid(destination))), st_coordinates(origin), output="SpatialLines") %>% 
    st_as_sf() %>% 
    st_length() %>% 
    as.numeric()
}


#### nearestsites ####
nearestsites <- function(lease,prov,sites,n,distmat){
  distances <- data.table(StnLocation=colnames(distmat),
                          distance=distmat[row.names(distmat)==lease$Lease_Identifier,]) 
    
  sites %>%
    left_join(distances, by = "StnLocation") %>% 
    # mutate(distance = distmat[lease$Lease_Identifier==row.names(distmat),]) %>% 
    top_n(-n,distance) %>% 
    arrange(distance) %>% 
    mutate(StnLocation = paste0(StnLocation," (",round(distance/1000)," km)"))
}

#### basemap ####
basemap <- function(leases, crabs, tunicates, tunicatesp){
  greenCrabIcon <- makeIcon(
    iconUrl = "GreenCrab.png",
    iconWidth = 50,
    iconHeight = 37
  )
  
  # this doesn't work, needs a web address
  html_legend <- paste0("<img src='",getwd(),"/",greenCrabIcon$iconUrl,"'>  Green Crab")
  
  leaflet(leases) %>%
    addTiles() %>%
    addPolygons() %>%
    addMarkers(data=crabs$Shape, icon = greenCrabIcon, group = "Green Crab") %>%
    addMinicharts(st_coordinates(tunicates$Shape)[,1],st_coordinates(tunicates$Shape)[,2],
                  type="pie",
                  chartdata=as.data.frame(tunicates)[,tunicatesp]) %>%
    addControl(html = html_legend, position = "bottomright") %>% 
    addLayersControl(overlayGroups = c("Green Crab"),
                     options = layersControlOptions(collapsed = FALSE))
}

