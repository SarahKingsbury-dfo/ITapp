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
  
  # print("calculating nearest sites")
  distances <- data.table(StnLocation=colnames(distmat),
                          distance=distmat[row.names(distmat)==as.character(lease$Lease_Identifier),]) 
  
  # if(ncol(distances)!=2){
  #   browser()
  # }
  
  if(!"Species" %in% names(sites)) {
    # sites for monitoring
    sites %>%
      left_join(distances, by = "StnLocation") %>% 
      top_n(-n,distance) %>% 
      arrange(distance) %>% 
      mutate(StnLocation = paste0(StnLocation," (",round(distance/1000)," km)"))
  }   else {
    # sites for incidentals
    sites %>%
      left_join(distances, by = "StnLocation") %>% 
      top_n(-n,distance) %>% 
      arrange(distance) %>% 
      mutate(StnLocation = paste0(Species, " ", StnLocation," (",round(distance/1000)," km)"))
  }
  
}

#### basemap ####
basemap <- function(leases, incidentals, monitoring, monitoringsp,...){
  
  # browser()

  IncidentalIcons <- iconList(
    "Argopecten irradians" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Ascidiella aspersa" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Botrylloides violaceus" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Botryllus schlosseri" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Caprella mutica" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Carcinus maenas" = makeIcon(
      iconUrl = "GreenCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Ciona intestinalis" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Codium fragile" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Didemnum vexillum" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Diplosoma listerianum" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Hemigrapsus sanguineus" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Membranipora membranacea" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Oncorhynchus mykiss" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Ostrea edulis" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37),
    "Styela clava" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 50,
      iconHeight = 37)
  )
  
  if(!all(incidentals$Species %in% names(IncidentalIcons))){
    warning(paste0("basemap() in functions.R does not have a logo associated with: ",
                   unique(incidentals$Species[!incidentals$Species %in% names(IncidentalIcons)]),
                   collapse="\n"))
  }
  
  # the url's need to be actual url's above for this to work, commenting out for now
  # html_legend <- paste0("<img src='",getwd(),"/",IncidentalIcons$`Carcinus maenas`$iconUrl,"'>  Incidental Observations")
  
  
  sp <- monitoringsp[monitoringsp %in% names(monitoring)]
  
  leaflet(leases,...) %>%
    addTiles() %>%
    addPolygons(popup = paste("Lease:",leases$Lease_Identifier),group = "Leases") %>%
    addMarkers(data=incidentals$geometry,
               icon = IncidentalIcons[as.numeric(factor(incidentals$Species,levels=sort(monitoringsp)))],
               group = incidentals$Species,
               popup = incidentals$link) %>%
    addMinicharts(st_coordinates(monitoring$geometry)[,1],
                  st_coordinates(monitoring$geometry)[,2],
                  type="pie",
                  chartdata=as.data.frame(monitoring)[,sp]) %>%
    # addControl(html = html_legend, position = "bottomright") %>% 
    addLayersControl(overlayGroups = c("Leases",incidentals$Species),
                     options = layersControlOptions(collapsed = FALSE))
}


create_response <- function(summitigation,species){
  if("Site" %in% names(summitigation)){
    summitigation$Site
  } else if("Risk Assessment" %in% names(summitigation)){
    mitigation <- read.csv("mitigation.csv")
    if("High risk" %in% summitigation$`Risk Assessment`){
      paste0("The risk to AIS/FFHPP/SARP is high with medium certainty because there are aquatic invasive species (",
             paste(unique(summitigation$Common_Name[summitigation$`Risk Assessment`=="High risk"]),collapse = ", "),
             ") present at the origin site that are not found at the destination site")
    }else {
      paste0("The risk to AIS/FFHPP/SARP is considered low with high certainty, with mitigation, because all aquatic invasive species (",
             paste(unique(summitigation$Common_Name),collapse = ", "),
             ") present at the origin site are also present at the destination site. To reduce the risk of further spreading aquatic invasive species, the following mitigation treatment(s) are recommended: ",
             tolower(paste(unique(summitigation$Treatment_proposed),collapse = "; as well as, ")))
    }
  } else {
    "Error: Could not generate a response (called from create_response())"
  }
}

