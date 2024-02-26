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
  #browser()
  if(length(lease$Lease_Identifier)==0){
    stop("Invalid lease identifier (lease error)")
  }
  if(!lease$Lease_Identifier %in% prov$Lease_Identifier){
    stop("Invalid lease identifier (prov error)")
  }
  if(!lease$Lease_Identifier %in% row.names(distmat)){
    #browser()
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
  } else {
    # sites for incidentals
    sites %>%
      left_join(distances, by = "StnLocation") %>% 
      top_n(-n,distance) %>% 
      arrange(distance) %>% 
      mutate(StnLocation = paste0(Species, " ", StnLocation," (",round(distance/1000)," km)"))
  }
  
}

#### basemap ####
basemap <- function(leases, incidentals, monitoring, monitoringsp, metabarcoding, metabarcodingsp, ...){
  
  #browser()

  IncidentalIcons <- iconList(
    "Argopecten_irradians" = makeIcon(
      iconUrl = "mussel.png",
      iconWidth = 30,
      iconHeight = 15),
    "Ascidiella_aspersa" = makeIcon(
      iconUrl = "tunicate.png",
      iconWidth = 30,
      iconHeight = 15),
    "Botrylloides_violaceus" = makeIcon(
      iconUrl = "tunicate.png",
      iconWidth = 30,
      iconHeight = 15),
    "Botryllus_schlosseri" = makeIcon(
      iconUrl = "tunicate.png",
      iconWidth = 30,
      iconHeight = 15),
    "Caprella_mutica" = makeIcon(
      iconUrl = "skeletonshrimp.png",
      iconWidth = 30,
      iconHeight = 15),
    "Carcinus_maenas" = makeIcon(
      iconUrl = "GreenCrab.png",
      iconWidth = 30,
      iconHeight = 15),
    "Ciona_intestinalis" = makeIcon(
      iconUrl = "tunicate.png",
      iconWidth = 30,
      iconHeight = 15),
    "Codium_fragile" = makeIcon(
      iconUrl = "algae.png",
      iconWidth = 30,
      iconHeight = 15),
    "Didemnum_vexillum" = makeIcon(
      iconUrl = "tunicate.png",
      iconWidth = 30,
      iconHeight = 15),
    "Diplosoma_listerianum" = makeIcon(
      iconUrl = "tunicate.png",
      iconWidth = 30,
      iconHeight = 15),
    "Hemigrapsus_sanguineus" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 30,
      iconHeight = 15),
    "Membranipora_membranacea" = makeIcon(
      iconUrl = "algae.png",
      iconWidth = 30,
      iconHeight = 15),
    "Oncorhynchus_mykiss" = makeIcon(
      iconUrl = "trout.png",
      iconWidth = 30,
      iconHeight = 15),
    "Ostrea_edulis" = makeIcon(
      iconUrl = "mussel.png",
      iconWidth = 30,
      iconHeight = 15),
    "Styela_clava" = makeIcon(
      iconUrl = "blackCrab.png",
      iconWidth = 30,
      iconHeight = 15),
    "Procambarus_clarkii"=makeIcon(
      iconUrl="crayfish.png",
      iconWidth = 30,
      iconHeight = 15),
    "Esox_niger"=makeIcon(
      iconUrl="pike.png",
      iconWidth = 30,
      iconHeight = 15),
    "Micopterus_dolomieu"=makeIcon(
      iconUrl="trout.png",
      iconWidth = 30,
      iconHeight = 15),
    "Cipangopaludina_chinensis"=makeIcon(
      iconUrl="snail.png",
      iconWidth = 30,
      iconHeight = 15),
    "Carassius_auratus"=makeIcon(
      iconUrl="goldfish.png",
      iconWidth = 30,
      iconHeight = 15)
  )
  
  if(!all(incidentals$Species %in% names(IncidentalIcons))){
    warning(paste0("basemap() in functions.R does not have a logo associated with: ",
                   unique(incidentals$Species[!incidentals$Species %in% names(IncidentalIcons)]),
                   collapse="\n"))
  }

  
  
  # if(!all(metabarcoding$Species %in% names(MetabarcodeIcons))){
  #   warning(paste0("basemap() in functions.R does not have a logo associated with: ",
  #                  unique(metabarcoding$Species[!metabarcoding$Species %in% names(MetabarcodeIcons)]),
  #                  collapse="\n"))
  # }

  # the url's need to be actual url's above for this to work, commenting out for now
  # html_legend <- paste0("<img src='",getwd(),"/",IncidentalIcons$`Carcinus maenas`$iconUrl,"'>  Incidental Observations")
  
 #browser()
  sp <- monitoringsp[monitoringsp %in% names(monitoring)]
  sp_m<-metabarcoding%>%
    dplyr::select(-StnLocation, -distance)%>%
    tidyr::pivot_longer(!geometry, names_to="species", values_to="presence")%>%
    dplyr::filter(presence=="TRUE")%>%
    dplyr::select(species)
  
  leaflet(leases,...) %>%
    addTiles() %>%
    addPolygons(popup = paste("Lease:",leases$Lease_Identifier),group = "Leases") %>%
    addMarkers(data=incidentals$geometry,
               icon = IncidentalIcons[as.numeric(factor(incidentals$Species,levels=sort(monitoringsp)))],
               group = incidentals$Species
               #popup = incidentals$link
               ) %>%
    # addMinicharts(st_coordinates(metabarcoding$geometry)[,1],
    #               st_coordinates(metabarcoding$geometry)[,2],
    #               type="pie",
    #               chartdata=as.data.frame(metabarcoding)[,sp_m],
    #               #colorPalette = colors,
    #               legend=TRUE,
    #               legendPosition = 'bottomright',
    #               layerId = metabarcoding,
    #               popupOptions = list(closeButton=FALSE, showTitle=TRUE),
    #               ) %>%
    addCircleMarkers(
      data=metabarcoding$geometry,
      color= "lightyellow",
      weight=1,
      group="metabarcoding",
      label=paste("Presence Present:", "<br>",
                  sp_m)%>%
        lapply(htmltools::HTML),
    )%>%
    addMinicharts(st_coordinates(monitoring$geometry)[,1],
                  st_coordinates(monitoring$geometry)[,2],
                  type="pie",
                  chartdata=as.data.frame(monitoring)[,sp],
                  #layerId= monitoring,
                  #colorPalette = d3.schemeCategory10,
                  legend = TRUE,
                  legendPosition = 'topright') %>%
    addLayersControl(overlayGroups = c("Leases",incidentals$Species, "metabarcoding"),
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

