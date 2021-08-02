if(!require("devtools")) install.packages("devtools")
if(!require("esri2sf")) devtools::install_github("yonghah/esri2sf")
if(!require("shiny")) install.packages("shiny")
if(!require("sf")) install.packages("sf")
if(!require("leaflet")) install.packages("leaflet")
if(!require("leaflet.minicharts")) install.packages("leaflet.minicharts")
if(!require("data.table")) install.packages("data.table")
if(!require("tidyverse")) install.packages("tidyverse")

#### global variables ####

proj <- "+proj=longlat +datum=WGS84"


### download provincial lease data or just open the saved version
if(!file.exists("spatialdata/NS.rds")){
  NS <- esri2sf::esri2sf('https://services.arcgis.com/nQHSMRVltyfsxeFe/ArcGIS/rest/services/Marine_Lease_Boundary_Database_Shellfish_View/FeatureServer/0') %>% 
    filter(SiteStatus=="Issued") %>% 
    mutate(Lease_Identifier=License_Lease_Num) %>% 
    st_transform(proj)
  NB <- esri2sf::esri2sf('https://gis-erd-der.gnb.ca/arcgis/rest/services/MASMPS/MASMPS_service/MapServer/0') %>% 
    mutate(Lease_Identifier=MSNO) %>% 
    st_transform(proj)

  raw <- jsonlite::read_json(
    "https://www.arcgis.com/sharing/rest/content/items/16aa8830c7084a8a92ce066b525978b4/data",
    simplifyVector = FALSE
  )
  
  features <- c(raw$operationalLayers[[1]]$featureCollection$layers[[1]]$featureSet$features,
                raw$operationalLayers[[2]]$featureCollection$layers[[1]]$featureSet$features,
                raw$operationalLayers[[3]]$featureCollection$layers[[1]]$featureSet$features)
  
  PEI <- lapply(features, "[[", "attributes") %>% 
    lapply(as_tibble) %>% 
    bind_rows() %>% 
    mutate(geometry = esri2sf:::esri2sfPolygon(features)) %>% 
    st_as_sf() %>% 
    st_set_crs(features[[1]]$geometry$spatialReference$latestWkid)%>% 
    mutate(Lease_Identifier=Lease) %>% 
    st_transform(proj)
  
  saveRDS(NS,"spatialdata/NS.rds")
  saveRDS(NB,"spatialdata/NB.rds")
  saveRDS(PEI,"spatialdata/PEI.rds")
  
  
}
NS <- readRDS("spatialdata/NS.rds")
NB <- readRDS("spatialdata/NB.rds")
PEI <- readRDS("spatialdata/PEI.rds")


AIS <- read.csv("commonnames.csv")

mitigations <- read.csv("mitigation.csv",stringsAsFactors = FALSE)%>% 
  complete(Scientific_Name,Product_treated) %>% 
  left_join(read.csv("commonnames.csv",stringsAsFactors = FALSE),by = "Scientific_Name")

if(!file.exists("outputdata/pei_monitoring_dist.rds")){
  source("make_distance_matrix.R")
}

incidental_sites <- readRDS("outputdata/incidental_sites.rds")
incidental <- readRDS("outputdata/incidental.rds")
monitoring_sites <- readRDS("outputdata/monitoring_sites.rds")
monitoring <- readRDS("outputdata/monitoring.rds")

ns_incidental_dist <- readRDS("outputdata/ns_incidental_dist.rds")
ns_monitoring_dist <- readRDS("outputdata/ns_monitoring_dist.rds")
nb_incidental_dist <- readRDS("outputdata/nb_incidental_dist.rds")
nb_monitoring_dist <- readRDS("outputdata/nb_monitoring_dist.rds")
pei_incidental_dist <- readRDS("outputdata/pei_incidental_dist.rds")
pei_monitoring_dist <- readRDS("outputdata/pei_monitoring_dist.rds")

greenCrabIcon <- makeIcon(
  iconUrl = "GreenCrab.png",
  iconWidth = 50,
  iconHeight = 37
)

html_legend <- paste0("<img src='",getwd(),"/",greenCrabIcon$iconUrl,"'>  Green Crab")

source("functions.R")

#### User Interface ####
ui <- navbarPage(
  title = "Introductions and Transfers Tool",
  
  
  tabPanel("Origin",
           sidebarLayout(
             
             sidebarPanel(
               selectInput(inputId = "origprov",
                           label = "Choose a Province:",
                           choices = c("NS", "NB", "PEI")),
               selectInput(inputId = "origlease",
                           label = "Choose a lease:",
                           choices = NS$Lease_Identifier),
               numericInput(inputId = "origmonitoringnum",
                         label = "Number of Biofouling Monitoring Sites",
                         value = 3),
               numericInput(inputId = "origincidentalnum",
                         label = "Number of Incidental Observation Sites",
                         value = 3),
               checkboxGroupInput(inputId = "origmonitoringsite",
                                  label = "Biofouling Monitoring Sites to Include",
                                  choices = "temp"),
               checkboxGroupInput(inputId = "origincidentalsite",
                                  label = "Incidental Observation Sites to Include",
                                  choices = "temp")
             ),
             
             mainPanel(
               leafletOutput("leafletorig",height=800)
             )
             
           )
  ),
  
  
  tabPanel("Destination",
           sidebarLayout(
             
             sidebarPanel(
               selectInput(inputId = "destprov",
                           label = "Choose a Province:",
                           choices = c("NS", "NB", "PEI")),
               selectInput(inputId = "destlease",
                           label = "Choose a lease:",
                           choices = NS$Lease_Identifier),
               numericInput(inputId = "destmonitoringnum",
                         label = "Number of Biofouling Monitoring Sites",
                         value = 3),
               numericInput(inputId = "destincidentalnum",
                         label = "Number of Incidental Observation Sites",
                         value = 3),
               checkboxGroupInput(inputId = "destmonitoringsite",
                                  label = "Biofouling Monitoring Sites to Include",
                                  choices = "temp"),
               checkboxGroupInput(inputId = "destincidentalsite",
                                  label = "Incidental Observation Sites to Include",
                                  choices = "temp")
             ),
             
             mainPanel(
               leafletOutput("leafletdest",height=800)
             )
             
           )
  ),
  
  
  tabPanel("Summary",
           checkboxGroupInput(
             inputId = "product",
             label = "Applicable Product Description(s):",
             choices = unique(mitigations$Product_treated)
           ),
           tableOutput("species"),
           tableOutput("mitigation")),
  
  
  tabPanel("Interactive Map",
           leafletOutput("leafletmap",height=800)
  ),
  
  
  tabPanel("Settings",
           numericInput(inputId = "monitoringyear",
                        label = "Ignore Biofouling Monitoring Records Older Than:",
                        value = 2013),
           numericInput(inputId = "incidentalyear",
                        label = "Ignore Incidental Observation Records Older Than:",
                        value = 2013) 
  )
  
)




#### Server Logic ####
server <- function(input, output, session) {
  
  # reactive functions that filter data
  monitoring_filtered <- reactive({
    # browser()
    monitoring %>% 
      filter(Year>=input$monitoringyear) %>% 
      as.data.table() %>% 
      dplyr::select(-geometry) %>% 
      gather(key = "Species", value = "Presence",-StnLocation,-Year) %>% 
      group_by(Species,StnLocation) %>% 
      summarize(Presence = if_else(all(is.na(Presence)),
                                   FALSE,
                                   any(Presence>0,na.rm = TRUE))) %>% 
      ungroup() %>% 
      mutate(Species=gsub("_"," ",Species)) %>% 
      spread(key = "Species", value = "Presence") %>% 
      inner_join(monitoring_sites,by = "StnLocation")
  })
  
  incidental_filtered <- reactive({
    incidental %>% 
      filter(Year>=input$incidentalyear) %>% 
      as.data.table() %>% 
      dplyr::select(-geometry) %>% 
      group_by(Species,StnLocation) %>% 
      summarize(Presence = TRUE) %>% 
      ungroup() %>% 
      left_join(incidental_sites,by = "StnLocation")
  })
  
  
  
  
  
  
  # functions for updating UI
  
  update_orig_sites <- function(lease,prov){
    if(input$origmonitoringnum!=""){
      nearestmonitoring <- nearestsites(lease,
                                        prov,
                                        monitoring_filtered(),
                                        as.numeric(input$origmonitoringnum),
                                        monitoring_dist_orig())
      
      updateCheckboxGroupInput(session,
                               "origmonitoringsite",
                               choices = nearestmonitoring$StnLocation)
      
    }
    
    if(input$origincidentalnum!=""){
      nearestincidental <- nearestsites(lease,
                                        prov,
                                        incidental_filtered(),
                                        as.numeric(input$origincidentalnum),
                                        incidental_dist_orig())
      
      updateCheckboxGroupInput(session,
                               "origincidentalsite",
                               choices = nearestincidental$StnLocation)
    }
  }
  
  update_dest_sites <- function(lease,prov){
    if(input$destmonitoringnum!=""){
      nearestmonitoring <- nearestsites(lease,
                                        prov,
                                        monitoring_filtered(),
                                        as.numeric(input$destmonitoringnum),
                                        monitoring_dist_dest())
      
      updateCheckboxGroupInput(session,
                               "destmonitoringsite",
                               choices = nearestmonitoring$StnLocation)
      
    }
    
    if(input$destincidentalnum!=""){
      nearestincidental <- nearestsites(lease,
                                        prov,
                                        incidental_filtered(),
                                        as.numeric(input$destincidentalnum),
                                        incidental_dist_dest())
      
      updateCheckboxGroupInput(session,
                               "destincidentalsite",
                               choices = nearestincidental$StnLocation)
    }
  }
  
  
  
  
  
  
  # Observe events to trigger updates
  
  # Origin - choose a province 
  observeEvent(input$origprov, {
    #get provincial leases
    prov <- origprovInput()
    
    # update lease options
    freezeReactiveValue(input,"map")
    updateSelectInput(session,
                      "origlease",
                      choices = prov$Lease_Identifier,
                      selected = prov$Lease_Identifier[1])
    
    # get correct lease
    lease <- origprovInput() %>%
      filter(Lease_Identifier==prov$Lease_Identifier[1])

    update_orig_sites(lease,prov)
  })
  
  # Origin - Choose a lease
  observeEvent(input$origlease, {
     update_orig_sites(lease=origprovInput() %>%
                        filter(Lease_Identifier==input$origlease),
                      prov=origprovInput())
  })
  
  # Origin - # of monitoring sites
  observeEvent(input$origmonitoringnum, {
    update_orig_sites(lease=origprovInput() %>%
                        filter(Lease_Identifier==input$origlease),
                      prov=origprovInput())
  })
  
  # Origin - # of incidental sites
  observeEvent(input$origincidentalnum, {
    update_orig_sites(lease=origprovInput() %>%
                        filter(Lease_Identifier==input$origlease),
                      prov=origprovInput())
  })
  
  # Destination - choose a province 
  observeEvent(input$destprov, {
    #get provincial leases
    prov <- destprovInput()
    
    # update lease options
    updateSelectInput(session,
                      "destlease",
                      choices = prov$Lease_Identifier,
                      selected = prov$Lease_Identifier[1])
    
    # get correct lease
    lease <- destprovInput() %>%
      filter(Lease_Identifier==prov$Lease_Identifier[1])
    
    update_dest_sites(lease,prov)
  })
  
  # Destination - Choose a lease
  observeEvent(input$destlease, {
    update_dest_sites(lease=destprovInput() %>%
                        filter(Lease_Identifier==input$destlease),
                      prov=destprovInput())
  })
  
  # Destination - # of monitoring sites
  observeEvent(input$destmonitoringnum, {
    update_dest_sites(lease=destprovInput() %>%
                        filter(Lease_Identifier==input$destlease),
                      prov=destprovInput())
  })
  
  # Destination - # of incidental sites
  observeEvent(input$destincidentalnum, {
    update_dest_sites(lease=destprovInput() %>%
                        filter(Lease_Identifier==input$destlease),
                      prov=destprovInput())
  })
  
  
  
    
 
  
  
  

  
  # reactive switches that get the correct leases and distance matrices
  origprovInput <- reactive({
    switch(input$origprov,
           "NS" = NS,
           "NB" = NB,
           "PEI" = PEI)
  })
  
  destprovInput <- reactive({
    switch(input$destprov,
           "NS" = NS,
           "NB" = NB,
           "PEI" = PEI)
  })
  
  monitoring_dist_orig <- reactive({
    switch(input$origprov,
           "NS" = ns_monitoring_dist,
           "NB" = nb_monitoring_dist,
           "PEI" = pei_monitoring_dist)
  })
  
  incidental_dist_orig <- reactive({
    switch(input$origprov,
           "NS" = ns_incidental_dist,
           "NB" = nb_incidental_dist,
           "PEI" = pei_incidental_dist)
  })
  
  monitoring_dist_dest <- reactive({
    switch(input$destprov,
           "NS" = ns_monitoring_dist,
           "NB" = nb_monitoring_dist,
           "PEI" = pei_monitoring_dist)
  })
  
  incidental_dist_dest <- reactive({
    switch(input$destprov,
           "NS" = ns_incidental_dist,
           "NB" = nb_incidental_dist,
           "PEI" = pei_incidental_dist)
  })
  #### Leaflet maps ####
  
  # full interactive map
  output$leafletmap <- renderLeaflet({
    basemap(leases=NS,
            incidentals=incidental_filtered(),
            monitoring=monitoring_filtered(),
            monitoringsp=AIS$Scientific_Name)
  })


  # map for origin tab
  output$leafletorig <- renderLeaflet({
    print("making map")
    
    prov <- origprovInput()
    
    if(!input$origlease %in% prov$Lease_Identifier){
      lease <- prov %>%
        filter(Lease_Identifier==prov$Lease_Identifier[1])
    } else {
      lease <- origprovInput() %>%
        filter(Lease_Identifier==input$origlease)
    }
    

    if(input$origmonitoringnum!=""){
      nearestmonitoring <- nearestsites(lease,
                                       prov,
                                       monitoring_filtered(),
                                       as.numeric(input$origmonitoringnum),
                                       monitoring_dist_orig())
    }

    if(input$origincidentalnum!=""){
      nearestincidental <- nearestsites(lease,
                                       prov,
                                       incidental_filtered(),
                                       as.numeric(input$origincidentalnum),
                                       incidental_dist_orig())
    }

    if(input$origincidentalnum!="" & input$origmonitoringnum!=""){
      basemap(leases=lease,
              incidentals=nearestincidental,
              monitoring=nearestmonitoring,
              monitoringsp=AIS$Scientific_Name)
    }
  })

  # map for destination tab
  output$leafletdest <- renderLeaflet({

    prov <- destprovInput()
    
    if(!input$destlease %in% prov$Lease_Identifier){
      lease <- prov %>%
        filter(Lease_Identifier==prov$Lease_Identifier[1])
    } else {
      lease <- destprovInput() %>%
        filter(Lease_Identifier==input$destlease)
    }
    
    
    if(input$destmonitoringnum!=""){
      nearestmonitoring <- nearestsites(lease,
                                        prov,
                                        monitoring_filtered(),
                                        as.numeric(input$destmonitoringnum),
                                        monitoring_dist_dest())
    }
    
    if(input$destincidentalnum!=""){
      nearestincidental <- nearestsites(lease,
                                        prov,
                                        incidental_filtered(),
                                        as.numeric(input$destincidentalnum),
                                        incidental_dist_dest())
    }
    
    if(input$destincidentalnum!="" & input$destmonitoringnum!=""){
      basemap(leases=lease,
              incidentals=nearestincidental,
              monitoring=nearestmonitoring,
              monitoringsp=AIS$Scientific_Name)
    }
  })
  
  
  #### summary ####
  
  summaryValues <- reactive({
    print("Generating Summary")
    # browser()
    if(is.null(input$origmonitoringsite)&
       is.null(input$origincidentalsite)&
       is.null(input$destmonitoringsite)&
       is.null(input$destincidentalsite)){
      summary <- data.frame(
        Site = "No Origin or Destination sites selected"
      )
    } else if(is.null(input$origmonitoringsite)&
              is.null(input$origincidentalsite)){
      summary <- data.frame(
        Site = "No Origin  sites selected"
      )
    }else if(is.null(input$destmonitoringsite)&
             is.null(input$destincidentalsite)){
      summary <- data.frame(
        Site = "No Destination sites selected"
      )
    } else {
      if(!is.null(input$origmonitoringsite)){
        origin <- monitoring_filtered() %>% 
          filter(gsub("\\s*\\([^\\)]+\\)","",StnLocation) %in% gsub("\\s*\\([^\\)]+\\)","",input$origmonitoringsite)) %>% 
          data.frame() %>% 
          gather(key="Species",value="Presence",-StnLocation) %>% 
          mutate(Species=gsub("\\."," ",Species)) %>% 
          filter(Species %in% AIS$Scientific_Name) %>% 
          group_by(Species) %>% 
          summarize(Presence = any(as.logical(Presence)))
      }else {
        origin <- data.frame("Species"=character(0),"Presence"=logical(0))
      }
      
      if(!is.null(input$destmonitoringsite)){  
        destination <- monitoring_filtered() %>% 
          filter(gsub("\\s*\\([^\\)]+\\)","",StnLocation) %in% gsub("\\s*\\([^\\)]+\\)","",input$destmonitoringsite)) %>% 
          data.frame() %>% 
          gather(key="Species",value="Presence",-StnLocation) %>% 
          mutate(Species=gsub("\\."," ",Species)) %>% 
          filter(Species %in% AIS$Scientific_Name) %>% 
          group_by(Species) %>% 
          summarize(Presence = any(as.logical(Presence)))
      } else {
        destination <- data.frame("Species"=character(0),"Presence"=logical(0))
      }
      
      if(!is.null(input$origincidentalsite)){
        origin <- origin %>%
          bind_rows(data.frame("Species"="Carcinus_maenas","Presence"=TRUE))
      }

      if(!is.null(input$destincidentalsite)){
        destination <- destination %>%
          bind_rows(data.frame("Species"="Carcinus_maenas","Presence"=TRUE))
      }

      summary <- full_join(origin,destination,by="Species",suffix=c(" (Origin)"," (Destination)")) %>% 
        replace_na(list(Species=NA,"Presence (Origin)"=FALSE,"Presence (Destination)"=FALSE)) %>% 
        mutate(Species = str_replace(Species,"_"," "))
    }
    summary
  })
  
  output$species <- renderTable({
    summaryspecies <- summaryValues() 
    if(ncol(summaryspecies)>1){
      summaryspecies <- summaryspecies %>% 
        mutate(Mitigation = if_else(`Presence (Origin)`,
                                    if_else(`Presence (Destination)`,"Recommended","Required"),
                                    "")
               )
    }
    summaryspecies
  })
  
  output$mitigation <- renderTable({
    summarymitigation <- summaryValues()
    if(ncol(summarymitigation)>1){
      if(length(input$product)>0){
        summarymitigation <- summarymitigation %>%
          mutate(Mitigation = if_else(`Presence (Origin)`,
                                      if_else(`Presence (Destination)`,"Recommended","Required"),
                                      "")) %>%
          filter(Mitigation!="") %>%
          left_join(mitigations,by=c("Species"="Scientific_Name")) %>% 
          dplyr::select(Species, Common_Name, Product_treated, Treatment_proposed) %>% 
          filter(Product_treated %in% input$product)
      } else {
        summarymitigation <- data.frame(
          Mitigation = "At least one product type must be selected"
        )
      }
    }
    
    if(nrow(summarymitigation)==0){
      data.frame(
        Mitigation = "No mitigation measures exist for this combination of species and product"
      )
    } else {
      summarymitigation
    }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

