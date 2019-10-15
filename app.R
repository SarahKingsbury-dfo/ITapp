library(shiny)
library(sf)
library(leaflet)
library(leaflet.minicharts)
library(data.table)
library(tidyverse)

#### global variables ####
gdbpath <- "//dcnsbiona01b/edc_v1_shr6/HMD/HF&LD/GIS_FPP/InteractiveMaps/Working/PMF_Working.gdb"
proj <- "+proj=longlat +datum=WGS84"

NS <- read_sf(gdbpath, layer="NS_Aquaculture_Leases_2018") %>% st_transform(crs=proj)
# PEI <- read_sf(gdbpath, layer="PEI_Aquaculture_Leases_2019") %>% st_transform(crs=proj)
# NB <- read_sf(gdbpath, layer="NB_Aquaculture_Leases_2019") %>% st_transform(crs=proj)

tunicatesp <- c("Ciona_intestinalis","Botryllus_schlosseri","Botrylloides_violaceus","Styela_clava",
                "Diplosoma_listerianum","Ascidiella_aspersa","Membranipora_membranacea","Caprella_mutica",
                "Didemnum_vexillum")

mitigations <- read.csv("mitigation.csv",stringsAsFactors = FALSE)%>% 
  complete(Scientific_Name,Product_treated) %>% 
  left_join(read.csv("commonnames.csv",stringsAsFactors = FALSE),by = "Scientific_Name")

greencrab_sites <- readRDS("greencrab_sites.rds")
greencrab <- readRDS("greencrab.rds")
tunicates_sites <- readRDS("tunicates_sites.rds")
tunicates <- readRDS("tunicates.rds")
nsgcdist <- readRDS("nsgcdist.rds")
nstudist <- readRDS("nstudist.rds")

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
               numericInput(inputId = "origtunicatenum",
                         label = "Number of Tunicate Sites",
                         value = 3),
               numericInput(inputId = "origcrabnum",
                         label = "Number of Green Crab Sites",
                         value = 3),
               checkboxGroupInput(inputId = "origtunicatesite",
                                  label = "Tunicate Monitoring Sites to Include",
                                  choices = "temp"),
               checkboxGroupInput(inputId = "origcrabsite",
                                  label = "Green Crab Sites to Include",
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
               numericInput(inputId = "desttunicatenum",
                         label = "Number of Tunicate Sites",
                         value = 3),
               numericInput(inputId = "destcrabnum",
                         label = "Number of Green Crab Sites",
                         value = 3),
               checkboxGroupInput(inputId = "desttunicatesite",
                                  label = "Tunicate Monitoring Sites to Include",
                                  choices = "temp"),
               checkboxGroupInput(inputId = "destcrabsite",
                                  label = "Green Crab Sites to Include",
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
           numericInput(inputId = "tunicateyear",
                        label = "Ignore Tunicate Records Older Than:",
                        value = 2016),
           numericInput(inputId = "greencrabyear",
                        label = "Ignore Green Crab Older Than:",
                        value = 2013) 
  )
  
)

#### Server Logic ####
server <- function(input, output, session) {
  tunicate_filtered <- reactive({
    tunicates %>% 
      filter(Year>=input$tunicateyear) %>% 
      as.data.table() %>% 
      select(-Shape) %>% 
      gather(key = "Species", value = "Presence",-StnLocation,-Year) %>% 
      group_by(Species,StnLocation) %>% 
      summarize(Presence = if_else(all(is.na(Presence)),
                                   FALSE,
                                   any(Presence>0,na.rm = TRUE))) %>% 
      ungroup() %>% 
      spread(key = "Species", value = "Presence") %>% 
      inner_join(tunicates_sites,by = "StnLocation")
  })
  
  greencrab_filtered <- reactive({
    greencrab %>% 
      filter(Year>=input$greencrabyear) %>% 
      as.data.table() %>% 
      select(-Shape) %>% 
      group_by(Species,StnLocation) %>% 
      summarize(Presence = TRUE) %>% 
      ungroup() %>% 
      left_join(greencrab_sites,by = "StnLocation")
  })
  
  observe({
    x <- input$destprov
    dest <- destprovInput()
    
    updateSelectInput(session,
                      "destlease",
                      choices = dest$Lease_Identifier)
    
  })
  
  observe({
    x <- input$origlease
    prov <- origprovInput()
    lease <- origprovInput() %>% 
      filter(Lease_Identifier==input$origlease)
    
    if(input$origtunicatenum!=""){
      nearesttunicates <- nearestsites(lease,
                                       prov,
                                       tunicate_filtered(),
                                       as.numeric(input$origtunicatenum),
                                       nstudist)

      updateCheckboxGroupInput(session,
                               "origtunicatesite",
                               choices = nearesttunicates$StnLocation)
    }
    
    if(input$origcrabnum!=""){
      nearestgreencrab <- nearestsites(lease,
                                       prov,
                                       greencrab_filtered(),
                                       as.numeric(input$origcrabnum),
                                       nsgcdist)

      updateCheckboxGroupInput(session,
                               "origcrabsite",
                               choices = nearestgreencrab$StnLocation)
    }
  })
  
  observe({
    x <- input$destlease
    prov <- destprovInput()
    lease <- destprovInput() %>% 
      filter(Lease_Identifier==input$destlease)
    
    if(input$desttunicatenum!=""){
      nearesttunicates <- nearestsites(lease,
                                       prov,
                                       tunicate_filtered(),
                                       as.numeric(input$desttunicatenum),
                                       nstudist)
      
      updateCheckboxGroupInput(session,
                               "desttunicatesite",
                               choices = nearesttunicates$StnLocation)
    }
    
    if(input$destcrabnum!=""){
      nearestgreencrab <- nearestsites(lease,
                                       prov,
                                       greencrab_filtered(),
                                       as.numeric(input$destcrabnum),
                                       nsgcdist)
      
      updateCheckboxGroupInput(session,
                               "destcrabsite",
                               choices = nearestgreencrab$StnLocation)
    }
  })
  
  
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
  
  
  output$leafletmap <- renderLeaflet({
    basemap(leases=NS,
            crabs=greencrab_filtered(),
            tunicates=tunicate_filtered(),
            tunicatesp=tunicatesp)
  })
   
  
  output$leafletorig <- renderLeaflet({
    lease <- origprovInput() %>% 
      filter(Lease_Identifier==input$origlease)
    
    prov <- origprovInput()
    
    
    if(input$origtunicatenum!=""){
      nearesttunicates <- nearestsites(lease,
                                       prov,
                                       tunicate_filtered(),
                                       as.numeric(input$origtunicatenum),
                                       nstudist)
    }
    
    if(input$origcrabnum!=""){
      nearestgreencrab <- nearestsites(lease,
                                       prov,
                                       greencrab_filtered(),
                                       as.numeric(input$origcrabnum),
                                       nsgcdist)
    }
    
    if(input$origcrabnum!="" & input$origtunicatenum!=""){
      basemap(leases=lease,
              crabs=nearestgreencrab,
              tunicates=nearesttunicates,
              tunicatesp=tunicatesp)
    }
  })
  
  output$leafletdest <- renderLeaflet({
    lease <- destprovInput() %>% 
      filter(Lease_Identifier==input$destlease)
    
    prov <- destprovInput()
    
    
    if(input$desttunicatenum!=""){
      nearesttunicates <- nearestsites(lease,
                                       prov,
                                       tunicate_filtered(),
                                       as.numeric(input$desttunicatenum),
                                       nstudist)
    }
    
    if(input$destcrabnum!=""){
      nearestgreencrab <- nearestsites(lease,
                                       prov,
                                       greencrab_filtered(),
                                       as.numeric(input$destcrabnum),
                                       nsgcdist)
    }
    
    if(input$destcrabnum!="" & input$desttunicatenum!=""){
      basemap(leases=lease,
              crabs=nearestgreencrab,
              tunicates=nearesttunicates,
              tunicatesp=tunicatesp)
    }
  })
  
  summaryValues <- reactive({
    print("Generating Summary")
    if(is.null(input$origtunicatesite)&
       is.null(input$origcrabsite)&
       is.null(input$desttunicatesite)&
       is.null(input$destcrabsite)){
      summary <- data.frame(
        Site = "No Origin or Destination sites selected"
      )
    } else if(is.null(input$origtunicatesite)&
              is.null(input$origcrabsite)){
      summary <- data.frame(
        Site = "No Origin  sites selected"
      )
    }else if(is.null(input$desttunicatesite)&
             is.null(input$destcrabsite)){
      summary <- data.frame(
        Site = "No Destination sites selected"
      )
    } else {
      if(!is.null(input$origtunicatesite)){
        origin <- tunicate_filtered() %>% 
          filter(StnLocation %in% gsub("\\s*\\([^\\)]+\\)","",input$origtunicatesite)) %>% 
          data.frame() %>% 
          gather(key="Species",value="Presence",-StnLocation) %>% 
          filter(Species %in% tunicatesp) %>% 
          group_by(Species) %>% 
          summarize(Presence = any(as.logical(Presence)))
      }else {
        origin <- data.frame("Species"=character(0),"Presence"=logical(0))
      }
      
      if(!is.null(input$desttunicatesite)){  
        destination <- tunicate_filtered() %>% 
          filter(StnLocation %in% gsub("\\s*\\([^\\)]+\\)","",input$desttunicatesite)) %>% 
          data.frame() %>% 
          gather(key="Species",value="Presence",-StnLocation) %>% 
          filter(Species %in% tunicatesp) %>% 
          group_by(Species) %>% 
          summarize(Presence = any(as.logical(Presence)))
      } else {
        destination <- data.frame("Species"=character(0),"Presence"=logical(0))
      }
      
      if(!is.null(input$origcrabsite)){
        origin <- origin %>%
          bind_rows(data.frame("Species"="Carcinus_maenas","Presence"=TRUE))
      }

      if(!is.null(input$destcrabsite)){
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
          select(Species, Common_Name, Product_treated, Treatment_proposed) %>% 
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

