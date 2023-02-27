if(!require("spocc")) install.packages("spocc")
if(!require("robis")) install.packages("robis")
if(!require("devtools")) install.packages("devtools")
if(!require("esri2sf")) devtools::install_github("yonghah/esri2sf")
if(!require("sf")) install.packages("sf")
if(!require("raster")) install.packages("raster")
if(!require("fasterize")) install.packages("fasterize")
if(!require("gdistance")) install.packages("gdistance")
if(!require("data.table")) install.packages("data.table")
if(!require("tidyverse")) install.packages("tidyverse")

print("Warning: Initial installation will take multiple hours!")

#### global variables ####

print("Loading data")
proj <- "+proj=longlat +datum=WGS84"
equidist <- "+proj=eqdc +lon_0=-63.59 +lat_1=43.92 +lat_2=48.33 +lat_0=46.13 +x_0=1000000 +y_0=1000000 +datum=WGS84 +units=m +no_defs"



NS <- readRDS("spatialdata/NS.rds")
NB <- readRDS("spatialdata/NB.rds")
PEI <- readRDS("spatialdata/PEI.rds")

species <- read.csv("commonnames.csv")



# Load and clean up incidental data ---------------------------------------

searcharea <- c(NS$geometry,NB$geometry,PEI$geometry) %>% 
  st_combine() %>% 
  st_convex_hull() %>% 
  st_transform(equidist) %>% 
  st_buffer(100000) %>% 
  st_transform(proj)


incidental_occ <- occ(query=species$Scientific_Name,
                      from=c("gbif","inat"),
                      geometry = st_bbox(searcharea),
                      has_coords = TRUE,
                      limit=10000) %>%
  occ2df() %>%
  mutate(StnLocation=paste0("within 1.1km of lon ",round(as.numeric(longitude),2)," lat ",round(as.numeric(latitude),2))) %>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326)%>%
  st_transform(crs=proj) %>%
  mutate(link=case_when(prov=="gbif" ~ paste0("https://www.gbif.org/occurrence/",key),
                        prov=="inat" ~ paste0("https://www.inaturalist.org/observations/",key)
  )) %>%
  mutate(Species=case_when(name %in% c("Aequipecten irradians (Lamarck, 1819)","Argopecten irradians (Lamarck, 1819)", "Argopecten irradians") ~ "Argopecten_irradians",
                           name %in% c("Ascidiella aspersa (MÃ¼ller, 1776)") ~ "Ascidiella_aspersa",
                           name %in% c("BOLD:AAA7687","BOLD:ACL8382","Carcinus maenas (Linnaeus, 1758)", "Carcinus maenas") ~ "Carcinus_maenas",
                           name %in% c("Botrylloides violaceus Oka, 1927", "Botrylloides violaceus") ~ "Botrylloides_violaceus",
                           name %in% c("Botryllus schlosseri (Pallas, 1766)", "Botryllus schlosseri") ~ "Botryllus_schlosseri",
                           name %in% c("Caprella mutica Schurin, 1935","BOLD:AAE7686", "Caprella mutica") ~ "Caprella_mutica",
                           name %in% c("Ciona intestinalis (Linnaeus, 1767)","Ciona intestinalis tenella (Stimpson, 1852)","Ciona tenella (Stimpson, 1852)", "Ciona intestinalis" ) ~ "Ciona_intestinalis",
                           name %in% c("Codium fragile (Suringar) Hariot","Codium fragile subsp. fragile","Codium fragile subsp. tomentosoides (Goor) P.C.Silva","Codium fragile tomentosoides", "Codium fragile" ) ~ "Codium_fragile",
                           name %in% c("Didemnum vexillum Kott, 2002", "Didemnum vexillum") ~ "Didemnum_vexillum",
                           name %in% c("Hemigrapsus sanguineus (De Haan, 1835)","Hemigrapsus sanguineus (de Haan, 1835)", "Hemigrapsus sanguineus") ~ "Hemigrapsus_sanguineus",
                           name %in% c("Membranipora membranacea (Linnaeus, 1767)") ~ "Membranipora_membranacea",
                           name %in% c("Oncorhynchus mykiss (Walbaum, 1792)", "Oncorhynchus mykiss") ~ "Oncorhynchus_mykiss",
                           name %in% c("Ostrea edulis (Linnaeus, 1767)","Ostrea edulis Linnaeus, 1758","Ostrea edulis") ~ "Ostrea_edulis",
                           name %in% c("Styela clava Herdman, 1881", "Styela clava") ~ "Styela_clava",
                           name %in% c("Diplosoma listerianum (Milne Edwards, 1841)", "Diplosoma listerianum") ~ "Diplosoma_listerianum",
                           name %in% c("Ascidiella aspersa (Müller, 1776)","Ascidiella aspersa" ) ~ "Ascidiella_aspersa",
                           name %in% c( "Flustra membranacea Linnaeus, 1767", "Membranipora membranacea") ~ "Membranipora_membranacea",
                           TRUE ~ name),
         Year=as.numeric(substr(date,1,4)))

if(!all(sort(unique(incidental_occ$Species)) %in% sort(species$Scientific_Name))){
  sp <- sort(unique(incidental_occ$Species))[!sort(unique(incidental_occ$Species)) %in% sort(species$Scientific_Name)]
  warning(paste0(sp," is not found in a recognized species name, rename in `incidental_occ` which is in `prepare_data.R`"))
}


asian_shore_crab_2020 <- read.csv("recentdata/Asian_crab_2020_present_absent.csv")%>%
  st_as_sf(coords=c('Long','Lat'),crs=4326) %>% 
  filter(Presen_absent==1) %>% 
  mutate(Species = "Hemigrapsus_sanguineus",
         prov = paste("Maritimes Science Data:", Observer),
         Year = 2020,
         StnLocation = paste("ASC:", Site.Name)) %>% 
  dplyr::select(Species,StnLocation,Year,prov)
  

gulf_tunicate_incidental_2020 <- readxl::read_excel("recentdata/Gulf AIS data_biof_monit_incidental_AISNCP MAR_April 2021.xlsx",sheet=2,col_types =  "text") %>%
  mutate('Longitude (W)'=case_when(`Latitude (N)`=="*waiting for coordinate"~-61.91,   #fixing bad data entry
                                   `Longitude (W)`>0~as.numeric(`Longitude (W)`)*-1,
                                   TRUE~as.numeric(`Longitude (W)`)),
         'Latitude (N)'=case_when(`Latitude (N)`=="*waiting for coordinate"~"45.88",
                                  TRUE~`Latitude (N)`),
         'Latitude (N)'=as.numeric(`Latitude (N)`),
         Year=as.numeric(Year))%>% 
  filter(!is.na(`Longitude (W)`)) %>%
  st_as_sf(coords=c('Longitude (W)','Latitude (N)'),crs=4326)%>% 
  dplyr::rename(StnLocation=Location,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Caprella_mutica"="C mutica",
                "Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile") %>% 
  dplyr::select(-Province,-Comments) %>% 
  gather(key = "Species", value = "Presence",-StnLocation,-Year,-geometry) %>% 
  group_by(Species,StnLocation,Year) %>% 
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>% 
  ungroup() %>% 
  filter(Presence) %>% 
  mutate(prov="Gulf Science Data contact Renee.Bernier@dfo-mpo.gc.ca") %>% 
  st_cast('POINT')



gulf_tunicate_incidental_2021 <- readxl::read_excel("recentdata/Copy of P-A Table_2021 data_March2022.xlsx",sheet=2,col_types =  "text") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Location,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                #"Caprella_mutica"="C mutica",
                #"Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile") %>% 
  dplyr::select(-Province,-Comments) %>% 
  gather(key = "Species", value = "Presence",-StnLocation,-Year,-geometry) %>% 
  group_by(Species,StnLocation,Year) %>% 
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>% 
  ungroup() %>% 
  filter(Presence) %>% 
  mutate(prov="Gulf Science Data contact Renee.Bernier@dfo-mpo.gc.ca") %>% 
  st_cast('POINT')

# if(!all(sort(unique(gulf_tunicate_incidental_2020$Species)) %in% sort(species$Scientific_Name))){
#   sp <- sort(unique(gulf_tunicate_incidental_2020$Species))[!sort(unique(gulf_tunicate_incidental_2020$Species)) %in% sort(species$Scientific_Name)]
#   warning(paste0(sp," is not found in a recognized species name, rename in `gulf_tunicate_incidental` which is in `prepare_data.R`"))
# }


mar_incidental_2022<-read.csv("recentdata/Incidental_AIS_Reports_MAR.csv")%>%
  sf::st_as_sf(coords=c('Lon','Lat'),crs=4326) %>% 
  filter(Picture_Confirmation==1) %>% 
  mutate(Presence=as.logical(Picture_Confirmation),
         prov = paste("Maritimes Incidental Data:", "Contact Sarah.Kingsbury@dfo-mpo.gc.ca"),
         Year = 2021,
         ) %>% 
  dplyr::select(Species,StnLocation,Year,prov)

incidental_sites <- rbind(incidental_occ %>% 
                            dplyr::select(StnLocation),
                          asian_shore_crab_2020 %>% 
                            dplyr::select(StnLocation),
                          gulf_tunicate_incidental_2020 %>% 
                            dplyr::select(StnLocation),
                          gulf_tunicate_incidental_2021%>% 
                            dplyr::select(StnLocation),
                          mar_incidental_2022%>%
                            dplyr::select(StnLocation))%>%
  group_by(StnLocation) %>% 
  summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
  unique() %>% 
  st_transform(equidist) %>% 
  filter(geometry%>% 
           st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
           lengths()>0) %>% 
  st_transform(proj)

incidental <- bind_rows(incidental_occ %>%
                          mutate(across(.fns = as.character))%>%
                          as.data.table(),
                        asian_shore_crab_2020 %>%
                          mutate(across(.fns = as.character))%>%
                          as.data.table(),
                        gulf_tunicate_incidental_2020 %>%
                          mutate(across(.fns = as.character))%>%
                          as.data.table(),
                        gulf_tunicate_incidental_2021 %>%
                          mutate(across(.fns = as.character))%>%
                          as.data.table(),
                        mar_incidental_2022%>%
                          mutate(across(.fns=as.character))%>%
                          as.data.table()) %>% 
  unique() %>%
  dplyr::select(Species,StnLocation,Year,prov,link) %>% 
  right_join(incidental_sites,by = "StnLocation") %>% 
  st_sf()

saveRDS(incidental_sites,"outputdata/incidental_sites.rds")
saveRDS(incidental,"outputdata/incidental.rds")

#Genomics Data
metabarcode_2021<-read.csv("recentdata/metbarcoding_sites_2021.csv")%>%
  st_as_sf(coords=c("longitude","latitude"),crs=4326)%>%
  st_transform(proj)%>%
  dplyr::select(-sample_name)


metabarcoding_sites<-metabarcode_2021%>% 
    group_by(StnLocation) %>% 
    summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
    unique() %>% 
    st_transform(equidist) %>% 
    filter(geometry%>% 
             st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
             lengths()>0) %>% 
    st_transform(proj)
  
saveRDS(metabarcoding_sites, "outputdata/metabarcoding_sites.rds")
saveRDS(metabarcode_2021, "outputdata/metabarcoding.rds")

# Load and clean up monitoring data ---------------------------------------

# Maritimes Tunicates
maritimes_tunicate_monitor <- rbind(
  #esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/1"),
                                    esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/7"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/13"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/19"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/25"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/34"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/43"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/54"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/66"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/79"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/92"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/104"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/117"),
                                    # esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/136"),
                                    esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Aquatic_Invasive_Species/MapServer/154"))%>%
  dplyr::rename(geometry=geoms,Year = Year_Observed)%>% 
  st_transform(proj) %>% 
  dplyr::select(-OBJECTID,-Latitude,-Longitude,-FCName,-Prov,-sampler,-StnDescription,-StructureType,-StnNum)


maritimes_tunicate_2020 <- read.csv("recentdata/AIS_2020_present_absent.csv")%>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326)%>% 
  dplyr::rename(Didemnum_vexillum=Didemnum_Vexillum)%>% 
  st_transform(proj)%>% 
  dplyr::select(-Region,-StnNum) %>% 
  mutate(Year=2020)

maritimes_tunicate_2021 <- read.csv("recentdata/Final_AIS_2021_present_absent.csv")%>%
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326)%>%
  st_transform(proj)%>%
  dplyr::select(-StnNum) %>%
  mutate(Year=2021)

maritimes_tunicate_2022<-read.csv("recentdata/Final_AIS_2022_present_absent_MAR.csv")%>%
st_as_sf(coords=c('Longitude','Latitude'),crs=4326)%>%
  st_transform(proj)%>%
  dplyr::select(-StnNum) %>%
  mutate(Year=2022)

# Gulf Tunicates
# gulf_tunicate_monitor <- esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_Gulf_Region_Aquatic_Invasive_Species_Data/MapServer/0")
# the above Gulf data is included in the xlsx file below


gulf_tunicate_monitor_2020 <- readxl::read_excel("recentdata/Gulf AIS data_biof_monit_incidental_AISNCP MAR_April 2021.xlsx") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Station,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Caprella_mutica"="C mutica",
                "Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile")

gulf_tunicate_monitor_2021 <- readxl::read_excel("recentdata/Copy of P-A Table_2021 data_March2022.xlsx") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Station,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Caprella_mutica"="C mutica",
                "Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile")

gulf_tunicate_monitor_2022 <- readxl::read_excel("recentdata/Copy of 2022 P-A Data_AIS monitoring_Gulf Region_Jan2023.xlsx") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Station_Name)%>%
  dplyr::select(-Date_In, -Date_Out) %>%
  mutate(Year=2022)


monitoring_sites <- rbind(maritimes_tunicate_2020 %>% 
                            dplyr::select(StnLocation),
                          maritimes_tunicate_2021 %>% 
                            dplyr::select(StnLocation),
                          maritimes_tunicate_monitor %>% 
                            dplyr::select(StnLocation),
                          maritimes_tunicate_2022%>%
                            dplyr::select(StnLocation),
                          gulf_tunicate_monitor_2020 %>% 
                            dplyr::select(StnLocation),
                          gulf_tunicate_monitor_2021 %>% 
                            dplyr::select(StnLocation),
                          gulf_tunicate_monitor_2022 %>% 
                            dplyr::select(StnLocation)
                          ) %>% 
  group_by(StnLocation) %>% 
  summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
  unique() %>% 
  mutate(StnLocation=gsub("[ \t]+$","",StnLocation)) %>% 
  st_transform(equidist) %>% 
  filter(geometry%>% 
           st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
           lengths()>0) %>% 
  st_transform(proj)

monitoring <- bind_rows(maritimes_tunicate_2020 %>% 
                          as.data.table(),
                        maritimes_tunicate_2021 %>% 
                          as.data.table(),
                        maritimes_tunicate_monitor %>%
                          as.data.table(),
                        maritimes_tunicate_2022 %>%
                          as.data.table(),
                        gulf_tunicate_monitor_2020 %>% 
                          as.data.table(),
                        gulf_tunicate_monitor_2021%>%
                          as.data.table(),
                        gulf_tunicate_monitor_2022%>%
                          as.data.table()) %>% 
  dplyr::select(-geometry) %>% 
  unique()%>%
  gather(key = "Species", value = "Presence",-StnLocation,-Year) %>% 
  group_by(Species,StnLocation,Year) %>% 
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>% 
  ungroup() %>% 
  spread(key = "Species", value = "Presence") %>% 
  right_join(monitoring_sites,by = "StnLocation") %>% 
  st_sf() %>% 
  mutate(StnLocation=gsub("[ \t]+$","",StnLocation))

#leaflet::leaflet(monitoring_sites) %>% addTiles() %>% addMarkers()

saveRDS(monitoring_sites,"outputdata/monitoring_sites.rds")
saveRDS(monitoring,"outputdata/monitoring.rds")


if(!file.exists("spatialdata/gshhg-shp-2.3.7.zip")){
  print("Downloading Coastline")
  curl::curl_download(url="http://www.soest.hawaii.edu/pwessel/gshhg/gshhg-shp-2.3.7.zip",
                      destfile = "spatialdata/gshhg-shp-2.3.7.zip")
  utils::unzip("spatialdata/gshhg-shp-2.3.7.zip",exdir="spatialdata")
}


# GSHHS is from http://www.soest.hawaii.edu/pwessel/gshhs/index.html
maritimes <- st_read("spatialdata/GSHHS_shp/f/GSHHS_f_L1.shp") %>% 
  filter(st_is_valid(geometry)) %>%
  st_transform(equidist) %>% 
  st_crop(st_bbox(st_transform(searcharea,equidist))) %>% 
  st_union() %>% 
  st_cast('POLYGON') %>% 
  st_sf() 

source("functions.R")

#### set up transition matrix ####

print("Setting up transition matrix")
r <- raster(maritimes ,
            ext=extent(st_bbox(searcharea %>% st_transform(equidist))),
            res = 1000)
r <- fasterize(maritimes, r)
r@data@values[r@data@values==1] <- 1
r@data@values[is.na(r@data@values)] <- 10000
plot(r)
tr <- transition(r, mean, directions = 16, symm=TRUE)
saveRDS(tr,"outputdata/transition.rds")


#### NS vs  incidentals and monitoring and metabarcoding ####

print("Calculating in water distances for NS")
ns_incidental_dist <- do.call(rbind,(lapply(NS$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(incidental_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(ns_incidental_dist) <- NS$Lease_Identifier
colnames(ns_incidental_dist) <- incidental_sites$StnLocation
saveRDS(ns_incidental_dist,"outputdata/ns_incidental_dist.rds")


ns_monitoring_dist <- do.call(rbind,(lapply(NS$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(monitoring_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(ns_monitoring_dist) <- NS$Lease_Identifier
colnames(ns_monitoring_dist) <- monitoring_sites$StnLocation
saveRDS(ns_monitoring_dist,"outputdata/ns_monitoring_dist.rds")

ns_metabarcoding_dist <- do.call(rbind,(lapply(NS$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(metabarcoding_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(ns_metabarcoding_dist) <- NS$Lease_Identifier
colnames(ns_metabarcoding_dist) <- metabarcoding_sites$StnLocation
saveRDS(ns_metabarcoding_dist,"outputdata/ns_metabarcoding_dist.rds")

#### NB vs  incidentals and monitoring####

print("Calculating in water distances for NB")
nb_incidental_dist <- do.call(rbind,(lapply(NB$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(incidental_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(nb_incidental_dist) <- NB$Lease_Identifier
colnames(nb_incidental_dist) <- incidental_sites$StnLocation
saveRDS(nb_incidental_dist,"outputdata/nb_incidental_dist.rds")


nb_monitoring_dist <- do.call(rbind,(lapply(NB$geometry %>%
                                              st_transform(equidist),
                                            function(x) inwaterdistance(monitoring_sites %>%
                                                                          st_transform(equidist),
                                                                        x,
                                                                        tr))))
row.names(nb_monitoring_dist) <- NB$Lease_Identifier
colnames(nb_monitoring_dist) <- monitoring_sites$StnLocation
saveRDS(nb_monitoring_dist,"outputdata/nb_monitoring_dist.rds")

nb_metabarcoding_dist <- do.call(rbind,(lapply(NB$geometry %>%
                                                 st_transform(equidist),
                                               function(x) inwaterdistance(metabarcoding_sites %>%
                                                                             st_transform(equidist),
                                                                           x,
                                                                           tr))))
row.names(nb_metabarcoding_dist) <- NB$Lease_Identifier
colnames(nb_metabarcoding_dist) <- metabarcoding_sites$StnLocation
saveRDS(nb_metabarcoding_dist,"outputdata/nb_metabarcoding_dist.rds")

#### PEI vs  incidentals and monitoring ####


print("Calculating in water distances for PEI")
pei_incidental_dist <- do.call(rbind,(lapply(PEI$geometry %>%
                                               st_transform(equidist),
                                             function(x) inwaterdistance(incidental_sites %>%
                                                                           st_transform(equidist),
                                                                         x,
                                                                         tr))))
row.names(pei_incidental_dist) <- PEI$Lease_Identifier
colnames(pei_incidental_dist) <- incidental_sites$StnLocation
saveRDS(pei_incidental_dist,"outputdata/pei_incidental_dist.rds")
pei_monitoring_dist <- do.call(rbind,(lapply(PEI$geometry %>%
                                               st_transform(equidist),
                                             function(x) inwaterdistance(monitoring_sites %>%
                                                                           st_transform(equidist),
                                                                         x,
                                                                         tr))))
row.names(pei_monitoring_dist) <- PEI$Lease_Identifier
colnames(pei_monitoring_dist) <- monitoring_sites$StnLocation
saveRDS(pei_monitoring_dist,"outputdata/pei_monitoring_dist.rds")

pei_metabarcoding_dist <- do.call(rbind,(lapply(PEI$geometry %>%
                                                 st_transform(equidist),
                                               function(x) inwaterdistance(metabarcoding_sites %>%
                                                                             st_transform(equidist),
                                                                           x,
                                                                           tr))))
row.names(pei_metabarcoding_dist) <- PEI$Lease_Identifier
colnames(pei_metabarcoding_dist) <- metabarcoding_sites$StnLocation
saveRDS(pei_metabarcoding_dist,"outputdata/pei_metabarcoding_dist.rds")
