if(!require("spocc")) install.packages("spocc")
if(!require("robis")) install.packages("robis")
if(!require("devtools")) install.packages("devtools")
if(!require("esri2sf")) devtools::install_github("yonghah/esri2sf")
if(!require("arcpullr")) devtools::install_github("pfrater/arcpullr")
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
sf_use_s2(FALSE)



NS <- readRDS("spatialdata/NS.rds")
NB <- readRDS("spatialdata/NB.rds")
PEI <- readRDS("spatialdata/PEI.rds")

species <- read.csv("commonnames.csv")



# Load and clean up incidental data ---------------------------------------

searcharea <- c(NS$geometry,NB$geometry,PEI$geometry) %>% 
  st_combine() %>% 
  st_convex_hull() %>% 
  st_sfc()%>%
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
  ))

incidental_occ<-incidental_occ%>%
  mutate(Species=case_when(name %in% c("Aequipecten irradians (Lamarck, 1819)","Argopecten irradians (Lamarck, 1819)", "Argopecten irradians", "Argopecten irradians irradians", "Aequipecten irradians sablensis A.H.Clarke, 1965" ,"Argopecten irradians sablensis (A.H.Clarke, 1965)") ~ "Argopecten_irradians",
                           name %in% c("Ascidiella aspersa (Müller, 1776)", "Ascidiella aspersa (M?ller, 1776)","Ascidiella aspersa" ) ~ "Ascidiella_aspersa",
                           name %in% c("BOLD:AAA7687","BOLD:ACL8382","Carcinus maenas (Linnaeus, 1758)", "Carcinus maenas") ~ "Carcinus_maenas",
                           name %in% c("Botrylloides violaceus Oka, 1927", "Botrylloides violaceus") ~ "Botrylloides_violaceus",
                           name %in% c("Botryllus schlosseri (Pallas, 1766)", "Botryllus schlosseri") ~ "Botryllus_schlosseri",
                           name %in% c("Caprella mutica Schurin, 1935","BOLD:AAE7686", "Caprella mutica") ~ "Caprella_mutica",
                           name %in% c("Carcinus maenas (Linnaeus, 1758)")~"Carcinus maenas",
                           name %in% c("Ciona intestinalis (Linnaeus, 1767)","Ciona intestinalis tenella (Stimpson, 1852)","Ciona tenella (Stimpson, 1852)", "Ciona intestinalis" ) ~ "Ciona_intestinalis",
                           name %in% c("Codium fragile (Suringar) Hariot","Codium fragile subsp. fragile","Codium fragile subsp. tomentosoides (Goor) P.C.Silva","Codium fragile tomentosoides", "Codium fragile", "Codium fragile (Suringar) Har." ) ~ "Codium_fragile",
                           name %in% c("Didemnum vexillum Kott, 2002", "Didemnum vexillum") ~ "Didemnum_vexillum",
                           name %in% c("Diplosoma listerianum (Milne Edwards, 1841)", "Diplosoma listerianum") ~ "Diplosoma_listerianum",
                           name %in% c("Hemigrapsus sanguineus (De Haan, 1835)","Hemigrapsus sanguineus (de Haan, 1835)", "Hemigrapsus sanguineus") ~ "Hemigrapsus_sanguineus",
                           name %in% c("Juxtacribrilina mutabilis (Ito, Onishi & Dick, 2015)","Juxtacribrilina mutabilis")~"Juxtacribrilina_mutabilis",
                           name %in% c("Membranipora membranacea (Linnaeus, 1767)", "Flustra membranacea Linnaeus, 1767", "Membranipora membranacea" ) ~ "Membranipora_membranacea",
                           name %in% c("Oncorhynchus mykiss (Walbaum, 1792)", "Oncorhynchus mykiss") ~ "Oncorhynchus_mykiss",
                           name %in% c("Ostrea edulis (Linnaeus, 1767)","Ostrea edulis Linnaeus, 1758","Ostrea edulis") ~ "Ostrea_edulis",
                           name %in% c("Sargassum muticum (Yendo) Fensholt", "Sargassum muticum")~"Sargassum_muticum",
                           name %in% c("Styela clava Herdman, 1881", "Styela clava") ~ "Styela_clava",
                           TRUE ~ name),
         Year=as.numeric(substr(date,1,4)))

if(!all(sort(unique(incidental_occ$Species)) %in% sort(species$R_Name))){
  sp <- sort(unique(incidental_occ$Species))[!sort(unique(incidental_occ$Species)) %in% sort(species$R_Name)]
  warning(paste0(sp," is not found in a recognized species name, rename in `incidental_occ` which is in `prepare_data.R`"))
}


asian_shore_crab_2020 <- rbind(arcpullr::get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/226"),
                               arcpullr::get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/227") )%>%
  dplyr::rename(geometry=geoms, StnLocation=stn_location)%>% 
  st_transform(proj) %>% 
  dplyr::select(-OBJECTID,-latitude,-longitude,-Region)%>%
  dplyr::filter(Count>=1)
  
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

gulf_tunicate_incidental_2023<-readxl::read_excel("recentdata/Gulf_incidental_new detections_2023.xlsx",col_types =  "text") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
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

gulf_tunicate_incidental_2024<-readxl::read_excel("recentdata/Gulf 2024 AIS Data_Feb2025.xlsx",sheet=2,col_types =  "text") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=`Location Name`,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Membranipora_membranacea"="M membranacea",
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile",
                "Juxtacribrilina_mutabilis"="J mutabilis") %>% 
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

mar_incidental <- read.csv("recentdata/Incidental_AIS_Reports_MAR.csv")%>%
  sf::st_as_sf(coords=c('Lon','Lat'),crs=4326) %>% 
  filter(Picture_Confirmation==1) %>% 
  mutate(Presence=as.logical(Picture_Confirmation),
         prov = paste("Maritimes Incidental Data:", "Contact Sarah.Kingsbury@dfo-mpo.gc.ca"),
         Year = 2021,
         ) %>% 
  dplyr::select(Species,StnLocation,Year,prov)%>%
  filter(Species %in% species$R_Name) #only keep species of relevance to I&T transfers

incidental_sites <- rbind(
  # incidental_occ %>%
  #   dplyr::select(StnLocation),
  asian_shore_crab_2020 %>% 
    dplyr::select(StnLocation),
  gulf_tunicate_incidental_2020 %>% 
    dplyr::select(StnLocation),
  gulf_tunicate_incidental_2021%>% 
    dplyr::select(StnLocation),
  gulf_tunicate_incidental_2023%>% 
    dplyr::select(StnLocation),
  gulf_tunicate_incidental_2024%>% 
    dplyr::select(StnLocation),
  mar_incidental%>%
    dplyr::select(StnLocation)
)%>%
  na.omit()%>%
  dplyr::group_by(StnLocation) %>% 
  dplyr::summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
  unique() %>% 
  sf::st_transform(equidist) %>% 
  dplyr::filter(geometry%>% 
                  st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
                  lengths()>0) %>% 
  sf::st_transform(proj)

#Note: incidental reports from iNat and GBiF will be moved to a public reports tab because these are unverified reports
incidental <-  dplyr::bind_rows(
  # incidental_occ %>%
  #   mutate(across(.fns = as.character))%>%
  #   as.data.table(),
  asian_shore_crab_2020 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  gulf_tunicate_incidental_2020 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  gulf_tunicate_incidental_2021 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  gulf_tunicate_incidental_2023 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  gulf_tunicate_incidental_2024 %>%
    dplyr::mutate(across(.fns = as.character))%>%
    as.data.table(),
  mar_incidental%>%
    dplyr::mutate(across(.fns=as.character))%>%
    as.data.table()
) %>% 
  unique() %>%
  dplyr::select(Species,StnLocation,Year,prov) %>% 
  dplyr::right_join(incidental_sites,by = "StnLocation") %>% 
  st_sf()%>%
  na.omit()

saveRDS(incidental_sites,"outputdata/incidental_sites.rds")
saveRDS(incidental,"outputdata/incidental.rds")

#Public incidental reports

publicdata_sites <- incidental_occ %>%
  dplyr::select(StnLocation)%>%
  dplyr::group_by(StnLocation) %>% 
  dplyr::summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
  unique() %>% 
  sf::st_transform(equidist) %>% 
  dplyr::filter(geometry%>% 
                  st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
                  lengths()>0) %>% 
  sf::st_transform(proj)

publicdata<- incidental_occ %>%
  dplyr::mutate(across(.fns = as.character))%>%
  as.data.table()%>%
  unique() %>%
  dplyr::select(Species,StnLocation,Year,prov) %>% 
  dplyr::right_join(publicdata_sites,by = "StnLocation") %>% 
  st_sf()%>%
  na.omit()

saveRDS(publicdata_sites,"outputdata/publicdata_sites.rds")
saveRDS(publicdata,"outputdata/publicdata.rds")

#Genomics Data
metabarcode_2022<-read.csv("recentdata/metbarcoding_MAR_2022.csv")%>%
  st_as_sf(coords=c("longitude","latitude"),crs=4326)%>%
  st_transform(proj)%>%
  dplyr::select(-sample_name, -LAB)%>%
  as.data.frame()

metabarcode_2023<-readxl::read_excel("recentdata/MAR-Metabarcoding-ESV_2023.xlsx",sheet=1,col_types =  "text") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326)%>%
  st_transform(proj)%>% 
dplyr::rename(StnLocation=Location,
              "Botrylloides_violaceus"="Botrylloides violaceus",
              "Caprella_mutica"="Caprella mutica",
              "Ascidiella_aspersa" ="Ascidiella aspersa",
              "Ciona_intestinalis"="Ciona intestinalis",
              "Membranipora_membranacea"="Membranipora membranacea")%>%
  mutate(Year=as.factor("2023"))%>%
  dplyr::select("Year","StnLocation", "Botrylloides_violaceus", "Caprella_mutica", "Ascidiella_aspersa", "Ciona_intestinalis", "Membranipora_membranacea", "geometry")%>%
  as.data.frame()

eDNA_2023<- readxl::read_excel("recentdata/MAR-Metabarcoding-qPCR_2023.xlsx",sheet=1,col_types =  "text") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326)%>%
  st_transform(proj)%>%
  mutate(Botrylloides_violaceus=as.character(case_when(`Botrylloides violaceus...15`=="Detected"~1,
                                                    `Botrylloides violaceus...31`=="Detected"~1)),
         Caprella_mutica=as.character(case_when(`Caprella mutica`=="Detected"~1)),
         Ascidiella_aspersa=as.character(case_when(`Ascidiella aspersa...13`=="Detected"~1,
                                                    `Ascidiella aspersa...27`=="Detected"~1)),
         Ciona_intestinalis=as.character(case_when(`Ciona intestinalis...17`=="Detected"~1)),
         Membranipora_membranacea=as.character(case_when(`Membranipora membranacea...28`=="Detected"~1,
                                                `Membranipora membranacea...19`=="Detected"~1)),
         Botryllus_schlosseri=as.character(case_when(`Botryllus schlosseri`=="Detected"~1)),
         Styela_Clava=as.character(case_when(`Styela Clava`=="Detected"~1)),
         Hemigrapsus_sanguineus=as.character(case_when(`Hemigrapsus sanguineus`=="Detected"~1)),
         Didemnum_vexillum=as.character(case_when(`Didemnum vexillum`=="Detected"~1)),
         Diplosoma_listerianum=as.character(case_when(`Diplosoma listerianum`=="Detected"~1))
         )%>%
  dplyr::rename(StnLocation=Site)%>%
  mutate(Year=as.factor("2023"))%>%
  dplyr::select("Year", "StnLocation", "Botrylloides_violaceus", "Caprella_mutica", "Ascidiella_aspersa", "Ciona_intestinalis", "Membranipora_membranacea","Botryllus_schlosseri", "Styela_Clava","Hemigrapsus_sanguineus", "Didemnum_vexillum", "Diplosoma_listerianum", "geometry")%>%
  as.data.frame()

eDNA_2023[is.na(eDNA_2023)]<-0

#we only have the qPCR eDNA results from 2024 thus far. 
eDNA_2024<-readxl::read_excel("recentdata/RESULTS-Preliminary_Harbour Monitoring Project_targeted qPCR.xlsx")%>% 
  filter(!Latitude == "NA")%>%
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326)%>%
  st_transform(proj)%>%
  mutate(Botrylloides_violaceus=as.character(case_when(`B.  violaceus Site result`=="Detected"~1)),
         Caprella_mutica=as.character(case_when(`C. mutica Site result`=="Detected"~1)),
         Ascidiella_aspersa=as.character(case_when(`A. aspersa Site result`=="Detected"~1)),
         Ciona_intestinalis=as.character(case_when(`C. intestinalis Site result`=="Detected"~1)),
         Botryllus_schlosseri=as.character(case_when(`B. schlosseri Site result`=="Detected"~1)),
         Styela_Clava=as.character(case_when(`S. clava Site result`=="Detected"~1)),
         Diplosoma_listerianum=as.character(case_when(`D. listerianum Site result`=="Detected"~1))
  )%>%
  dplyr::rename(StnLocation=Location)%>%
  mutate(Year=as.factor("2024"))%>%
  dplyr::select("Year", "StnLocation", "Botrylloides_violaceus", "Caprella_mutica", "Ascidiella_aspersa", "Ciona_intestinalis", "Botryllus_schlosseri", "Styela_Clava","Diplosoma_listerianum", "geometry")%>%
  as.data.frame()

eDNA_2024[is.na(eDNA_2024)]<-0 
  
metabarcoding_sites<-rbind(metabarcode_2022%>% 
                             dplyr::select(StnLocation, geometry),
                           metabarcode_2023%>% 
                             dplyr::select(StnLocation, geometry),
                           eDNA_2023%>% 
                             dplyr::select(StnLocation, geometry),
                           eDNA_2024%>% 
                             dplyr::select(StnLocation, geometry))%>%
  na.omit()%>%
  st_as_sf()%>%
  dplyr::group_by(StnLocation) %>% 
  dplyr::summarize(geometry = st_cast(st_centroid(st_union(geometry)),"POINT")) %>% 
  unique() %>% 
  sf::st_transform(equidist) %>% 
   dplyr::filter(geometry%>% 
           st_intersects(st_as_sfc(st_bbox(st_transform(searcharea,equidist)))) %>% 
           lengths()>0) %>% 
  st_transform(proj)

metabarcoding<-dplyr::bind_rows(
  metabarcode_2022%>%
    dplyr::mutate(across(.fns = as.character))%>%
    pivot_longer(cols = -c(StnLocation, Year, geometry), names_to = "Species", values_to = "Presence")%>%
    filter(Presence=="1"),
  metabarcode_2023%>%
    dplyr::mutate(across(.fns = as.character))%>%
    pivot_longer(cols = -c(StnLocation, Year, geometry), names_to = "Species", values_to = "Presence")%>%
    filter(Presence=="1"),
  eDNA_2023%>%
    dplyr::mutate(across(.fns = as.character))%>%
    pivot_longer(cols = -c(StnLocation, Year, geometry), names_to = "Species", values_to = "Presence")%>%
    filter(Presence=="1"),
  eDNA_2024%>%
    dplyr::mutate(across(.fns = as.character))%>%
    pivot_longer(cols = -c(StnLocation, Year, geometry), names_to = "Species", values_to = "Presence")%>%
    filter(Presence=="1")
)%>%
  unique() %>%
  dplyr::select(Species,StnLocation,Year) %>% 
  dplyr::right_join(metabarcoding_sites,by = "StnLocation") %>% 
  st_sf()%>%
  na.omit()

saveRDS(metabarcoding_sites, "outputdata/metabarcoding_sites.rds")
saveRDS(metabarcoding, "outputdata/metabarcoding.rds")

# Load and clean up monitoring data ---------------------------------------

# Maritimes Tunicates
#Biofouling data for MAR is available here: https://open.canada.ca/data/en/dataset/8d87f574-0661-40a0-822f-e9eabc35780d
#Please note that due to the file size, some of the data will be accessed in the first pull and the rest accessed in teh second pull. Data includes 2006-2023
maritimes_tunicate_monitor_1 <- rbind(
  (lapply(2:13,function(x)
     {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for A. aspersa
     })%>%
     bind_rows()),
  (lapply(29:46,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for B. violaceus
  })%>%
    bind_rows()),
  (lapply(68:85,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for B. schlosseri
  })%>%
    bind_rows()),
  (lapply(107:118,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for C. mutica
  })%>%
    bind_rows()),
  (lapply(257:268,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for S. clava
  })%>%
    bind_rows())
  )%>%
  dplyr::rename(geometry=geoms,Year = year, StnLocation=stn_location)%>% 
  st_transform(proj) %>% 
  dplyr::select(-OBJECTID,-latitude,-longitude,-cover_index,-province,-stn_num)

maritimes_tunicate_monitor_2 <- rbind(
  (lapply(134:151,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for C. intestinalis
  })%>%
    bind_rows()),
  (lapply(173:184,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for D. vexillum
  })%>%
    bind_rows()),
  (lapply(200:211,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for D. listerianum
  })%>%
    bind_rows()),
  (lapply(230:241,function(x)
  {arcpullr::get_spatial_layer(paste0("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/dfo_maritimes_biofouling_monitoring_program_en/MapServer/",x))#download all years of data at once for M. membranacea
  })%>%
    bind_rows())
)%>%
  dplyr::rename(geometry=geoms,Year = year, StnLocation=stn_location)%>% 
  st_transform(proj) %>% 
  dplyr::select(-OBJECTID,-latitude,-longitude,-cover_index,-province,-stn_num)

#the 2024 data is not yet availabel online and must be entered manually
                                
maritimes_tunicate_2024 <- read.csv("recentdata/Final_AIS_tunicate_2006_2024_present_absent_long.csv")%>%
  filter(year=="2024")%>%
  st_as_sf(coords=c('longitude','latitude'),crs=4326)%>%
  dplyr::rename(species_name=species.name,Year = year, StnLocation=stn.location)%>%
  st_transform(proj)%>%
  dplyr::select(-cover_index,-province,-stn_num) 

maritimes_tunicate_monitor<-rbind(maritimes_tunicate_monitor_1,
                                  maritimes_tunicate_monitor_2,
                                  maritimes_tunicate_2024)%>%
  mutate(Species=gsub(" ","_",species_name))%>%
  dplyr::select(-species_name)%>%
  mutate(Presence=as.integer(1))%>%
  st_transform(proj)


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
                "Codium_fragile"="C fragile")%>%
  dplyr::select(-Province)%>%
  st_transform(proj)

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
                "Codium_fragile"="C fragile")%>%
  dplyr::select(-Province)%>%
  st_transform(proj)

gulf_tunicate_monitor_2022 <- readxl::read_excel("recentdata/Copy of 2022 P-A Data_AIS monitoring_Gulf Region_Jan2023.xlsx") %>% 
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=Station_Name)%>%
  dplyr::select(-Date_In, -Date_Out, -Province) %>%
  mutate(Year=2022)%>%
  st_transform(proj)

#missing data for 2023 from Gulf

gulf_tunicate_montior_2024<-readxl::read_excel("recentdata/Gulf 2024 AIS Data_Feb2025.xlsx", sheet=1, col_types = 'text') %>% 
  filter(!Latitude=="NA")%>%
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% 
  dplyr::rename(StnLocation=`Location/Station Name`,
                "Botryllus_schlosseri"="B schlosseri",
                "Botrylloides_violaceus"="B violaceus",
                "Ciona_intestinalis"="C intestinalis",
                "Styela_clava"="S clava",
                "Caprella_mutica"="C mutica",
                "Membranipora_membranacea"="M membranacea", 
                "Carcinus_maenas"="C maenas",
                "Codium_fragile"="C fragile",
                "Juxtacribrilina_mutabilis"="J mutabilis"
                )%>%
  dplyr::select(-Province, -Comments)%>%
  st_transform(proj)

gulf_tunicate_monitor<-rbind(gulf_tunicate_incidental_2020,
                             gulf_tunicate_incidental_2021,
                             gulf_tunicate_incidental_2023,
                             gulf_tunicate_incidental_2024)%>%
  mutate(Presence=as.integer(Presence))%>%
  dplyr::select(-prov)
  

monitoring_sites <- rbind(maritimes_tunicate_monitor%>% 
                            dplyr::select(StnLocation), 
                          gulf_tunicate_monitor %>% 
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

monitoring <- rbind(maritimes_tunicate_monitor%>%
                          as.data.table(),
                        gulf_tunicate_monitor %>% 
                          as.data.table()) %>% 
  dplyr::select(-geometry) %>% 
  unique()%>%
  #gather(key = "Species", value = "Presence",-StnLocation,-Year) %>%
  group_by(Species,StnLocation,Year) %>% 
  #unique()%>%
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>%
  ungroup() %>%
  #spread(key = "Species", value = "Presence") %>%
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
library(raster)
library(fasterize)
library(sp)

print("Setting up transition matrix")
# searchbox<-extent(st_bbox(searcharea %>% st_transform(equidist)))
r <- raster(maritimes,
            # xmn=628730, #issue with extent and x and y values for min/max. 
            # xmx=1374056 , #Maritimes polygon is cropped to the box extent above, therefore, no need to repeat the operation here.
            # ymn=603784.7,
            # ymx=1311254,
            #ext=searchbox,
            #ext=extent(st_bbox(searcharea %>% st_transform(equidist))),
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
