library(sf)
library(raster)
library(fasterize)
library(gdistance)
library(rnaturalearth)
library(data.table)
library(tidyverse)

#### global variables ####
gdbpath <- "//dcnsbiona01b/edc_v1_shr6/HMD/HF&LD/GIS_FPP/InteractiveMaps/Working/PMF_Working.gdb"
proj <- "+proj=longlat +datum=WGS84"

NS <- read_sf(gdbpath, layer="NS_Aquaculture_Leases_2018") %>% st_transform(crs=proj)
# PEI <- read_sf(gdbpath, layer="PEI_Aquaculture_Leases_2019") %>% st_transform(crs=proj)
# NB <- read_sf(gdbpath, layer="NB_Aquaculture_Leases_2019") %>% st_transform(crs=proj)

# load and clean up data
greencrab_all <- read_sf(gdbpath, layer="GreenCrab_Occurrences_DiBacco") %>% 
  st_transform(crs=proj) %>% 
  filter(Lon<(-59)) %>% 
  mutate(StnLocation = LOCATION_N) 

greencrab_sites <- greencrab_all %>% 
  group_by(StnLocation) %>% 
  summarize(Shape = st_cast(st_centroid(st_union(Shape)),"POINT")) %>% 
  unique()

greencrab <- greencrab_all %>% 
  as.data.table() %>% 
  mutate(Year = YEAR_Colle,
         Species = "Carcinus maenas") %>% 
  dplyr::select(Species,StnLocation,Year) %>% 
  left_join(greencrab_sites,by = "StnLocation") %>% 
  st_sf()
saveRDS(greencrab_sites,"greencrab_sites.rds")
saveRDS(greencrab,"greencrab.rds")


tunicates2018 <- read_sf(gdbpath, layer="Tunicate_Community_Composition_2018") %>% 
  st_transform(crs=proj) %>% 
  dplyr::select(-Region,-Lat,-Long_,-StnNum) %>% 
  mutate(Year=2018)

tunicates_all <- read_sf(gdbpath, layer="CESD_Aquatic_Invasive_Tunicates") %>% 
  st_transform(crs=proj)%>% 
  mutate(Year = Year_Observed) %>% 
  dplyr::select(-Year_Observed,-Latitude,-Longitude,-FCName,-StnNum,-Prov,-sampler,-StnDescription,-StructureType) 

tunicates_sites <- rbind(tunicates_all %>% 
                         dplyr::select(StnLocation),
                       tunicates2018 %>% 
                         dplyr::select(StnLocation)) %>% 
  group_by(StnLocation) %>% 
  summarize(Shape = st_cast(st_centroid(st_union(Shape)),"POINT")) %>% 
  unique()

tunicates <- bind_rows(tunicates_all %>% 
                         as.data.table(),
                       tunicates2018 %>% 
                         as.data.table()) %>% 
  dplyr::select(-Shape) %>% 
  gather(key = "Species", value = "Presence",-StnLocation,-Year) %>% 
  group_by(Species,StnLocation,Year) %>% 
  summarize(Presence = if_else(all(is.na(Presence)),
                               FALSE,
                               any(Presence>0,na.rm = TRUE))) %>% 
  ungroup() %>% 
  spread(key = "Species", value = "Presence") %>% 
  left_join(tunicates_sites,by = "StnLocation") %>% 
  st_sf() 
saveRDS(tunicates_sites,"tunicates_sites.rds")
saveRDS(tunicates,"tunicates.rds")


CAN <- ne_states(country=c("Canada"),returnclass = "sf")
USA <- ne_states(country=c("United States of America"),returnclass = "sf")

bbox <- st_bbox(st_buffer(rbind(tunicates_sites,greencrab_sites),2))
# GSHHS is from http://www.soest.hawaii.edu/pwessel/gshhs/index.html
maritimes <- st_read("../GSHHS_shp/f/GSHHS_f_L1.shp")%>%
  st_crop(bbox)%>% 
  st_union() %>% 
  st_cast('POLYGON') %>% 
  st_sf()

source("functions.R")

#### set up transition matrix ####
r <- raster(maritimes,
            ext=extent(c(-68.5,-59,43,47.5)),
            # ext=extent(c(-61.3,-60.7,45.5,45.7)),
            res = 0.0025) # this SHOULD be ~0.0025
r <- fasterize(maritimes, r)
r@data@values[r@data@values==1] <- 1
r@data@values[is.na(r@data@values)] <- 10000
plot(r)
tr <- transition(r, mean, directions = 16, symm=TRUE)
saveRDS(tr,"transition.rds")


#### NS vs green crabs and tunicates ####

nsgcdist <- do.call(rbind,(lapply(NS$Shape, function(x) inwaterdistance(greencrab_sites,x,tr))))
row.names(nsgcdist) <- NS$Lease_Identifier
colnames(nsgcdist) <- greencrab_sites$StnLocation
saveRDS(nsgcdist,"nsgcdist.rds")
nstudist <- do.call(rbind,(lapply(NS$Shape, function(x) inwaterdistance(tunicates_sites,x,tr))))
row.names(nstudist) <- NS$Lease_Identifier
colnames(nstudist) <- tunicates_sites$StnLocation
saveRDS(nstudist,"nstudist.rds")

