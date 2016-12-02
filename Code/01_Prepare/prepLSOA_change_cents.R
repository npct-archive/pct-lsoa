# This code convers the input shapefiles for LSOA centroids annd boundaries into Rds
#################
#Development COde Not WORKING
##################

#Library
library(sp)
library(rgdal)
library(maptools)
library(utils)


#Set projection
proj <- CRS("+proj=longlat +init=epsg:3857") #Web mecator used by OSM

#Convert LSOA Centroids
cents <- readOGR(dsn =  "../pct-lsoa/Data/01_Raw/LSOA", layer = "england_lsoa_2011_centroids")
cents <- spTransform(cents, proj)
#lat <- cents@coords[,2]
#lng <- cents@coords[,1]
#lat <- 54.54590
#lng <- -1.240606
near <- cents
#near <- near[1:5,]
pb <- winProgressBar(title="Finding the nearest point on the road network", min=0, max=nrow(near), initial=0, label="0 centroids done")
progress = 0

for(i in 1:nrow(near)){
  nearest <- nearest_cyclestreets(near@coords[i,2],near@coords[i,1])
  near@coords[i,] <- nearest@coords
  progress = progress + 1
  info <- sprintf("%d centroids done", progress)
  setWinProgressBar(pb, progress, label = info)
  if(i %% 1000 == 0){
    print("taking a 5 second rest")
    Sys.sleep(5)
    }
}
close(pb)
  
osm <- nearest_cyclestreets(lat,lng)
cents@data <- cents@data[,c(2,1)]
saveRDS(cents,"../pct-lsoa/Data/02_Input/LSOA_cents.Rds")

#Convert LSOA Boundaries
zones <- readOGR(dsn =  "../pct-lsoa/Data/01_Raw/LSOA", layer = "england_lsoa_2011_gen_clipped")
zones <- spTransform(zones, proj)
zones@data <- zones@data[,c(2,1)]
saveRDS(zones,"../pct-lsoa/Data/02_Input/LSOA_zones.Rds")
