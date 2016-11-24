# This code convers the input shapefiles for LSOA centroids annd boundaries into Rds

#Library
library(sp)
library(rgdal)
library(maptools)

#Set projection
proj <- CRS("+proj=longlat +init=epsg:3857") #Web mecator used by OSM

#Convert LSOA Centroids
cents <- readOGR(dsn =  "../pct-lsoa/Data/01_Raw/LSOA", layer = "england_lsoa_2011_centroids")
cents <- spTransform(cents, proj)
cents@data <- cents@data[,c(2,1)]
saveRDS(cents,"../pct-lsoa/Data/02_Input/LSOA_cents.Rds")

#Convert LSOA Boundaries
zones <- readOGR(dsn =  "../pct-lsoa/Data/01_Raw/LSOA", layer = "england_lsoa_2011_gen_clipped")
zones <- spTransform(zones, proj)
zones@data <- zones@data[,c(2,1)]
saveRDS(zones,"../pct-lsoa/Data/02_Input/LSOA_zones.Rds")
