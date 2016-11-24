# This code convers the input shapefiles for LSOA centroids annd boundaries into Rds

#Library
library(sp)
library(rgdal)
library(maptools)

#Set projection
proj <- CRS("+init=epsg:3857") #Web mecator used by OSM

#Convert LSOA Centroids
cents <- readOGR(dsn =  "../pct-lsoa/Data/01 Raw/LSOA", layer = "england_lsoa_2011_centroids")
cents <- spTransform(cents, proj)
saveRDS(cents,"../pct-lsoa/Data/02 Input/LSOA_cents.Rds")

#Convert LSOA Boundaries
zones <- readOGR(dsn =  "../pct-lsoa/Data/01 Raw/LSOA", layer = "england_lsoa_2011_gen_clipped")
zones <- spTransform(zones, proj)
saveRDS(zones,"../pct-lsoa/Data/02 Input/LSOA_zones.Rds")
