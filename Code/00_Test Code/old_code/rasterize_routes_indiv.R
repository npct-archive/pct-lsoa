
library(sp)
library(rgeos)
library(velox)
library(raster)
library(dplyr)
library(rgdal)
library(utils)

#Inputs
routes_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus_fix.Rds")
flow_data = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")

#Parameters
resolution = 5 #Equal to OSM zoom level 13

#Prep and Join data
flow_data <- flow_data[,c("id","bicycle_16p")]
routes_master@data <- left_join(routes_master@data, flow_data, by = c("ID" = "id"))

#Remove Unneeded data
routes_master@data = routes_master@data[,c("ID","bicycle_16p")]
remove(flow_data) #,groups_master)
routes_master = routes_master[routes_master$bicycle_16p >0,]

routes_master <- spTransform(routes_master, CRS( "+init=epsg:27700" ) )
routes_poly <- gBuffer(routes_master, byid = T, width = 10) 

remove(routes_master)



#loop though each group and rasterize
print(paste0("There are ",nrow(routes_poly)," lines to do, getting started at ",Sys.time() ))
pb <- winProgressBar(title="Rastering progress bar", min=0, max=nrow(routes_poly), initial=0, label="0 routes done")
progress = 0
for(k in 1){ #:nrow(routes_poly) ){  
  lines2raster = routes_poly[k,]
  ras <- raster(resolution = resolution, ext = extent(lines2raster), crs= "+init=epsg:27700", vals = 0)
  sub <- velox(ras) 
  sub$rasterize(lines2raster, field="bicycle_16p", band=1, background = 0)
  sub$write(path = paste0("../pct-lsoa/Data/03_Intermediate/temp/rasters/indiv/rf-nat-4p-",k,".tif"))
  remove(sub)
  #print(paste0("Done group ",k," at ",Sys.time() ))
  progress = progress + 1
  info <- sprintf("%d lines done", progress)
  setWinProgressBar(pb, progress, label = info)
}
close(pb)


#Done the 4p lines

remove(routes_poly)

#Inputs
routes_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_less3p_fix.Rds")
flow_data = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")

#Parameters
resolution = 5 #Equal to OSM zoom level 13

#Prep and Join data
flow_data <- flow_data[,c("id","bicycle_16p")]
routes_master@data <- left_join(routes_master@data, flow_data, by = c("ID" = "id"))

#Remove Unneeded data
routes_master@data = routes_master@data[,c("ID","bicycle_16p")]
remove(flow_data) #,groups_master)
routes_master = routes_master[routes_master$bicycle_16p >0,]

routes_master <- spTransform(routes_master, CRS( "+init=epsg:27700" ) )
routes_poly <- gBuffer(routes_master, byid = T, width = 10) 

remove(routes_master)



#loop though each group and rasterize
print(paste0("There are ",nrow(routes_poly)," lines to do, getting started at ",Sys.time() ))
pb <- winProgressBar(title="Rastering progress bar", min=0, max=nrow(routes_poly), initial=0, label="0 routes done")
progress = 0
for(k in 70322:nrow(routes_poly) ){  
  lines2raster = routes_poly[k,]
  ras <- raster(resolution = resolution, ext = extent(lines2raster), crs= "+init=epsg:27700", vals = 0)
  sub <- velox(ras) 
  sub$rasterize(lines2raster, field="bicycle_16p", band=1, background = 0)
  sub$write(path = paste0("../pct-lsoa/Data/03_Intermediate/temp/rasters/indiv/rf-nat-l3-",k,".tif"))
  remove(sub)
  #print(paste0("Done group ",k," at ",Sys.time() ))
  progress = progress + 1
  info <- sprintf("%d lines done", progress)
  setWinProgressBar(pb, progress, label = info)
}
close(pb)



