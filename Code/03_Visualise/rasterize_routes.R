#Convert routes to a raster based on the number of cyclists
#WHen rasterizing it is necessary to not have overlapping lines

#4) Loop though each group convert to raster then add up rasters

#libs
library(sp)
library(rgeos)
library(velox)
library(raster)
#library(rgeos)
#library(raster)
#library(sp) 
#library(rgdal)
library(dplyr)
library(utils)
#library(dplyr)
#library(gdalUtils)

#Inputs
routes_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus.Rds")
groups_master = readRDS("../pct-lsoa/Data/03_Intermediate/groups/rf_nat_4plus_bearing.Rds")
flow_data = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")

#Parameters
size_limit = 5000
resolution = 19.093 #Equal to OSM zoom level 13

#Prep and Join data
groups_master$id_old = as.character(groups_master$id_old)
groups_master = groups_master[!is.na(groups_master$group),]
flow_data = flow_data[,c("id","bicycle_16p")]
flow_data = flow_data[flow_data$id %in% groups_master$id_old,]
routes_master = routes_master[routes_master$ID %in% groups_master$id_old,]
flow_data = left_join(flow_data,groups_master, by = c("id" = "id_old"))
routes_master@data = left_join(routes_master@data, flow_data, by = c("ID" = "id"))

#Remove Unneeded data
routes_master@data = routes_master@data[,c("ID","bicycle_16p","group")]
remove(flow_data,groups_master)
routes_master = routes_master[routes_master$bicycle_16p >0,]

#test subset #####################
#routes_master_backup = routes_master
#routes_master = routes_master_backup[1:1000,]
#routes_master = routes_master_backup[routes_master_backup$group < 2,]


#plot(routes_master)
######################################

group_list = as.data.frame(table(routes_master$group))
group_list$Var1 = as.integer(group_list$Var1)


routes_master <- spTransform(routes_master, CRS( "+init=epsg:27700" ) )
routes_poly <- gBuffer(routes_master, byid = T, width = 8) 
raster_master <- raster(resolution = resolution, ext = extent(routes_master), crs= "+init=epsg:27700", vals = 0)
dataType(raster_master) <- "INT2U"
vx <- velox(raster_master, extent=extent(routes_master), res=c(resolution,resolution), crs="+init=epsg:27700")
remove(raster_master)

#loop though each group and rasterize
#then add the raster onto the last raster
for(k in group_list$Var1 ){  #max(groups[,2])
  lines2raster = routes_poly[routes_poly$group == k ,]
  vx_sub <- vx
  vx_sub$rasterize(lines2raster, field="bicycle_16p", band=1, background = 0)
  vx_sub$write(path = paste0("../pct-lsoa/Data/03_Intermediate/raster/rf_nat_4plus_bearing_velox_",k,".tif"))
  print(paste0("Done group ",k," at ",Sys.time() ))
}

#Add rasters togther
stack <- raster("../pct-lsoa/Data/03_Intermediate/raster/rf_nat_4plus_bearing_velox_1.tif")
stack[is.na(stack[])] <- 0 


#Still creating lots of temp rasters
for(m in 2:180 ){  #max(groups[,2])
  sub <- raster(paste0("../pct-lsoa/Data/03_Intermediate/raster/rf_nat_4plus_bearing_velox_",m,".tif"))
  sub[is.na(sub[])] <- 0 
  stack <- overlay(stack, sub, fun=function(x,y){return(x+y)}, filename='over_tmp.grd', overwrite = TRUE )
  stack[is.na(stack[])] <- 0 
  print(paste0("added group ",as.character(m)," to the stack at ",as.character(Sys.time())))
}

writeRaster(stack,filename = "../pct-lsoa/Data/03_Intermediate/raster/rf_nat_4plus_velox_stackfin.tif", format ="GTiff")


#Using Raster stack method
library(raster)
list <- list.files(path='../pct-lsoa/Data/03_Intermediate/raster/bearing/', full.names=TRUE)
head(list)
rasterStack <- stack(list)
rastersum <- stackApply(rasterStack,1,fun =sum)


#rasterStack <- stack(system.file("external/rlogo.grd", package="raster")) 



#setwd("C:/rasters")
#rlist <- list.files("../pct-lsoa/Data/03_Intermediate/raster/bearing/", pattern="tif$", full.names=FALSE)
#for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 

#Earlier test

#routes <- readRDS("D:/routesforNikolia.Rds")
#routes <- routes[1:5,]
#routes <- routes[routes$group == 1,]
#routes <- spTransform(routes, CRS( "+init=epsg:27700" ) )
#routes_poly <- gBuffer(routes, byid = T, width = 5) 
#raster_master <- raster(resolution = 9.547, ext = extent(routes), crs= "+init=epsg:27700", vals = 0)
#vx <- velox(raster_master, extent=extent(routes), res=c(9.547,9.547), crs="+init=epsg:27700")
#vx$rasterize(routes_poly, field="bicycle_16p", band=1, background = 0)
#vx$write(path = "D:/testraster8.tif")
