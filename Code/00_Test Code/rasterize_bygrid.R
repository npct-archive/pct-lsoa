
library(sp)
library(rgeos)
library(velox)
library(raster)
library(dplyr)
library(rgdal)
library(utils)
library(maptools)


#Inputs
routes_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus_fix.Rds")
flow_data = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")

#Parameters
resolution = 10 #Equal to OSM zoom level 13

#Prep and Join data
flow_data <- flow_data[,c("id","bicycle_16p")]
routes_master@data <- left_join(routes_master@data, flow_data, by = c("ID" = "id"))

#Remove Unneeded data
routes_master@data = routes_master@data[,c("ID","bicycle_16p")]
remove(flow_data) #,groups_master)
routes_master = routes_master[routes_master$bicycle_16p >0,]

routes_master <- spTransform(routes_master, CRS( "+init=epsg:27700" ) )
points <- SpatialLinesMidPoints(routes_master)



#remove(routes_master)


### define SpatialGrid object
grid_size = 10000
bb <- bbox(points)
cs <- c(grid_size, grid_size)
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
sp_grd <- SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)), proj4string=CRS(proj4string(points)))

#Assign Grid IDs to Lines
routes_master@data$grid <- as.integer(NA)
over <- over(points, sp_grd)
routes_master@data$grid <- over$id
#groups = groups_master
#groups_master = groups_master[with(groups_master,order(-count)),]
#over_summary = data.frame(table(groups_master$grid))
remove("over","bb","cs","cc","cd","grd")

#plot(routes_poly[routes_poly$grid == 2484,])

#test <- routes_poly[routes_poly$grid == 2484,]
#nrow(test)

tab <- as.data.frame(table(routes_master$grid))

#i = 2336
#source("../pct-lsoa/Code/03_Visualise/grouping_functions.R")
#Loop thougjh each grid cell and make a raster
for(i in tab$Var1){
  print(paste0("Doing Grid number ", i," at ",Sys.time()))
  lines <- routes_master[routes_master$grid == i,]
  names(lines) <- c("id","bike","grid")
  #groups <- grouplines_complete(lines,1,10000)
  #groups <- groups[,2:3]
  #lines@data <- left_join(lines@data, groups, by = c("id" = "id_old"))
  polys <- gBuffer(lines, byid = T, width = 10)
  #group_list = as.data.frame(table(polys$nrow))
  #group_list$Var1 = as.integer(group_list$Var1)
  
  #Set Up Raster
  raster_master <- raster(resolution = resolution, ext = extent(polys), crs= "+init=epsg:27700", vals = 0)
  dataType(raster_master) <- "INT2U"
  vx <- velox(raster_master, extent=extent(polys), res=c(resolution,resolution), crs="+init=epsg:27700")
  remove(raster_master)
  vx_master <- vx
  
  #loop though each group and rasterize
  #print(paste0("There are ",nrow(group_list)," groups to do, getting started at ",Sys.time() ))
  for(k in 1:nrow(polys) ){  #max(groups[,2])
    lines2raster = polys[k,]
    vx_sub <- vx
    vx_sub$rasterize(lines2raster, field="bike", band=1, background = 0)
    vx_sub$write(path = paste0("C:/Rasters/rf-nat-4p-grid-",i,"-line-",k,".tif"))
    #print(paste0("Done line ",k," at ",Sys.time() ))
  }
  print(paste0("Finished making indervidual rasters now stacking them at ",Sys.time()))
  
  list <- list.files(path='C:/Rasters/', full.names=TRUE)
  #head(list)
  #sublist <- list[1:10]
  rasterOptions(tmpdir='C:/RasterTemp')
  rasterStack <- stack(list, quick = T)
  rastersum <- stackApply(rasterStack,1,fun =sum)
  writeRaster(rastersum,filename = paste0("../pct-lsoa/Data/04_Output/rastergrid/rf-nat-4p-grid-",i,".tif"), format ="GTiff")
  do.call(file.remove, list(list.files("C:/Rasters", full.names = TRUE)))
  do.call(file.remove, list(list.files("C:/RasterTemp", full.names = TRUE)))
  
  
}


