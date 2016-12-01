#Convert routes to a raster based on the number of cyclists
#WHen rasterizing it is necessary to not have overlapping lines

#4) Loop though each group convert to raster then add up rasters

#libs
library(rgeos)
library(raster)
library(sp) 
library(rgdal)
library(dplyr)
library(utils)
#library(gdalUtils)
source("../pct-lsoa/Code/03_Visualise/gdal_rasterize_mod.R")


#gdal_setInstallation(search_path = "D:/Program Files/QGIS 2.18/bin", verbose = T)

#Inputs
lines_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_test.Rds")
lines_master@data = subset(lines_master@data, select=c("ID"))
#lines_master <- lines_master[!duplicated(lines_master$id),] #remove when rf_nat fixed
lines_data = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")
lines_data = subset(lines_data, select=c("id","bicycle_16p"))

names(lines_data) = c("id","val")
lines_data = lines_data[lines_data$val > 0 ,]
#lines_data <- lines_data[!duplicated(lines_data$id),] #remove when rf_lines fixed
groups <- readRDS("../pct-lsoa/Data/03_Intermediate/groups/rf_nat_test.Rds")

#Test subsetting
#################################
lines_master = lines_master[lines_master@data$ID %in% groups$id_old,]
lines_data = lines_data[lines_data$id %in% lines_master$ID,]
lines_master = lines_master[lines_master@data$ID %in% lines_data$id,]
groups = groups[groups$id_old %in% lines_master$ID,]
###############################


size_limit = 5000
resolution = 19.093 #Equal to OSM zoom level 13

#join in the cycling data
lines_master@data = merge(lines_master@data,lines_data, by.x = "ID", by.y = "id")
lines_master@data = merge(lines_master@data,groups, by.x = "ID", by.y = "id_old")
#merge <- right_join(lines_master@data, lines_data@data, by = "id")
remove(lines_data)
#lines_master <- lines_master[1:100000,] #for low ram computers


#Set up the king raster
#Xres <- as.integer(geosphere::distHaversine(c(lines_master@bbox[1,1],lines_master@bbox[2,1]), c(lines_master@bbox[1,2],lines_master@bbox[2,1]))/resolution)
#Yres <- as.integer(geosphere::distHaversine(c(lines_master@bbox[1,1],lines_master@bbox[2,1]), c(lines_master@bbox[1,1],lines_master@bbox[2,2]))/resolution)
#raster_king <- raster(ncols=Xres, nrows=Yres, ext = extent(lines_master), crs= "+proj=longlat +init=epsg:3857", vals = 0)
#raster_king <- raster(resolution = 20, ext = extent(lines_master), crs= "+proj=longlat +init=epsg:3857", vals = 0)
#dataType(raster_king) <- "INT4S"
#writeRaster(raster_king,paste0("../pct-lsoa/Data/03_Intermediate/temp/","test_group_raster_king"), format = "GTiff", datatype = "INT2U", overwrite = T)

groups_list = sort(unique(groups$nrow))

for(i in groups_list){
  lines = lines_master[lines_master@data$nrow == i,]
  #raster <- crop(raster_king,extent(lines))
  #writeRaster(raster,paste0("../pct-lsoa/Data/03_Intermediate/temp/","test_group_raster",i), format = "GTiff", overwrite = T)
  writeOGR(lines, dsn = "../pct-lsoa/Data/03_Intermediate/temp", layer = paste0("test_group_",i), driver = "ESRI Shapefile", overwrite_layer = T)
  src <- paste0("../pct-lsoa/Data/03_Intermediate/temp/","test_group_",i,".shp")
  dst <- paste0("../pct-lsoa/Data/03_Intermediate/temp/","test_raster",i,".tif")
  Xres1 <- as.integer(geosphere::distHaversine(c(lines@bbox[1,1],lines@bbox[2,1]), c(lines@bbox[1,2],lines@bbox[2,1]))/resolution)
  Yres1 <- as.integer(geosphere::distHaversine(c(lines@bbox[1,1],lines@bbox[2,1]), c(lines@bbox[1,1],lines@bbox[2,2]))/resolution)
  gdal_rasterize_malcolm(src,dst, ts = c(Xres1,Yres1),a = "val") #works with creating single raster
  file.remove(paste0("../pct-lsoa/Data/03_Intermediate/temp/","test_group_",i,".shp"))
  file.remove(paste0("../pct-lsoa/Data/03_Intermediate/temp/","test_group_",i,".shx"))
  file.remove(paste0("../pct-lsoa/Data/03_Intermediate/temp/","test_group_",i,".dbf"))
  file.remove(paste0("../pct-lsoa/Data/03_Intermediate/temp/","test_group_",i,".prj"))
}

#Set up the king raster
raster_king <- raster(resolution = resolution, ext = extent(lines_master), crs= "+proj=longlat +init=epsg:3857", vals = 0)
dataType(raster_king) <- "INT2U"
raster_stack = raster(paste0("../pct-lsoa/Data/03_Intermediate/temp/test_raster",groups_list[1],".tif"))
raster_stack <- extend(raster_stack,extent(raster_king), value = 0) #this is painfully slow or broken

for(j in groups_list[2:length(groups_list)]){
  raster_next = raster(paste0("../pct-lsoa/Data/03_Intermediate/temp/test_raster",j,".tif"))
  plot(raster_next)
  extent(raster_next)
  raster_next <- extend(raster_next,extent(raster_king), value = 0)
  extent(raster_next)
  raster_stack <- overlay(raster_stack, raster_next, fun = function(x,y){return(x+y)})
}

writeRaster(raster_stack,paste0("../pct-lsoa/Data/03_Intermediate/temp/test_raster_all.tif", format = "GTiff", datatype = "INT2U"))
























#Set up the raster
raster_master <- crop(raster_king,extent(lines))
raster_stack <- raster_master

#loop though each group and rasterize
#then add the raster onto the last raster
print(paste0("Grouping complete for loop ",v," starting raster process for ",as.character(max(groups[,2]))," groups at ",Sys.time()))
pb <- winProgressBar(title="Raster progress bar", min=0, max=max(groups[,2]), initial=0, label="0 rasters done")
progress = 0
for(k in 1:max(groups[,2]) ){  #max(groups[,2])
  checklist = groups[which(groups[,2] == k),]
  lines2raster = lines[which(lines$id %in% checklist[,1]),]
  lines2raster <- spTransform(lines2raster,CRS("+init=epsg:4267"))
  raster_sub <- rasterize(lines2raster,raster_master , field ="bicycle", filename= "../pct-lsoa/Data/03_Intermediate/temp/singletemp.grd", overwrite = TRUE)
  raster_sub[is.na(raster_sub[])] <- 0 
  raster_stack <- overlay(raster_stack, raster_sub, fun=function(x,y){return(x+y)}, filename = "../pct-lsoa/Data/03_Intermediate/temp/overlaytemp.grd", overwrite = TRUE)
  raster_stack[is.na(raster_stack[])] <- 0
  progress = progress + 1
  info <- sprintf("%d rasters done", progress)
  setWinProgressBar(pb, progress, label = info)
  #print(paste0("added group ",as.character(k)," to the stack at ",as.character(Sys.time())))
}
close(pb)

#Replace the 0 with NA 
raster_stack[which(raster_stack[] == 0)] <- NA

#Need to convert to 8bit

#Save results
writeRaster(raster_stack,filename = paste0("../pct-lsoa/Data/03_Intermediate/temp/Raster",v,".tif"), format ="GTiff")
print(paste0("Rasetering complete for loop ",v," at ",Sys.time()))

file.remove("../pct-lsoa/Data/03_Intermediate/temp/singletemp.grd")
file.remove("../pct-lsoa/Data/03_Intermediate/temp/overlaytemp.grd")

#Clean up some variaibles
remove(matrix)
remove(groups)
remove(matrix_master)
remove(lines)
remove(rowsum)
}

