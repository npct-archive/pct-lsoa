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

#Inputs
lines_master = readRDS("../pct-lsoa/Data/03_Intermediate/rq_Cam.Rds")
lines_master@data = subset(lines_master@data, select=c("id"))
lines_master <- lines_master[!duplicated(lines_master$id),] #remove when rf_nat fixed
lines_data = readRDS("../pct-lsoa/Data/03_Intermediate/l_Cam.Rds")
lines_data@data = subset(lines_data@data, select=c("id","bicycle"))
lines_data <- lines_data[!duplicated(lines_data$id),] #remove when rf_lines fixed
groups <- readRDS("...")

size_limit = 5000

#join in the cycling data
lines_master@data = merge(lines_master@data,lines_data@data, by = "id")
#merge <- right_join(lines_master@data, lines_data@data, by = "id")
remove(lines_data)
#lines_master <- lines_master[1:100000,] #for low ram computers


#Set up the king raster
Xres <- as.integer(geosphere::distHaversine(c(lines_master@bbox[1,1],lines_master@bbox[2,1]), c(lines_master@bbox[1,2],lines_master@bbox[2,1]))/20)
Yres <- as.integer(geosphere::distHaversine(c(lines_master@bbox[1,1],lines_master@bbox[2,1]), c(lines_master@bbox[1,1],lines_master@bbox[2,2]))/20)
raster_king <- raster(ncols=Xres, nrows=Yres, ext = extent(lines_master), crs= "+proj=longlat +init=epsg:3857", vals = 0)

#Set up the raster
raster_master <- crop(raster_king,extent(lines))
raster_stack<- raster_master

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

