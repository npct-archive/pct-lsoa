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

group_list = as.data.frame(table(routes_master$group))
group_list$Var1 = as.integer(group_list$Var1)
#group_list = group_list[group_list$Freq > 2,]

hist(group_list$Freq)

#Set up the raster
Xres <- as.integer(geosphere::distHaversine(c(routes_master@bbox[1,1],routes_master@bbox[2,1]), c(routes_master@bbox[1,2],routes_master@bbox[2,1]))/resolution)
Yres <- as.integer(geosphere::distHaversine(c(routes_master@bbox[1,1],routes_master@bbox[2,1]), c(routes_master@bbox[1,1],routes_master@bbox[2,2]))/resolution)
raster_master <- raster(ncols=Xres, nrows=Yres, ext = extent(routes_master), crs= "+init=epsg:4267", vals = 0)
raster_stack<- raster_master

###### Needs to cleanr up files ater itself


#loop though each group and rasterize
#then add the raster onto the last raster
for(k in group_list$Var1 ){  #max(groups[,2])
  #checklist = groups[which(groups[,2] == k),]
  lines2raster = routes_master[routes_master$group == k ,]
  lines2raster <- spTransform(lines2raster,CRS("+init=epsg:4267"))
  raster_sub <- rasterize(lines2raster,raster_master , field ="bicycle_16p", filename ="../pct-lsoa/Data/03_Intermediate/raster/raster_sub_temp.grd", overwrite = TRUE )
  raster_sub[is.na(raster_sub[])] <- 0 
  raster_stack <- overlay(raster_stack, raster_sub, fun=function(x,y){return(x+y)}, filename ="../pct-lsoa/Data/03_Intermediate/raster/raster_overlay_temp.grd", overwrite = TRUE )
  raster_stack[is.na(raster_stack[])] <- 0
  print(paste0("added group ",as.character(k)," to the stack at ",as.character(Sys.time())))
}

#Replace the 0 with NA 
#raster_fin <- raster_stack
raster_stack[which(raster_stack[] == 0)] <- NA


#Need to convert to 8bit

#Save results
writeRaster(raster_stack,filename = "../pct-lsoa/Data/03_Intermediate/raster/rf_nat_4plus_bearing.tif", format ="GTiff")

plot(raster_fin)
writeRaster(raster_master,filename = "../pct-lsoa/Data/03_Intermediate/raster/england_raster_master.tif", format ="GTiff")
#write.csv(matrix_master, file = "../pct-lsoa-test/data/matrix.csv")




















#Set up the king raster
raster_king <- raster(resolution = c(resolution,resolution), ext = extent(routes_master), crs= "+proj=longlat +init=epsg:3857", vals = 0)
dataType(raster_king) <- "INT2U"
raster_king[is.na(raster_king[])] <- 0

loop = group_list$Var1

#loop though each group and rasterize
#then add the raster onto the last raster

pb <- winProgressBar(title="Raster progress bar", min=0, max=max(group_list$Var1), initial=0, label="0 rasters done")
progress = 0
for(k in loop){
  lines2raster = routes_master[routes_master@data$group == k,]
  lines2raster = routes_master[1:10,]
  raster_sub = crop(raster_king,extent(lines2raster))
  #lines2raster <- spTransform(lines2raster,CRS("+init=epsg:4267"))
  raster_sub <- rasterize(lines2raster,raster_sub , field ="bicycle_16p", filename= "../pct-lsoa/Data/03_Intermediate/temp/singletemp.grd", overwrite = TRUE)
  raster_sub2 <- rasterize(lines2raster,raster_sub , field ="bicycle_16p", filename= "../pct-lsoa/Data/03_Intermediate/temp/singletemp.grd", overwrite = TRUE, background = 0)
  raster_sub2[is.na(raster_sub[])] <- 0 
  raster_king <- overlay(raster_king, raster_sub, fun=function(x,y){return(x+y)}, filename = "../pct-lsoa/Data/03_Intermediate/temp/overlaytemp.grd", overwrite = TRUE)
  
  progress = progress + 1
  info <- sprintf("%d rasters done", progress)
  setWinProgressBar(pb, progress, label = info)
  #print(paste0("added group ",as.character(k)," to the stack at ",as.character(Sys.time())))
}
close(pb)

writeRaster(raster_sub2,"D:/testrasr2.tif")
