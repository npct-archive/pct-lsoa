#Takes a folver full of rasters and adds them together



#Add rasters togther
stack <- raster("../pct-lsoa/Data/03_Intermediate/temp/rasters/indiv/rf-nat-4p-1.tif")
stack[is.na(stack[])] <- 0 


#Still creating lots of temp rasters
for(m in 2:20000 ){  #max(groups[,2])
  sub <- raster(paste0("../pct-lsoa/Data/03_Intermediate/temp/rasters/indiv/rf-nat-4p-",m,".tif"))
  sub[is.na(sub[])] <- 0 
  stack <- overlay(stack, sub, fun=function(x,y){return(x+y)}, filename='over_tmp.grd', overwrite = TRUE )
  stack[is.na(stack[])] <- 0 
  print(paste0("added group ",as.character(m)," to the stack at ",as.character(Sys.time())))
}

writeRaster(stack,filename = "../pct-lsoa/Data/03_Intermediate/raster/rf_nat_4plus_velox_stackfin.tif", format ="GTiff")


#Using Raster stack method
library(raster)
list <- list.files(path='../pct-lsoa/Data/03_Intermediate/temp/rasters/indiv/', full.names=TRUE)
head(list)
sublist <- list[1:100]
rasterStack <- stack(sublist)
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
