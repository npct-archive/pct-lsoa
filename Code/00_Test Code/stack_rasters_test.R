#Stack up and delete old files
library(raster)
library(velox)

print(paste0("Start at ",Sys.time()))


folder <- "C:/results/gender-l3 - Copy"
outfld <- folder
grd  <- 2232
list <- list.files(path = folder, pattern = paste0("grd-",grd,"-"), full.names = T)

#Make lsit of velox objects
list_vel <- vector("list",length(list))
for(n in 1:length(list)){
  list_vel[n] <- velox(list[n])
}

#Read in the fist raster
rall <- velox(list_vel)
rall <- rall$as.RasterStack()
rbrick <- brick(rall)
brickstack <- stackApply(rbrick, 1, sum)

print(paste0("Done at ",Sys.time()))

writeRaster(brickstack,filename = paste0(outfld,"/grd-",i,"-all.tif"), format ="GTiff")
file.remove(list)

