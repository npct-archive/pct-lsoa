#Stack up and delete old files
library(raster)
library(velox)

print(paste0("Start at ",Sys.time()))


folder <- "D:/results/gov-4p/cluster-7/parts"
grids <- read.csv("D:/results/gender-4p-redo/from robin/ones/numbs.csv")
names(grids) <- "numb"
outfld <- "D:/results/gov-4p/cluster-7"

for(i in 1:nrow(grids)){
  
  grd  <- grids$numb[i]
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
  
  print(paste0("Done ",grd, " at ",Sys.time()))
  
  writeRaster(brickstack,filename = paste0(outfld,"/grd-",grd,"-all.tif"), format ="GTiff")
  file.remove(list)
  
}



