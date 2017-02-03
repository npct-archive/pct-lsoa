library('raster')
library('rgdal')

infld <- "D:/results/ebikes-4p/2001-2500"
outfld <- "D:/results/ebikes-4p/Rstack/2001-2500"

rasterOptions(maxmemory = 1e+09)
master_list <- list.files(path =infld,pattern = ".tif$",full.names = TRUE )
batch_size = 40
nbatch <- ceiling(length(master_list)/batch_size)
if (!isTRUE(file.info(paste0(outfld))$isdir)) dir.create(paste0(outfld), recursive=TRUE)

for(m in 1:nbatch){
  print(paste0("Doing batch ",m," of ",nbatch," at ",Sys.time()))
  lstart <- 1 + batch_size * (m-1)
  lfin <- if(batch_size * m > length(master_list)){length(master_list)}else{batch_size*m}
  raster_files <- master_list[lstart:lfin]
  
  #Get the maximum extent of all the rasters
  #Make a list of rasters
  raster.list <- list()
  for (i in 1:(length(raster_files))){ 
    raster_file <- raster::raster(raster_files[i])
    raster.list <- append(raster.list, raster_file)
  }
  remove(raster_files)
  
  #Make a list of extents
  extent_list <- list()
  for (j in 1:length(raster.list)){
    ext <- extent(raster.list[[j]])
    extent_list <- append(extent_list,ext)
  }
  remove(raster_file)
  
  #Union Extents to get whole extent
  Uext <- extent_list[[1]]
  for (k in 2:length(raster.list)){
    Uext <- union(Uext,extent_list[[k]])
  }
  remove(extent_list,ext)
  
  #Make Snap Raster
  snap <- raster(resolution = c(10,10), ext = Uext, crs = "+init=epsg:27700")
  
  #Resample Rasters to snap
  for (l in 1:(length(raster.list))){ 
    raster.list[[l]] <- projectRaster(raster.list[[l]], snap, method = "ngb")
    print(paste0("Done ",l," at ",Sys.time()))
  }
  remove(snap, Uext)
  
  # edit settings of the raster list for use in do.call and mosaic
  names(raster.list) <- NULL
  
  #####This function deals with overlapping areas
  raster.list$fun <- sum
  
  #run do call to implement mosaic over the list of raster objects.
  mos <- do.call(raster::mosaic, raster.list)
  
  #set crs of output
  crs(mos) <-"+init=epsg:27700"
  
  writeRaster(mos,paste0(outfld,"/merge-",m,".tif"), format = "GTiff")
  remove(raster.list, mos)
  gc()
  
}
 