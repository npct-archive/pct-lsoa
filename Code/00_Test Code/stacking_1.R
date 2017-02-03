library('raster')
library('rgdal')


rasterOptions(tolerance = 0.5)

mosaicList <- function(rasList){
  
  #Internal function to make a list of raster objects from list of files.
  ListRasters <- function(list_names) {
    raster_list <- list() # initialise the list of rasters
    for (i in 1:(length(list_names))){ 
      grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
      raster_file <- raster::raster(grd_name)
      raster_file <- projectRaster(raster_file, snap, method = "ngb")
          }
    raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
  }
  
  #convert every raster path to a raster object and create list of the results
  raster.list <-sapply(rasList, FUN = ListRasters)
  
  # edit settings of the raster list for use in do.call and mosaic
  names(raster.list) <- NULL
  #####This function deals with overlapping areas
  raster.list$fun <- sum
  #raster.list$tolerance <- 0.1
  
  #run do call to implement mosaic over the list of raster objects.
  mos <- do.call(raster::mosaic, raster.list)
  
  #set crs of output
  crs(mos) <- crs(x = raster(rasList[1]))
  return(mos)
}

raster_files <- list.files(path ="D:/results/gov-4p/stackoverflow",pattern = ".tif$",full.names = TRUE )
snap <- raster(resolution = c(10,10), xmn = 180000, xmx = 300000, ymn = 60000, ymx = 100000, crs = "+init=epsg:27700") 
national_layer <- mosaicList(raster_files )



writeRaster(national_layer,"D:/results/gov-4p/stackoverflow/merge4.tif", format = "GTiff")



#r2_rsp <- resample(r2,r1,"ngb")