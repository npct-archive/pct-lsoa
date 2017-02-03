library('raster')
library('rgdal')

master_list <- list.files(path ="D:/results/gov-4p/stackoverflow2",pattern = ".tif$",full.names = TRUE )
raster_files <- master_list[1:10]

#####Internal function to make a list of raster objects from list of files.
ListRasters <- function(list_names) {
    raster_list <- list() # initialise the list of rasters
    for (i in 1:(length(list_names))){ 
      grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
      raster_file <- raster::raster(grd_name)
            }
    raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
  }
######

#####Internal function to make a list of raster objects from list of files.
ListRastersSnap <- function(list_names) {
  raster_list <- list() # initialise the list of rasters
  for (i in 1:(length(list_names))){ 
    grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
    raster_file <- raster::raster(grd_name)
    raster_file <- projectRaster(raster_file, snap, method = "ngb")
  }
  raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
}
######


#convert every raster path to a raster object and create list of the results
raster.list <-sapply(raster_files, FUN = ListRasters)

#extent.list <-sapply(raster.list, FUN = GetExtent)

extent_list <- list()
for (j in 1:length(raster.list)){
  ext <- extent(raster.list[[j]])
  extent_list <- append(extent_list,ext)
}

Uext <- extent_list[[1]]
for (k in 2:length(raster.list)){
  Uext <- union(Uext,extent_list[[k]])
}

snap <- raster(resolution = c(10,10), ext = Uext, crs = "+init=epsg:27700")
raster.list <-sapply(raster_files, FUN = ListRastersSnap)
  
# edit settings of the raster list for use in do.call and mosaic
names(raster.list) <- NULL

#####This function deals with overlapping areas
raster.list$fun <- sum

#run do call to implement mosaic over the list of raster objects.
mos <- do.call(raster::mosaic, raster.list)
  
#set crs of output
#crs(mos) <- crs(x = raster(raster_files[1]))
crs(mos) <-"+init=epsg:27700"

#raster_files <- list.files(path ="D:/results/gov-4p/stackoverflow",pattern = ".tif$",full.names = TRUE )
#snap <- raster(resolution = c(10,10), xmn = 180000, xmx = 300000, ymn = 60000, ymx = 100000, crs = "+init=epsg:27700") 

national_layer <- mosaicList(raster_files )

writeRaster(national_layer,"D:/results/gov-4p/stackoverflow/merge6.tif", format = "GTiff")
remove(national_layer,snap,raster_files,raster.list,ext,extent_list,master_list,raster_files)
gc()

#r2_rsp <- resample(r2,r1,"ngb")

 