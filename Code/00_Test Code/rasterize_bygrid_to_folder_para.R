
library(sp)
library(rgeos)
library(velox)
library(raster)
library(dplyr)
library(rgdal)
library(maptools)


#Inputs
routes_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_all.Rds")   #CHANGE ME
flow_data = read.csv("../pct-bigdata-lsoa/flow_results_nat_161220/flow_results_nat_161220.csv") #CHANGE ME

#Parameters
resolution = 10 #Pixle size in meters
tempfld <- "../pct-lsoa/Data/03_Intermediate/temp/rasterise-par/" #Where tempry rasters are saved
outfld <- "../pct-lsoa/Data/03_Intermediate/raster-grid-par/" #Where results are saved

#Make Out folder
if (!isTRUE(file.info(outfld)$isdir)) dir.create(outfld, recursive=TRUE)

#Prep and Join data
#flow_data <- flow_data[,c("id","bicycle_16p")]
head(flow_data)
flow_data$govtarget_slc <- as.integer(round(flow_data$govtarget_slc,0))
flow_data$gendereq_slc <- as.integer(round(flow_data$gendereq_slc,0))
flow_data$dutch_slc <- as.integer(round(flow_data$dutch_slc,0))
flow_data$ebike_slc <- as.integer(round(flow_data$ebike_slc,0))
head(flow_data)

routes_master@data <- left_join(routes_master@data, flow_data, by = c("ID" = "id"))

#Remove Unneeded data
routes_master@data = routes_master@data[,c("ID","govtarget_slc")]
remove(flow_data) #,groups_master)
routes_master = routes_master[routes_master$govtarget_slc >0,]
nrow(routes_master)
routes_master <- spTransform(routes_master, CRS( "+init=epsg:27700" ) )
points <- SpatialLinesMidPoints(routes_master)

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
remove("over","bb","cs","cc","cd","grd","points","sp_grd","grid_size")

tab <- as.data.frame(table(routes_master$grid))
print(paste0("There are ",nrow(tab), " grids to do"))


#Do grids Parallel
#library(doSNOW)
#library(foreach)
#cl<-makeCluster(1) #change the 2 to your number of CPU cores
#registerDoSNOW(cl)

#Loop though each grid cell and make a raster
#foreach(i = as.integer(as.character(tab$Var1))) %dopar% {
for(i in tab$Var1){
  
  print(paste0("Doing ",tab$Freq[tab$Var1==i]," lines in  grid number ", i," at ",Sys.time()))
  lines <- routes_master[routes_master$grid == i,]
  names(lines) <- c("id","bike","grid")
  polys <- gBuffer(lines, byid = T, width = 10)
  
  #Set Up Raster
  raster_master <- raster(resolution = resolution, ext = extent(polys), crs= "+init=epsg:27700", vals = 0)
  dataType(raster_master) <- "INT2U"
  vx <- velox(raster_master, extent=extent(polys), res=c(resolution,resolution), crs="+init=epsg:27700")
  remove(raster_master)
  vx_master <- vx
  #print(paste0("SetUp",Sys.time()))
  
  #loop though each line and rasterize
    todir <- paste0(tempfld,i,"/") #CHNAGE ME n.b. don't change the i variables
    if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
    for(k in 1:nrow(polys) ){  
    lines2raster = polys[k,]
    vx_sub <- vx
    vx_sub$rasterize(lines2raster, field="bike", band=1, background = 0)
    vx_sub$write(path = paste0(todir,"r",k,".tif"))
    }
  #print(paste0("Made ",Sys.time()))
    
  #loop back though created rasters and add them together
    stack <- raster(paste0(todir,"r",1,".tif"))
    if(nrow(polys) > 1) {
      for(j in 2:nrow(polys)){
        nxt <- raster(paste0(todir,"r",j,".tif"))
        stack <- stack + nxt
        }
    }
    #print(paste0("Added ",Sys.time()))
    writeRaster(stack,filename = paste0(outfld,"grd-",i,".tif"), format ="GTiff")
    removeTmpFiles(h = 1)
  
  #Remove all the indervidual rasters
  do.call(file.remove, list(list.files(todir, full.names = TRUE)))
  #print(paste0("Clean ",Sys.time()))
}
#stopCluster(cl)

