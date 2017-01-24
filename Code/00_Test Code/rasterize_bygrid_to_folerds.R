
library(sp)
library(rgeos)
library(velox)
library(raster)
library(dplyr)
library(rgdal)
library(maptools)


#Inputs
routes_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_less3p_fix.Rds")   #CHANGE ME
flow_data = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds") #CHANGE ME

#Parameters
resolution = 10 #Pixle size in meters

#Prep and Join data
flow_data <- flow_data[,c("id","bicycle_16p")]
routes_master@data <- left_join(routes_master@data, flow_data, by = c("ID" = "id"))

#Remove Unneeded data
routes_master@data = routes_master@data[,c("ID","bicycle_16p")]
remove(flow_data) #,groups_master)
routes_master = routes_master[routes_master$bicycle_16p >0,]

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
remove("over","bb","cs","cc","cd","grd")

tab <- as.data.frame(table(routes_master$grid))
print(paste0("There are ",nrow(tab), " grids to do"))

#Loop thougjh each grid cell and make a raster
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
  
  #loop though each line and rasterize
    for(k in 1:nrow(polys) ){  
    lines2raster = polys[k,]
    vx_sub <- vx
    vx_sub$rasterize(lines2raster, field="bike", band=1, background = 0)
    to <- paste0("D:/Git/pct-lsoa/Data/03_Intermediate/temp/rasters/indiv-grid-3l/",i,"/rf-nat-l3p-",k,".tif") #CHNAGE ME n.b. don't change the i and k variables
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
    vx_sub$write(path = to)
    }

}


