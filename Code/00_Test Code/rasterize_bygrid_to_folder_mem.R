
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
#tempfld <- "../pct-lsoa/Data/03_Intermediate/temp/rasterise-par/" #Where tempry rasters are saved
outfld <- "C:/results/census3l/" #Where results are saved

#Make Out folder
if (!isTRUE(file.info(outfld)$isdir)) dir.create(outfld, recursive=TRUE)

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
remove("over","bb","cs","cc","cd","grd","points","sp_grd","grid_size")

tab <- as.data.frame(table(routes_master$grid))
names(tab) <- c("grid","count")
tab$grid <- as.integer(as.character(tab$grid))
tab$count <- as.integer(tab$count)
tab <- tab[order(tab$count),]
write.csv(tab,paste0(outfld,"run_order.csv"))
print(paste0("There are ",nrow(tab), " grids to do"))

for(i in tab$grid){
  
  print(paste0("Doing ",tab$count[tab$grid==i]," lines in  grid number ", i," at ",Sys.time()))
  lines <- routes_master[routes_master$grid == i,]
  names(lines) <- c("id","bike","grid")
  polys <- gBuffer(lines, byid = T, width = 10)
  remove(lines)
  
  #Set Up Raster
  raster_master <- raster(resolution = resolution, ext = extent(polys), crs= "+init=epsg:27700", vals = 0)
  dataType(raster_master) <- "INT2U"
  vx <- velox(raster_master, extent=extent(polys), res=c(resolution,resolution), crs="+init=epsg:27700")
  remove(raster_master)
  #print(paste0("SetUp",Sys.time()))
  
    #Make and empty raster stack
    lx <- list(vx)
    for (j in 1:nrow(polys)) lx[[j]] <- vx
    vx_sub2 <- velox(lx, extent=extent(polys), res=c(resolution,resolution), crs="+init=epsg:27700")
    remove(lx, vx)
    #print(paste0("Make ",Sys.time()))
    
    #loop though each line and rasterize
    for(k in 1:nrow(polys) ){  
      lines2raster = polys[k,]
      vx_sub2$rasterize(lines2raster, field="bike", band= k, background = 0)
    }
    remove(lines2raster, polys)
    #print(paste0("Rastered ",Sys.time()))
    
    #Add rasters togther
    rs <- vx_sub2$as.RasterStack()
    remove(vx_sub2)
    rsum <- stackApply(rs, 1, sum)
    #print(paste0("Added ",Sys.time()))
    
    writeRaster(rsum,filename = paste0(outfld,"grd-",i,".tif"), format ="GTiff")
    #print(paste0("Written ",Sys.time()))
    
    removeTmpFiles(h = 1)
    remove(rs,rsum)
    #print(paste0("Cleaned ",Sys.time()))
}


