library(sp)
library(rgeos)
library(velox)
library(raster)
library(dplyr)
library(rgdal)
library(maptools)

#Inputs
infld <- "D:/Git/pct-lsoa/Data/03_Intermediate/routes/par_batch/ebike-4p/alvaro_unfinished/"
inname <- "ebike-alvaro-unfin-" #Which scenario do you want to do gendereq_slc_r- govtarget_slc_r-  dutch_slc_r- ebike_slc_r-
outfld <- "D:/results/test/"
clusterNo <- 1

#Parameters
resolution = 10 #Pixle size in meters
limit = 200 #Maximum number of lines to be done at once
options(nwarnings = 10000)
options("warn" = 1)
rasterOptions(maxmemory = 1e+10) #10+8 is default
rasterOptions(datatype = "INT2U")

#Get input file
routes_master = readRDS(paste0(infld,inname,clusterNo,".Rds"))   

#Make Out folder
if (!isTRUE(file.info(paste0(outfld,"cluster-",clusterNo,"/parts"))$isdir)) dir.create(paste0(outfld,"cluster-",clusterNo,"/parts"), recursive=TRUE)

tab <- as.data.frame(table(routes_master$grid))
names(tab) <- c("grid","count")
tab$grid <- as.integer(as.character(tab$grid))
tab$count <- as.integer(tab$count)
tab <- tab[order(tab$count),]
write.csv(tab,paste0(outfld,"cluster-",clusterNo,"/run_order-",clusterNo,".csv"))
print(paste0("There are ",nrow(tab), " grids to do"))

for(i in tab$grid){
  #start <- Sys.time()
  print(paste0("Doing grid ",i," with ",tab$count[tab$grid==i]," lines, at ",Sys.time()))
  lines <- routes_master[routes_master$grid == i,]
  names(lines) <- c("id","bike","grid")
  polys <- gBuffer(lines, byid = T, width = 10)
  remove(lines)
  
  #Set Up Raster
  raster_master <- raster(resolution = resolution, ext = extent(polys), crs= "+init=epsg:27700", vals = 0)
  dataType(raster_master) <- "INT2U"
  vx <- velox(raster_master, extent=extent(polys), res=c(resolution,resolution), crs="+init=epsg:27700")
  remove(raster_master)
  
  if(nrow(polys) <= limit){
    #Make and empty raster stack
    lx <- list(vx)
    for (j in 1:nrow(polys)) lx[[j]] <- vx
    vx_sub2 <- velox(lx, extent=extent(polys), res=c(resolution,resolution), crs="+init=epsg:27700")
    remove(lx, vx)
    
    #loop though each line and rasterize
    for(k in 1:nrow(polys) ){  
      lines2raster = polys[k,]
      vx_sub2$rasterize(lines2raster, field="bike", band= k, background = 0)
    }
    remove(lines2raster, polys)
    
    #Add rasters togther
    rs <- vx_sub2$as.RasterStack()
    remove(vx_sub2)
    rsum <- stackApply(rs, 1, sum)
    
    writeRaster(rsum,filename = paste0(outfld,"cluster-",clusterNo,"/grd-",i,".tif"), format ="GTiff")
    

    removeTmpFiles(h = 1)
    remove(rs,rsum)

  } else {
    
    print("Too large breaking into chunks")
    for(l in 1:ceiling(nrow(polys)/limit)){
      print(paste0("Doing part ",l," of ",ceiling(nrow(polys)/limit)," at ",Sys.time()))
      lstart <- 1 + limit * (l-1)
      lfin <- if(limit * l > nrow(polys)){nrow(polys)}else{limit*l}
      lx <- list(vx)
      for (j in 1:limit) lx[[j]] <- vx
      vx_sub2 <- velox(lx, extent=extent(polys), res=c(resolution,resolution), crs="+init=epsg:27700")
      remove(lx)
      
      #loop though each line and rasterize
      #print(paste0("Rasterize at ",Sys.time()))
      for(k in lstart:lfin ){  
        lines2raster = polys[k,]
        vx_sub2$rasterize(lines2raster, field="bike", band= (k - lstart + 1), background = 0)
      }
      remove(lines2raster)
      
      #Add rasters togther
      #print(paste0("Add togther at ",Sys.time()))
      rs <- vx_sub2$as.RasterStack()
      remove(vx_sub2)
      rsum <- stackApply(rs, 1, sum)
      
      writeRaster(rsum,filename = paste0(outfld,"cluster-",clusterNo,"/parts/grd-",i,"-",l,".tif"), format ="GTiff")
      
      
      remove(rs,rsum)
      
    }
    print(paste0("Adding parts together at ",Sys.time()))
    
    #Add all the parts togther
    list <- list.files(path = paste0(outfld,"cluster-",clusterNo,"/parts"), pattern = paste0("grd-",i,"-"), full.names = T)
    
    #Make lsit of velox objects
    list_vel <- vector("list",length(list))
    for(n in 1:length(list)){
      list_vel[n] <- velox(list[n], crs="+init=epsg:27700")
    }
    
    rall <- velox(list_vel, crs="+init=epsg:27700")
    rall <- rall$as.RasterStack()
    rbrick <- brick(rall)
    brickstack <- stackApply(rbrick, 1, sum)
    writeRaster(brickstack,filename = paste0(outfld,"cluster-",clusterNo,"/grd-",i,"-all.tif"), format ="GTiff")
    file.remove(list)
    remove(list,list_vel,rall,rbrick,brickstack)
    removeTmpFiles(h = 1)
  }
  
}
print(paste0("Finished ",inname," cluster no ",clusterNo," at ",Sys.time()))


