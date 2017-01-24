library(sp)
library(rgeos)
library(velox)
library(raster)
library(dplyr)
library(rgdal)
library(maptools)

#Inputs
infld <- "../pct-lsoa/Data/03_Intermediate/routes/par_batch/gov-4p/"
inname <- "govtarget_slc_r-" #Which scenario do you want to do gendereq_slc_r- govtarget_slc_r-  dutch_slc_r- ebike_slc_r-
outfld <- "C:/results/gov-4p/"
clusterNo <- 1

#Parameters
resolution = 10 #Pixle size in meters
limit = 200 #Maximum number of lines to be done at once

#Get input file
routes_master = readRDS(paste0(infld,inname,clusterNo,".Rds"))   

#Make Out folder
if (!isTRUE(file.info(outfld)$isdir)) dir.create(outfld, recursive=TRUE)

tab <- as.data.frame(table(routes_master$grid))
names(tab) <- c("grid","count")
tab$grid <- as.integer(as.character(tab$grid))
tab$count <- as.integer(tab$count)
tab <- tab[order(tab$count),]
write.csv(tab,paste0(outfld,"run_order.csv"))
print(paste0("There are ",nrow(tab), " grids to do"))
# Special subsetting for restarting after running out of memory
#tab <- tab[tab$count > XXXXX,]
#routes_master <- routes_master[routes_master$grid %in% tab$grid,]

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
  #print(paste0("SetUp",Sys.time()))
  
  if(nrow(polys) <= limit){
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
    #end <- Sys.time()
    #nlines <- tab$count[tab$grid==i]
    #print(paste0("Done grid ",i," with ",nlines," lines, at ",(end-start)/(nlines)," s/line, at ",Sys.time()))
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
      #print(paste0("Make ",Sys.time()))
      #loop though each line and rasterize
      for(k in lstart:lfin ){  
        lines2raster = polys[k,]
        vx_sub2$rasterize(lines2raster, field="bike", band= (k - lstart + 1), background = 0)
      }
      remove(lines2raster)
      #print(paste0("Rastered ",Sys.time()))
      #Add rasters togther
      rs <- vx_sub2$as.RasterStack()
      remove(vx_sub2)
      rsum <- stackApply(rs, 1, sum)
      #print(paste0("Added ",Sys.time()))
      writeRaster(rsum,filename = paste0(outfld,"grd-",i,"-",l,".tif"), format ="GTiff")
      #print(paste0("Written ",Sys.time()))
      removeTmpFiles(h = 1)
      remove(rs,rsum)
      
    }
  }
  
}


