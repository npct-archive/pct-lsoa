#Add non overlapping rasters
library(raster)
library(rgdal)

limit <- readOGR("../pct-lsoa/Data/01_Raw/regions","rasterzones")
limit <- limit[limit$Id ==3,]
limit <- spTransform(limit, CRS( "+init=epsg:27700" ) )
plot(limit)
#limit$pcycle <- as.numeric(limit$pcycle)

nat <- raster(resolution = c(10,10), ext = extent(limit), crs= "+init=epsg:27700")
#nat <- raster(x = limit, ext = extent(limit) ,resolution = c(10,10), crs =  "+init=epsg:27700")
#nat <- rasterize(limit,nat, "Id")
plot(nat)

#Inputs
infld <- "D:/results/gender-4p-redo/cluster-1/"

#get files
rasters1 <- list.files(path = infld, pattern = "grd-107", full.names = T)

#Read  create the rasters
rast.list <- list()
for(i in 1:length(rasters1)) { rast.list[i] <- raster(rasters1[i]) }

# And then use do.call on the list of raster objects
rast.list$fun <- sum
rast.mosaic <- do.call(mosaic,rast.list)
plot(rast.mosaic)

r1 <- raster(rasters1[1])
r1 <- projectRaster(r1, crs = "+init=epsg:27700")
r1_snap <- resample(r1, nat, "bilinear")

r2 <- raster(rasters1[2])
r2_ext <- extend(r2,r2,0)
r2_rsp <- resample(r2,r1,"ngb")
testmos <- mosaic(r1,r2)
testmos <- mosaic(r1,r2_rsp,fun=sum)