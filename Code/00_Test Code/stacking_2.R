library('raster')
library('rgdal')

################################ MAKE SAMPLE DATA

e1 <- extent(0,10,0,10)
r1 <- raster(e1)
res(r1) <- 0.5
r1[] <- runif(400, min = 0, max = 1)
plot(r1)

e2 <- extent(5,15,5,15)
r2 <- raster(e2)
res(r2) <- 0.5
r2[] <- rnorm(400, 5, 1)
plot(r2)

e3 <- extent(18,40,18,40)
r3 <- raster(e3)
res(r3) <- 0.5
r3[] <- rnorm(1936, 12, 1)
plot(r3)

# Write them out
wdata <- '../pct-lsoa/results/stackoverflow' # your local folder
writeRaster(r1, file.path(wdata, 'r1.tif'), overwrite = TRUE)
writeRaster(r2,file.path(wdata, 'r2.tif'), overwrite = TRUE)
writeRaster(r3,file.path(wdata, 'r3.tif'), overwrite = TRUE)

################################## END MAKE SAMPLE DATA

#Inputs
infld <- "D:/results/gender-4p-redo/cluster-1/"

#get files
rasters1 <- list.files(path = infld, pattern = "grd-107", full.names = T)
ltif <- rasters1



setMethod('mosaic', signature(x='list', y='missing'), 
          function(x, y, fun, tolerance=0.05, filename=""){
            stopifnot(missing(y))
            args <- x
            if (!missing(fun)) args$fun <- fun
            if (!missing(tolerance)) args$tolerance<- tolerance
            if (!missing(filename)) args$filename<- filename
            do.call(mosaic, args)
          })

f.Mosaic <- function(x=x, func = median){
  files <- list.files(file.path(wdata), all.files = F)
  # List  TIF files at wdata folder
  ltif <- grep(".tif$", files, ignore.case = TRUE, value = TRUE) 
  #lext <- list()
  #1rt <- raster(file.path(wdata, i),
  #            package = "raster", varname = fname, dataType = 'FLT4S')
  # Give an extent area here (you can read it from your first tif or define manually)
  uext <- extent(c(0, 100, 0, 100))
  #uext <- extent(union(r1,r3)) get the extent of two rasters
  # Get Total Extent Area
  stkl <- list()
  for(i in 1:length(ltif)){
    x <- raster(file.path(wdata, ltif[i]),
                package = "raster", varname = fname, dataType = 'FLT4S')
    xext <- extent(x)
    uext <- union(uext, xext)
    stkl[[i]] <- x
  }
  # Global Area empty rasterLayer
  rt <- raster(uext)
  res(rt) <- 0.5
  rt[] <- NA
  # Merge each rasterLayer to Global Extent area
  stck <- list()
  for(i in 1:length(stkl)){
    merged.r <- merge(stkl[[i]], rt,  tolerance = 1e+6)
    #merged.r <- reclassify(merged.r, matrix(c(NA, 0), nrow = 1))
    stck[[i]] <- merged.r
  }
  # Mosaic with Median
  mosaic.r <- raster::mosaic(stck, fun = func) # using median
  mosaic.r
}
# Run the function with func = median
mosaiced <- f.Mosaic(x, func = median)
# Plot it
plot(mosaiced)
