# Modified from
# https://github.com/cran/gdalUtils/blob/master/R/gdal_rasterize.R
# Adds the -add function that is supported by gdal_rasterize but missing from gdalUtils


gdal_rasterize_malcolm <- function(
  src_datasource,dst_filename,
  b,i,at,burn,a,threeD,add,l,where,sql,dialect,
  of,a_srs,co,a_nodata,init,
  te,tr,tap,ts,ot,q,
  #		additional_commands,
  output_Raster=FALSE,
  ignore.full_scan=TRUE,
  verbose=FALSE)
{
  if(output_Raster && (!requireNamespace("raster") || !requireNamespace("rgdal")))
  {
    warning("rgdal and/or raster not installed. Please install.packages(c('rgdal','raster')) or set output_Raster=FALSE")
    return(NULL)
  }
  
  parameter_values <- as.list(environment())
  
  if(verbose) message("Checking gdal_installation...")
  gdal_setInstallation(ignore.full_scan=ignore.full_scan)
  if(is.null(getOption("gdalUtils_gdalPath"))) return()
  
  # Place all gdal function variables into these groupings:
  parameter_variables <- list(
    logical = list(
      varnames <- c(
        "i","at","threeD","add","tap","q"
      )),
    vector = list(
      varnames <- c(
        "init","te","tr","ts"	
      )),
    scalar = list(
      varnames <- c(
        "a_nodata"
      )),
    character = list(
      varnames <- c(
        "a","where","sql","dialect","of","a_srs","ot",
        "src_datasource","dst_filename"
      )),
    repeatable = list(
      varnames <- c(
        "b","burn","l","co"
      ))
  )
  
  parameter_order <- c(
    "b","i","at","burn","a","threeD","add","l",
    "where","sql","dialect",
    "of","a_srs","co","a_nodata","init",
    "te","tr","tap","ts","ot","q",
    "src_datasource","dst_filename"
  )
  
  parameter_noflags <- c("src_datasource","dst_filename")
  
  parameter_noquotes <- unlist(parameter_variables$vector)
  
  executable <- "gdal_rasterize"
  
  cmd <- gdal_cmd_builder(
    executable=executable,
    parameter_variables=parameter_variables,
    parameter_values=parameter_values,
    parameter_order=parameter_order,
    parameter_noflags=parameter_noflags,
    parameter_noquotes=parameter_noquotes,
    gdal_installation_id=gdal_chooseInstallation(hasDrivers=of))
  
  if(verbose) message(paste("GDAL command being used:",cmd))
  
  cmd_output <- system(cmd,intern=TRUE) 
  
  if(output_Raster)
  {
    return(brick(dst_filename))	
  } else
  {
    return(NULL)
  }		
}