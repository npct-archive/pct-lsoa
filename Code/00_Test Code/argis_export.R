#ArcGIS Export
#Convert routes to a raster based on the number of cyclists
#WHen rasterizing it is necessary to not have overlapping lines

#4) Loop though each group convert to raster then add up rasters

#libs
library(sp)
library(rgeos)
library(velox)
library(raster)
library(dplyr)
library(rgdal)

#Inputs
routes_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus_fix.Rds")
#groups_master = readRDS("../pct-lsoa/Data/03_Intermediate/groups/rf_nat_4plus_region1_finished2.Rds")
flow_data = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")

#Parameters
resolution = 20 #Equal to OSM zoom level 13

#Prep and Join data
#groups_master$id_old <- as.character(groups_master$id_old)
#groups_master <- groups_master[!is.na(groups_master$group),]
flow_data <- flow_data[,c("id","bicycle_16p")]
#flow_data <- flow_data[flow_data$id %in% groups_master$id_old,]
#routes_master <- routes_master[routes_master$ID %in% groups_master$id_old,]
#flow_data <- left_join(flow_data,groups_master, by = c("id" = "id_old"))
routes_master@data <- left_join(routes_master@data, flow_data, by = c("ID" = "id"))

#Remove Unneeded data
routes_master@data = routes_master@data[,c("ID","bicycle_16p")] #,"group")]
#routes_master@data = routes_master@data[,c("ID","bicycle_16p","group")]
remove(flow_data,groups_master)
routes_master = routes_master[routes_master$bicycle_16p >0,]

#group_list = as.data.frame(table(routes_master$group))
#group_list$Var1 = as.integer(group_list$Var1)


routes_master <- spTransform(routes_master, CRS( "+init=epsg:27700" ) )
routes_poly <- gBuffer(routes_master, byid = T, width = 10) 
#routes_poly <- gSimplify(routes_poly, tol = 0.1, topologyPreserve=T)
writeOGR(routes_poly,"../pct-lsoa/Data/03_Intermediate/routes","rf_nat_4plus_fix","ESRI Shapefile")
writeOGR(routes_master,"../pct-lsoa/Data/03_Intermediate/routes","rf_nat_4plus_fix_lines","ESRI Shapefile")









raster_master <- raster(resolution = resolution, ext = extent(routes_master), crs= "+init=epsg:27700", vals = 0)
dataType(raster_master) <- "INT2U"
vx <- velox(raster_master, extent=extent(routes_master), res=c(resolution,resolution), crs="+init=epsg:27700")
remove(raster_master)

#loop though each group and rasterize
print(paste0("There are ",nrow(group_list)," groups to do, getting started at ",Sys.time() ))
for(k in group_list$Var1 ){  #max(groups[,2])
  lines2raster = routes_poly[routes_poly$group == k ,]
  vx_sub <- vx
  vx_sub$rasterize(lines2raster, field="bicycle_16p", band=1, background = 0)
  vx_sub$write(path = paste0("../pct-lsoa/Data/03_Intermediate/temp/rasters/rf-nat-4plus-region1-",k,".tif"))
  print(paste0("Done group ",k," at ",Sys.time() ))
}

