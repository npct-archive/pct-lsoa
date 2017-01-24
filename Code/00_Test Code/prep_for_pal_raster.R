# Reads in the lines and flow data and produces a set of small files that can be distributed across many processes
library(dplyr)
library(sp)
#library(rgeos)
#library(rgdal)
library(maptools)

#Inputs
routes_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_less3p_fix.Rds")   #CHANGE ME
flow_data =  read.csv("../pct-lsoa/Data/02_Input/flow_results_nat_round_170121.csv") #CHANGE ME
outfld <- "../pct-lsoa/Data/03_Intermediate/routes/par_batch/gov-l3/" #CHANGE ME - where results are saved
scenario <- "govtarget_slc_r" #Which scenario do you want to do gendereq_slc_r govtarget_slc_r  dutch_slc_r ebike_slc_r

#Make Out folder
if (!isTRUE(file.info(outfld)$isdir)) dir.create(outfld, recursive=TRUE)

#Prep and Join data
routes_master@data <- left_join(routes_master@data, flow_data, by = c("ID" = "id"))
head(routes_master@data)

#Remove Unneeded data
routes_master@data = routes_master@data[,c("ID",scenario)]  #CHANGE ME #Change for different scenarios
remove(flow_data)
names(routes_master) <- c("ID","bike")
summary(is.na(routes_master$bike))
routes_master <- routes_master[routes_master$bike >0,]
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
names(tab) <- c("grid","count")
tab$grid <- as.integer(as.character(tab$grid))
tab$count <- as.integer(tab$count)
tab <- tab[order(tab$count),]
#write.csv(tab,paste0(outfld,"run_order.csv"))
print(paste0("There are ",nrow(tab), " grids to do"))

#Breakup and save out
tab$sum <- as.integer(0)
tab$sum <- cumsum(tab$count) #sum up the rows as you go
nbatch <- ceiling(max(tab$sum)/70000) #70,000 is aproximatly 24 hours processing

for(i in 1:nbatch){
  print(paste0("Doing batch ",i," of ",nbatch," at ",Sys.time()))
  sum_min <- 1 + 70000 * (i-1)
  sum_max <- if(i * 70000 > max(tab$sum)){max(tab$sum)+1}else{i * 70000}
  tab_sub <- tab[tab$sum < sum_max,]
  tab_sub <- tab_sub[tab_sub$sum >= sum_min,]
  routes <- routes_master[routes_master$grid %in% tab_sub$grid,]
  saveRDS(routes,paste0(outfld,scenario,"-",i,".Rds"))
  
}







