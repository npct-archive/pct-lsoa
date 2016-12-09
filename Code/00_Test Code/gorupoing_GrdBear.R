#Functions for gropuing routes
#merge in both bearing and grid methods


library(rgeos)
library(sp) 
library(dplyr)
library(utils)
library(stplanr)
library(dplyr)
library(maptools)

lines_master = readRDS("D:/Git/pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus.Rds")
#lines_master = lines_master[1:10000,]
lines_master = lines_master[,"ID"]

names(lines_master) = c("id")
group_no = 1
chunk_size = 10000
grid_size = 5000

grouplines_GrdBear <- function(lines_master, group_no, chunk_size, grid_size){
  #Groups lines quickly by will not find a group for all lines
  #Check if too many lines to do at once
  if(nrow(lines_master) < chunk_size){
    goes = 1
  } else {
    goes = ceiling(nrow(lines_master)/chunk_size)
  }
  #Set up df to store results
  groups_master = data.frame(id_old = as.character(lines_master$id), id_new = as.integer(1:nrow(lines_master)),bearing=as.integer(NA),grid = as.integer(NA) ,group=as.integer(NA))
  #rowsum_master = data.frame(id_new = as.integer(),count=as.integer())
  #Rename IDs so that there are list of intergers
  lines_master$id = groups_master$id_new
  
  # Stage 1: find the bearing for all the lines
  #Do bearings in chunks becuase fails over 10,000
  bearing_res <- data.frame(id_new = as.integer(), bearing = as.integer())
  for(v in 1:goes){
    if((v*chunk_size)>=nrow(lines_master)){l_max = nrow(lines_master)}else{l_max = (v*chunk_size)}
    lines = lines_master[(1 + (v-1)*chunk_size):l_max,]
    bearing <- round(line_bearing(lines),0)
    for(h in 1:length(bearing)){
      if(bearing[h]<=0){bearing[h]=bearing[h]+180}
      if(bearing[h]==0){bearing[h]=180} # first if intorduces some 0 so remove them
    }
    bearing <- data.frame(id_new = as.integer(lines$id), bearing = as.integer(bearing))
    bearing_res <- rbind(bearing_res,bearing)
  }
  groups_master$bearing <- bearing_res$bearing[match(groups_master$id_new, bearing_res$id_new)]
  print(paste0("Bearings found for all lines at ",Sys.time()))
  remove(bearing, bearing_res, lines, goes, v, h, l_max)
  
  # Stage 2: find the grid cell for each line
  lines_trans <- spTransform(lines_master, CRS("+proj=utm +zone=30 +datum=WGS84"))
  points = SpatialLinesMidPoints(lines_trans)
  
  ### define SpatialGrid object
  bb <- bbox(points)
  cs <- c(grid_size, grid_size)
  cc <- bb[, 1] + (cs/2)  # cell offset
  cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd <- SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)), proj4string=CRS(proj4string(points)))
  
  #Assign Grid IDs to Lines
  #groups_master$grid <- as.integer(NA)
  over <- over(points, sp_grd)
  groups_master$grid <- over$id
  remove("lines_trans","over","bb","cs","cc","cd","grd","sp_grd","points")
  #groups_master = groups_master[with(groups_master,order(-count)),]
  #over_summary = data.frame(table(groups_master$grid))
  
  #Stage 3: For each bearing creat an interaction matrix and find the easy groups
  # This loop ignores grids and so its the same as the pure bearing approach
  #for(i in 1:180){
  #  id_list <- groups_master$id_new[groups_master$bearing == i]
  #  lines = lines_master[lines_master$id %in% id_list,]
  #  matrix_master = gIntersects(lines, byid = T)
  #  rownames(matrix_master) = lines$id
  #  colnames(matrix_master) = lines$id
  #  rowsum = data.frame(id_new = rownames(matrix_master),count=as.integer(rowSums(matrix_master)))
  #  rowsum_master = rbind(rowsum_master,rowsum)
  #  twos = as.character(rowsum$id_new[rowsum$count <= 2])
  #  matrix = matrix_master[(rownames(matrix_master) %in% twos),(colnames(matrix_master) %in% twos), drop = F]
  #  rowsum2 = data.frame(id_new = rownames(matrix),count=as.integer(rowSums(matrix)))
  # ones = as.character(rowsum2$id_new[rowsum2$count == 1])
  #  groups_master$group[groups_master$id_new %in% ones] <- group_no
  #  group_no = group_no + 1
  #  
  #}
  #remove("matrix","matrix_master","rowsum","rowsum2","lines","ones","twos")
  #print(paste0("Of ",nrow(groups_master)," lines, ",nrow(groups_master[!is.na(groups_master$group),])," have been assigend a group on the first attempt, at ",Sys.time()))
  
  #Stage 4: Perfom a Grid aware loop of bearings were each bearing has only one route per grid cell
  
  gridtab = as.data.frame(table(groups_master$grid))
  summary(gridtab$Freq)
  #ones = 1
  for(k in 1:20){
    groups = groups_master[is.na(groups_master$group),]
    for(j in 1:180){
      set = groups[groups$bearing == j,]
      set = set[!duplicated(set$grid), ]
      #id_list <- groups$id_new[groups_master$bearing == j]
      lines = lines_master[lines_master$id %in% set$id_new,]
      matrix_master = gIntersects(lines, byid = T)
      rownames(matrix_master) = lines$id
      colnames(matrix_master) = lines$id
      rowsum = data.frame(id_new = rownames(matrix_master),count=as.integer(rowSums(matrix_master)))
      #rowsum_master = rbind(rowsum_master,rowsum)
      twos = as.character(rowsum$id_new[rowsum$count <= 2])
      matrix = matrix_master[(rownames(matrix_master) %in% twos),(colnames(matrix_master) %in% twos), drop = F]
      rowsum2 = data.frame(id_new = rownames(matrix),count=as.integer(rowSums(matrix)))
      ones = as.character(rowsum2$id_new[rowsum2$count == 1])
      groups_master$group[groups_master$id_new %in% ones] <- group_no
      group_no = group_no + 1
      
    }
    print(paste0("Of ",nrow(groups_master)," lines, ",nrow(groups_master[!is.na(groups_master$group),])," have been assigend a group after attempt number ", k+1 ,", at ",Sys.time()))
  }
  
  return(groups_master)
}
