#Before Rasterising the routes the must be sorted into groups
#such that within a group no routes overlap
#tHis code create those groups from a given input

#1) Create a table of lines and which group they are in
#2) Create a intraction matrix of lines that do and don't overlap
#3a) Outer Loop - Loop thought lines from least overlapping to most overlapping
#3b) Inner Loop - Having idetified a line in Outer loop, loop though all of its 
#non over lapping lines added to the group then checking that remaing don't overlap with each other


#Inputs
lines_master = readRDS("../pct-lsoa/Data/03_Intermediate/rq_Cam.Rds")

#Parameters
size_limit = 5000


#libs
#library(rgeos)
#library(raster)
library(sp) 
#library(rgdal)
library(dplyr)
library(utils)

#Code
lines_master@data = subset(lines_master@data, select=c("id"))

#Simplify IDs
id2id <- data.frame(id_old=lines_master$id,id_new=1:nrow(lines_master))
lines_master$id <- id2id$id_new

#Check if too many lines to do at once
if(nrow(lines_master) < size_limit){
  goes = 1
} else {
  goes = ceiling(nrow(lines_master)/size_limit)
}

groups_master =  data.frame(id=as.character(lines_master$id),nrow=as.integer(0)) #create a table of IDs to store which group they should go in

#Loop for when too many lines
for(v in 1:goes){
  print(paste0("Doing loop ",as.character(v)," of ",as.character(goes)," at ",Sys.time()))
  #lines <- lines_master[seq.int(from = v, to = nrow(lines_master), goes),] # Subset evenly across the country
  lines = lines_master[(1 + (v-1)*size_limit):(v*size_limit),]
  matrix_master= gIntersects(lines, byid = T) 
  colnames(matrix_master) <- lines$id
  rownames(matrix_master) <- lines$id
  matrix = matrix_master
  groups =  data.frame(id=as.character(lines$id),nrow=as.integer(0)) #create a table of IDs to store which group they should go in
  pb <- winProgressBar(title="Grouping progress bar", min=0, max=nrow(lines), initial=0, label="0 lines done")
  progress = 0
  #Outer Loop
  for(i in 1:nrow(lines)){ #loop thought every line
    rowsum = data.frame(name=rownames(matrix),count=rowSums(matrix)) 
    if(nrow(matrix) == 0){ 
      break()
    }
    else {
      max1 = max(rowsum$count)
      row1 = as.character(rowsum$name[rowsum$count == max1][1])
      line1 = lines[lines$id == row1,]
      groups[groups$id==line1$id,2] <- i
      partners1 = matrix[row1,]
      partners_name1 = names(partners1[partners1 == FALSE])
      matrix = matrix[!(rownames(matrix) %in% row1),!(colnames(matrix) %in% row1), drop = F]
      submatrix = matrix[(rownames(matrix) %in% partners_name1),(colnames(matrix) %in% partners_name1), drop = F]
      #Inner Loop
      
      for(j in 1:nrow(submatrix)){
        if(nrow(submatrix) == 0){
          break()
        }
        else {
          subrowsum = data.frame(name=rownames(submatrix),count=rowSums(submatrix))
          max2 = max(subrowsum$count)
          row2 = as.character(subrowsum$name[subrowsum$count == max2][1])
          line2 = lines[lines$id == row2,]
          groups[groups$id==line2$id,2] <- i
          partners2 = matrix[row2,]
          matrix = matrix[!(rownames(matrix) %in% row2),!(colnames(matrix) %in% row2), drop = F] 
          submatrix = submatrix[!(rownames(submatrix) %in% row2),!(colnames(submatrix) %in% row2), drop = F] 
          partners_name2 = names(partners2[partners2 == FALSE])
          submatrix = submatrix[(rownames(submatrix) %in% partners_name2),(colnames(submatrix) %in% partners_name2), drop = F]
          progress = progress + 1
          info <- sprintf("%d lines done", progress)
          setWinProgressBar(pb, progress, label = info)
        }
      }
    }
    
  }
  close(pb)
  #####################
  # Need to work out how to make the groups_master from the groups file
  ###########
  #groups_master$nrow <- 
}

groups$id <- as.character(groups$id)
id2id$id_old <- as.character(id2id$id_old)
foo <- left_join(groups, id2id, by.x = "id", by.y = "id_new")
saveRDS(foo,paste0("../pct-lsoa/Data/03_Intermediate/groups/groups.Rds"))
#Rename IDs

