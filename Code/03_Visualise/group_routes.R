#Before Rasterising the routes the must be sorted into groups
#such that within a group no routes overlap
#tHis code create those groups from a given input

#1) Create a table of lines and which group they are in
#2) Create a intraction matrix of lines that do and don't overlap
#3a) Outer Loop - Loop thought lines from least overlapping to most overlapping
#3b) Inner Loop - Having idetified a line in Outer loop, loop though all of its 
#non over lapping lines added to the group then checking that remaing don't overlap with each other


#####################
#Ideas

#Pregenerate the matrixes so that the line_master can be removed from memory

#Parallel the process


##########################

#Inputs
lines_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus.Rds")
#lines_master <- lines_master[1:5000,] #test subset

#Parameters
size_limit = 1000
file_name = "rf_nat_4plus_groups" #What output filename should be

#libs
library(rgeos)
#library(raster)
library(sp) 
#library(rgdal)
library(dplyr)
library(utils)

#Code
lines_master@data = subset(lines_master@data, select=c("ID"))

#Simplify IDs
#id2id <- data.frame(id_old=lines_master$id,id_new=1:nrow(lines_master))
groups <-  data.frame(id_old=as.character(lines_master$ID),id_new=1:nrow(lines_master),nrow=as.integer(0)) #create a table of IDs to store which group they should go in
lines_master$ID <- groups$id_new

#Check if too many lines to do at once
if(nrow(lines_master) < size_limit){
  goes = 1
} else {
  goes = ceiling(nrow(lines_master)/size_limit)
}

i_start <- 1 #Sets a starting point for outer loop, loops go though ranges that don't overlap with previous loops

time_start <- Sys.time()
#Loop for when too many lines
#done 1-142
for(v in 1:goes){
  print(paste0("Doing loop ",as.character(v)," of ",as.character(goes)," at ",Sys.time()))
  saveRDS(groups,paste0("../pct-lsoa/Data/03_Intermediate/groups/",file_name,"_dump.Rds"))
  #lines <- lines_master[seq.int(from = v, to = nrow(lines_master), goes),] # Subset evenly across the country
  if((v*size_limit)>=nrow(lines_master)){l_max = nrow(lines_master)}else{l_max = (v*size_limit)}
  lines = lines_master[(1 + (v-1)*size_limit):l_max,]
  matrix_master= gIntersects(lines, byid = T) 
  colnames(matrix_master) <- lines$ID
  rownames(matrix_master) <- lines$ID
  matrix = matrix_master
  #groups =  data.frame(id_new=as.character(lines$ID),nrow=as.integer(0)) #create a table of IDs to store which group they should go in
  pb <- winProgressBar(title="Grouping progress bar", min=0, max=nrow(lines), initial=0, label="0 lines done")
  progress = 0
  #Outer Loop
  for(i in i_start:(i_start + nrow(lines))){ #loop thought every line
    rowsum = data.frame(name=rownames(matrix),count=rowSums(matrix)) 
    if(nrow(matrix) == 0){
      i_start <- i + 1
      break()
    }
    else {
      max1 = max(rowsum$count)
      row1 = as.character(rowsum$name[rowsum$count == max1][1])
      groups[groups$id_new==row1,3] <- i
      #print(paste0("row ",row1," added to group ",i," OL"))
      partners1 = matrix[rownames(matrix) == row1,]
      partners_name1 = names(partners1[partners1 == FALSE])
      matrix = matrix[!(rownames(matrix) %in% row1),!(colnames(matrix) %in% row1), drop = F]
      submatrix = matrix[(rownames(matrix) %in% partners_name1),(colnames(matrix) %in% partners_name1), drop = F]
      #Inner Loop
      
      for(j in 1:nrow(submatrix)){
        if(nrow(submatrix) == 0){
          break()
        }
        else {
          subrowsum = data.frame(name=rownames(submatrix),count=rowSums(submatrix)) #Sum all rows in the sub matrix (this is time consuming)
          max2 = max(subrowsum$count) #find most overlapping line
          row2 = as.character(subrowsum$name[subrowsum$count == max2][1]) #get ID of most overlapping line
          groups[groups$id_new==row2,3] <- i #update the groups table
          #print(paste0("row ",row2," added to group ",i," IL"))
          partners2 = matrix[rownames(matrix) == row2,] #Get jsut the row for this line
          matrix = matrix[!(rownames(matrix) %in% row2),!(colnames(matrix) %in% row2), drop = F] #remove this line from the matrix
          submatrix = submatrix[!(rownames(submatrix) %in% row2),!(colnames(submatrix) %in% row2), drop = F] #remove this line from the submatrix
          partners_name2 = names(partners2[partners2 == FALSE]) #get list of lines that don't overlap with this line
          submatrix = submatrix[(rownames(submatrix) %in% partners_name2),(colnames(submatrix) %in% partners_name2), drop = F] #subset the submatrix to lines that don't overlap with this line
          progress = progress + 1
          info <- sprintf("%d lines done", progress)
          setWinProgressBar(pb, progress, label = info)
        }
      }
    }
    
  }
  close(pb)
}
print(paste0("Completed ",goes," batches of ",size_limit," in ",round(difftime(Sys.time(),time_start, units = "mins")), " minutes"))
saveRDS(groups,paste0("../pct-lsoa/Data/03_Intermediate/groups/",file_name,".Rds"))


