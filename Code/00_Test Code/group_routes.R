# Reads in interaction matrixes and group
# Asummes that you have pregrouped the lines by bearings

library(rgeos)
library(sp) 
library(dplyr)
library(utils)


h_end = 180 #for testing set to 180 for full run
i_start = 1
groups_master = data.frame(id_new=as.integer(),id_old=as.character(),count=as.integer(), group=as.integer())

for(h in 1:h_end){
  matrix_master = readRDS(paste0("../pct-lsoa/Data/03_Intermediate/interaction/rf_nat_4plus_IM_",h,".Rds"))
  groups = data.frame(id_new=as.integer(1:nrow(matrix_master)),id_old=as.character(rownames(matrix_master)),count=as.integer(rowSums(matrix_master)), group=as.integer(0))
  rownames(matrix_master) = groups$id_new
  colnames(matrix_master) = groups$id_new
  #Add all the non interacting into a big group
  groups[groups$count==1,4] <- i_start
  i_start = i_start + 1
  length(groups$group[groups$group == 1])
  matrix = matrix_master
  row0 = as.character(groups$id_new[groups$group == 1])
  matrix = matrix[!(rownames(matrix) %in% row0),!(colnames(matrix) %in% row0), drop = F] #remove all the group 1 routes
  pb <- winProgressBar(title="Grouping progress bar", min=0, max=nrow(matrix_master), initial=0, label="0 lines done")
  progress = 0
  
#Outer Loop
for(i in i_start:(i_start + nrow(matrix))){ #loop thought every line
  rowsum = data.frame(name=rownames(matrix),count=rowSums(matrix)) 
  if(nrow(matrix) == 0){
    i_start <- i + 1
    break()
  }
  else {
    max1 = max(rowsum$count)
    row1 = as.character(rowsum$name[rowsum$count == max1][1])
    groups[groups$id_new==row1,4] <- i
    #print(paste0("row ",row1," added to group ",i," Outer Loop"))
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
        groups[groups$id_new==row2,4] <- i #update the groups table
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
  print(paste0("Completed grouping for all lines with a bearing of ",h," at", Sys.time()))
  print(paste0("There are currently ",max(groups$group)," groups identified"))
}
saveRDS(groups,"../pct-lsoa/Data/03_Intermediate/groups/rf_nat_4plus_groups_fin.Rds")
