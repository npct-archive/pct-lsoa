# Reads in interaction matrixes and group
# Asummes that you have pregrouped the lines by bearings

library(rgeos)
library(sp) 
library(dplyr)
library(utils)


h_end = 180 #for testing set to 180 for full run
#i_start = 1
groups_master = data.frame(id_new=as.integer(),id_old=as.character(),count=as.integer(), group=as.integer())
group_no <- 1


for(h in 1:h_end){
  matrix_master = readRDS(paste0("../pct-lsoa/Data/03_Intermediate/interaction/rf_nat_4plus_IM_",h,".Rds"))
  groups = data.frame(id_new=as.integer(1:nrow(matrix_master)),id_old=as.character(rownames(matrix_master)),count=as.integer(rowSums(matrix_master)), group=as.integer(0))
  rownames(matrix_master) = groups$id_new
  colnames(matrix_master) = groups$id_new
  matrix = matrix_master
  brk = 0
  
  #Remove the low hanging fuit of large groups that don't overlap much
  for(g in 1:5000){
    if(brk > 5){break()}else{
      #Creat a rowsum
      rowsum = data.frame(id = rownames(matrix),count=as.integer(rowSums(matrix)))
      #List the lines that count == 1
      ones = as.character(rowsum$id[rowsum$count == 1])
      if(length(ones) != 0){
        #Update groups
        groups$group[groups$id_new %in% ones] <- group_no
        #print(paste0("created group ", group_no," with ", nrow(groups[groups$group == group_no,])," members at ",Sys.time()))
        if(brk > 0){brk <- brk - 1}
        #print(paste0("break = ",brk))
        group_no = group_no + 1
        #Remove all the ones from the master matrix
        matrix_master = matrix_master[!(rownames(matrix_master) %in% ones),!(colnames(matrix_master) %in% ones), drop = F]
        #Make a new matrix of the twos
        rowsum2 = data.frame(id = rownames(matrix_master),count=as.integer(rowSums(matrix_master)))
        twos = as.character(rowsum$id[rowsum$count == 2])
        matrix = matrix_master[(rownames(matrix_master) %in% twos),(colnames(matrix_master) %in% twos), drop = F]
      }else{
        #Run out of non-overlapping routes
        brk <- brk + 1
        #print(paste0("ran out of non-overlapping lines trying a subset, attempt = ",brk))
        #matrix = matrix_master
        #try removing all the overlapping lines and check again
        cutoff = 7
        bigs = as.character(rowsum$id[rowsum$count >= cutoff])
        matrix = matrix_master[!(rownames(matrix_master) %in% bigs),!(colnames(matrix_master) %in% bigs), drop = F]
      }}
  }

  print(paste0("Of ",nrow(groups)," routes ",nrow(groups[groups$group > 0,])," were added using the low hanging fruit method, at ",Sys.time()))
  nogroup = as.character(groups$id_new[groups$group == 0])
  matrix = matrix_master[(rownames(matrix_master) %in% nogroup),(colnames(matrix_master) %in% nogroup), drop = F]
  #no do the bits that are left  
  pb <- winProgressBar(title="Grouping progress bar", min=0, max=nrow(matrix_master), initial=0, label="0 lines done")
  progress = 0
  i_start = group_no
  
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
    groups$group[groups$id_new == row1] <- i
    #groups[groups$id_new==row1,4] <- i
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
        groups$group[groups$id_new == row2] <- i
        #groups[groups$id_new==row2,4] <- i #update the groups table
        #print(paste0("row ",row2," added to group ",i," Inner Loop"))
        partners2 = matrix[rownames(matrix) == row2,] #Get jsut the row for this line
        matrix = matrix[!(rownames(matrix) %in% row2),!(colnames(matrix) %in% row2), drop = F] #remove this line from the matrix
        submatrix = submatrix[!(rownames(submatrix)rf_ %in% row2),!(colnames(submatrix) %in% row2), drop = F] #remove this line from the submatrix
        partners_name2 = names(partners2[partners2 == FALSE]) #get list of lines that don't overlap with this line
        submatrix = submatrix[(rownames(submatrix) %in% partners_name2),(colnames(submatrix) %in% partners_name2), drop = F] #subset the submatrix to lines that don't overlap with this line
        progress = progress + 1
        info <- sprintf("%d lines done", progress)
        setWinProgressBar(pb, progress, label = info)
      }
    }
  } 
  #print(paste0("An outer loop has been completed ",nrow(groups[groups$group == (i),])," routes were added to group ",(i)," at ",Sys.time()))
}
  close(pb)
  group_no = i_start
  groups_master <- rbind(groups_master,groups)
  print(paste0("Completed grouping for all lines with a bearing of ",h," at ",Sys.time()))
  print(paste0("There are currently ",max(groups$group)," groups identified"))
  saveRDS(groups,paste0("../pct-lsoa/Data/03_Intermediate/groups/rf_nat_4plus_groups_fin_alt_",h,".Rds"))
}
saveRDS(groups_master,"../pct-lsoa/Data/03_Intermediate/groups/rf_nat_4plus_groups_fin_alt.Rds")
