# Reads in interaction matrixes and group
# Asummes that you have pregrouped the lines by bearings
#This method attemps to max out what can be done with the Low Hanging Fuit Method


library(rgeos)
library(sp) 
library(dplyr)
library(utils)


h_end = 1 #for testing set to 180 for full run
#i_start = 1
routes = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus.Rds")
routes_id = routes@data$id
groups_master = data.frame(id_new=as.integer(1:length(routes_id)),id_old=as.character(routes_id),count=as.integer(0), group=as.integer(0))
group_no <- 1


for(h in 1:h_end){
  matrix_master = readRDS(paste0("../pct-lsoa/Data/03_Intermediate/interaction/rf_nat_4plus_IM_",h,".Rds"))
  groups = groups_master[match(rownames(matrix_master), groups_master$id_old),] #Is this needed?
  #groups = data.frame(id_new=as.integer(1:nrow(matrix_master)),id_old=as.character(rownames(matrix_master)),count=as.integer(rowSums(matrix_master)), group=as.integer(0))
  rownames(matrix_master) = groups$id_new
  colnames(matrix_master) = groups$id_new
  matrix = matrix_master
  brk = 0
  
  #Remove the low hanging fuit of large groups that don't overlap much
  for(g in 1:5000){
    if(brk > 2){break()}else{
      #Creat a rowsum
      rowsum = data.frame(id = rownames(matrix),count=as.integer(rowSums(matrix)))
      #List the lines that count == 1
      ones = as.character(rowsum$id[rowsum$count == 1])
      if(length(ones) != 0){
        #Update groups
        groups$group[groups$id_new %in% ones] <- group_no #is this needed?
        groups_master$group[groups_master$id_new %in% ones] <- group_no
        print(paste0("created group ", group_no," with ", nrow(groups[groups$group == group_no,])," members at ",Sys.time()))
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
        matrix = matrix_master
        #try removing all the overlapping lines and check again
        cutoff = 7
        bigs = as.character(rowsum$id[rowsum$count >= cutoff])
        matrix = matrix_master[!(rownames(matrix_master) %in% bigs),!(colnames(matrix_master) %in% bigs), drop = F]
      }}
  }

  print(paste0("Of ",nrow(groups)," routes ",nrow(groups[groups$group > 0,])," were added using the low hanging fruit method, at ",Sys.time()))
  #nogroup = as.character(groups$id_new[groups$group == 0])
  #matrix = matrix_master[(rownames(matrix_master) %in% nogroup),(colnames(matrix_master) %in% nogroup), drop = F]
  #group_no = i_start
  #groups_master <- rbind(groups_master,groups)
  print(paste0("Completed grouping for all lines with a bearing of ",h," at ",Sys.time()))
  print(paste0("There are currently ",max(groups$group)," groups identified"))
  #saveRDS(groups,paste0("../pct-lsoa/Data/03_Intermediate/groups/rf_nat_4plus_groups_fin_alt_",h,".Rds"))
}
saveRDS(groups_master,"../pct-lsoa/Data/03_Intermediate/groups/rf_nat_4plus_groups_LHFmax.Rds")

#About 25% of routes will have been sorted out but some into really small groups
#Psodocode
groups_summary <- as.data.frame(table(groups_master$group))
hist(groups_summary$Freq)
smallgroup <- group_summary$numbers[groups_summary$Freq < 3]
smallgroup <- c(0,smallgroup)
unsorted <- groups_master$id_new[groups_master$group %in% smallgroup]
#remove the group number from small groups
groups_master$group[groups_master$id_new %in% unsorted] <- 0
nrow(groups_master[groups_master$group == 0,])

#loop though  to create new interaction matrix and reapply the LHF method
groups_un_master = groups_master[groups_master$group == 0,] # Subset evenly across the country

size_limit = 5000

if(nrow(groups_un_master) < size_limit){
  goes = 1
} else {
  goes = ceiling(nrow(groups_un_master)/size_limit)
}

routes = routes[routes$ID %in% groups_un_master$id_old,]

for(i in 1:goes){
  groups_un = groups_un_master[seq.int(from = i, to = nrow(groups_un_master), goes),] #Subset across the country
  
}



# LHF method may not work on the subset becasue they are the mostyl overlapping lines
# may be better to stick with current method and redo small groups

# Take in grouping results
# subset out small groups - start with groups of 1
# count the no of lines if <5,000  create a new intreaction matrix
# if more than 5,000 subset to groups of 5,000
# reassing these to (hopefully) fewer groups
# 




