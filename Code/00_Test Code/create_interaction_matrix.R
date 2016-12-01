#Create interation matrixes

#for each bearing group create an interaction matrix

library(rgeos)


#inputs

groups_master = readRDS("../pct-lsoa/Data/03_Intermediate/groups/l_nat_4plus_Bgrp.Rds")
routes_master = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus.Rds")

#Drop unneeded data
groups_master = groups_master[,c("id","bearing")]
routes_master = routes_master[,c("ID")]

print("Starting the loops")
for(i in 1:180){
  groups = groups_master[groups_master$bearing == i,]
  routes = routes_master[routes_master$ID %in% groups$id,]
  nrow(routes)
  matrix= gIntersects(routes, byid = T)
  colnames(matrix) <- routes$ID
  rownames(matrix) <- routes$ID
  saveRDS(matrix,paste0("../pct-lsoa/Data/03_Intermediate/interaction/rf_nat_4plus_IM_",i,".Rds"))
  print(paste0("Done Matrix ",i," of 180"))
}
