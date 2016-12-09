#Route routes using the groupsin functions

#Parameters
name = "rf_nat_4plus_a4" #Output file name


#Inputs
routes = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus.Rds")
flow_data = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")

#Prep and Join data
flow_data = flow_data[,c("id","bicycle_16p")]
flow_data = flow_data[flow_data$bicycle_16p > 0,]
routes = routes[routes$ID %in% flow_data$id,]
routes@data = left_join(routes@data, flow_data, by = c("ID" = "id"))
routes@data = routes@data[,c("ID","bicycle_16p")]
names(routes@data) = c("id","bike")
remove(flow_data)

#Start Grouping
source("../pct-lsoa-test/R/grouping_functions.R")

#Use Grid/Bearing Approach with a 1k grid
stage = 1
groups_1 = grouplines_GrdBear(routes, group_no = 1, grid_size = 1000, attempts = 2)
unfin = groups_1$id_old[is.na(groups_1$group)]
routes_unfin = routes[routes$id %in% unfin,]
nxtgrp = max(groups_1$group[!is.na(groups_1$group)]) + 1
groups_1 = groups_1[!is.na(groups_1$group),]
groups_1 = groups_1[,c("id_old","id_new","group")]
#saveRDS(groups_1,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1
#remove(groups_1)

#Use Grid/Bearing Approach with a 10k grid
groups_2 = grouplines_GrdBear(routes_unfin, group_no = nxtgrp, grid_size = 10000, attempts = 20)
unfin = groups_2$id_old[is.na(groups_2$group)]
routes_unfin = routes[routes$id %in% unfin,]
nxtgrp = max(groups_2$group[!is.na(groups_2$group)]) + 1
groups_2 = groups_2[!is.na(groups_2$group),]
groups_2 = groups_2[,c("id_old","id_new","group")]
#saveRDS(groups_2,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Use Grid Appraoch with a 20k grid
groups_3 = grouplines_grid(routes_unfin,nxtgrp,20000)
unfin = groups_3$id_old[is.na(groups_3$group)]
routes_unfin = routes[routes$id %in% unfin,]
nxtgrp = max(groups_3$group[!is.na(groups_3$group)]) + 1
groups_3 = groups_3[!is.na(groups_3$group),]
groups_3 = groups_3[,c("id_old","id_new","group")]
#saveRDS(groups_3,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Use complete approach with small batch sizes
groups_4 <- grouplines_complete(routes_unfin, nxtgrp, 5000)
summary(is.na(groups_4$nrow)) #Should be no ungrouped lines
nxtgrp = max(groups_4$nrow[!is.na(groups_4$nrow)]) + 1
groups_4 = groups_4[!is.na(groups_4$nrow),]
names(groups_4) = c("id_new","group","id_old")
groups_4 = groups_4[,c(3,1,2)]
#saveRDS(groups_4,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#clean up
groups_all = rbind(groups_1,groups_2,groups_3,groups_4)
saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
remove(groups_1,groups_2,groups_3,groups_4)

#We have now grouped all the lines but many lines will have been grouped poorly
groups_summary = as.data.frame(table(groups_all$group))
groups_summary$Var1 = as.integer(groups_summary$Var1)
#Work out how many lines are poorly grouped
freq_summary = data.frame(freq = unique(groups_summary$Freq), sum = as.integer(NA))
for(j in nrow(freq_summary)){
  freq_summary$sum[j] = sum(groups_summary$Freq[groups_summary$Freq == freq_summary$freq[j]])
}








smallgroups = groups_summary$Var1[groups_summary$Freq < 5]
regroup = as.character(groups_all$id_old[groups_all$group %in% smallgroups])
routes_regroup = routes[routes$id %in% regroup,]

#Use complete approach with small batch sizes
groups_5 <- grouplines_complete(routes_regroup, nxtgrp, 5000)
summary(is.na(groups_5$nrow)) #Should be no ungrouped lines
nxtgrp = max(groups_5$nrow[!is.na(groups_5$nrow)]) + 1
groups_5 = groups_5[!is.na(groups_5$nrow),]
names(groups_5) = c("id_new","group","id_old")
groups_5 = groups_5[,c(3,1,2)]
saveRDS(groups_5,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Update Groups_all
groups_all$id_old = as.character(groups_all$id_old )
groups_5$id_old = as.character(groups_5$id_old )

for(i in 1:nrow(groups_5)){
  groups_all$group[groups_all$id_old == groups_5$id_old[i]] <- groups_5$group[i]
}

saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

length(unique(groups_all$group))

#Do that all again

#remove(groups_1,groups_2,groups_3,groups_4)
groups_summary = as.data.frame(table(groups_all$group))
groups_summary$Var1 = as.integer(groups_summary$Var1)
smallgroups = groups_summary$Var1[groups_summary$Freq < 10] #This time for less than 10
regroup = as.character(groups_all$id_old[groups_all$group %in% smallgroups])
routes_regroup = routes[routes$id %in% regroup,]

#Use complete approach with small batch sizes
groups_6 <- grouplines_complete(routes_regroup, nxtgrp, 5000)
summary(is.na(groups_6$nrow)) #Should be no ungrouped lines
nxtgrp = max(groups_6$nrow[!is.na(groups_6$nrow)]) + 1
groups_6 = groups_6[!is.na(groups_6$nrow),]
names(groups_6) = c("id_new","group","id_old")
groups_6 = groups_6[,c(3,1,2)]
saveRDS(groups_6,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Update Groups_all
groups_all$id_old = as.character(groups_all$id_old )
groups_6$id_old = as.character(groups_6$id_old )

for(i in 1:nrow(groups_6)){
  groups_all$group[groups_all$id_old == groups_6$id_old[i]] <- groups_6$group[i]
}

saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

length(unique(groups_all$group))

#Do that all again

#remove(groups_1,groups_2,groups_3,groups_4)
groups_summary = as.data.frame(table(groups_all$group))
groups_summary$Var1 = as.integer(groups_summary$Var1)
smallgroups = groups_summary$Var1[groups_summary$Freq < 10] #This time for less than 10
regroup = as.character(groups_all$id_old[groups_all$group %in% smallgroups])
routes_regroup = routes[routes$id %in% regroup,]

#Use complete approach with small batch sizes
groups_7 <- grouplines_complete(routes_regroup, nxtgrp, 5000)
summary(is.na(groups_7$nrow)) #Should be no ungrouped lines
nxtgrp = max(groups_7$nrow[!is.na(groups_7$nrow)]) + 1
groups_7 = groups_7[!is.na(groups_7$nrow),]
names(groups_7) = c("id_new","group","id_old")
groups_7 = groups_7[,c(3,1,2)]
saveRDS(groups_7,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Update Groups_all
groups_all$id_old = as.character(groups_all$id_old )
groups_7$id_old = as.character(groups_7$id_old )

for(i in 1:nrow(groups_7)){
  groups_all$group[groups_all$id_old == groups_7$id_old[i]] <- groups_7$group[i]
}

saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

length(unique(groups_all$group))
hist(groups_summary$Freq, breaks = 1:810)


#Do that all again

#remove(groups_1,groups_2,groups_3,groups_4)
groups_summary = as.data.frame(table(groups_all$group))
groups_summary$Var1 = as.integer(groups_summary$Var1)
smallgroups = groups_summary$Var1[groups_summary$Freq < 20] #This time for less than 20
regroup = as.character(groups_all$id_old[groups_all$group %in% smallgroups])
routes_regroup = routes[routes$id %in% regroup,]

#Use complete approach with small batch sizes
groups_8 <- grouplines_complete(routes_regroup, nxtgrp, 5000)
summary(is.na(groups_8$nrow)) #Should be no ungrouped lines
nxtgrp = max(groups_8$nrow[!is.na(groups_8$nrow)]) + 1
groups_8 = groups_8[!is.na(groups_8$nrow),]
names(groups_8) = c("id_new","group","id_old")
groups_8 = groups_8[,c(3,1,2)]
saveRDS(groups_8,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Update Groups_all
groups_all$id_old = as.character(groups_all$id_old )
groups_8$id_old = as.character(groups_8$id_old )

for(i in 1:nrow(groups_8)){
  groups_all$group[groups_all$id_old == groups_8$id_old[i]] <- groups_8$group[i]
}

saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

length(unique(groups_all$group))
hist(groups_summary$Freq, breaks = 1:810)

#Do that all again

#remove(groups_1,groups_2,groups_3,groups_4)
groups_summary = as.data.frame(table(groups_all$group))
groups_summary$Var1 = as.integer(groups_summary$Var1)
smallgroups = groups_summary$Var1[groups_summary$Freq < 50] #This time for less than 20
regroup = as.character(groups_all$id_old[groups_all$group %in% smallgroups])
routes_regroup = routes[routes$id %in% regroup,]

#Use complete approach with small batch sizes
groups_9 <- grouplines_complete(routes_regroup, nxtgrp, 5000)
summary(is.na(groups_9$nrow)) #Should be no ungrouped lines
nxtgrp = max(groups_9$nrow[!is.na(groups_9$nrow)]) + 1
groups_9 = groups_9[!is.na(groups_9$nrow),]
names(groups_9) = c("id_new","group","id_old")
groups_9 = groups_9[,c(3,1,2)]
saveRDS(groups_9,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Update Groups_all
groups_all$id_old = as.character(groups_all$id_old )
groups_9$id_old = as.character(groups_9$id_old )

for(i in 1:nrow(groups_9)){
  groups_all$group[groups_all$id_old == groups_9$id_old[i]] <- groups_9$group[i]
}

saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

length(unique(groups_all$group))
hist(groups_summary$Freq, breaks = 1:810)
nrow(groups_summary[groups_summary$Freq < 20,])
sum(groups_summary$Freq[groups_summary$Freq < 20])

#Do that all again

#remove(groups_1,groups_2,groups_3,groups_4)
groups_summary = as.data.frame(table(groups_all$group))
groups_summary$Var1 = as.integer(groups_summary$Var1)
smallgroups = groups_summary$Var1[groups_summary$Freq < 50] #This time for less than 20
regroup = as.character(groups_all$id_old[groups_all$group %in% smallgroups])
routes_regroup = routes[routes$id %in% regroup,]

#Use complete approach with small batch sizes
groups_10 <- grouplines_complete(routes_regroup, nxtgrp, 5000)
summary(is.na(groups_10$nrow)) #Should be no ungrouped lines
nxtgrp = max(groups_10$nrow[!is.na(groups_10$nrow)]) + 1
groups_10 = groups_10[!is.na(groups_10$nrow),]
names(groups_10) = c("id_new","group","id_old")
groups_10 = groups_10[,c(3,1,2)]
saveRDS(groups_10,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Update Groups_all
groups_all$id_old = as.character(groups_all$id_old )
groups_10$id_old = as.character(groups_10$id_old )

for(i in 1:nrow(groups_10)){
  groups_all$group[groups_all$id_old == groups_10$id_old[i]] <- groups_10$group[i]
}

saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

length(unique(groups_all$group))
hist(groups_summary$Freq, breaks = 1:810)
nrow(groups_summary[groups_summary$Freq < 20,])
sum(groups_summary$Freq[groups_summary$Freq < 20])

#Do that all again



#remove(groups_1,groups_2,groups_3,groups_4)
groups_summary = as.data.frame(table(groups_all$group))
groups_summary$Var1 = as.integer(groups_summary$Var1)
smallgroups = groups_summary$Var1[groups_summary$Freq < 2] #This time for less than 20
regroup = as.character(groups_all$id_old[groups_all$group %in% smallgroups])
routes_regroup = routes[routes$id %in% regroup,]

#Use complete approach with small batch sizes
groups_11 <- grouplines_complete(routes_regroup, nxtgrp, 5000)
summary(is.na(groups_11$nrow)) #Should be no ungrouped lines
nxtgrp = max(groups_11$nrow[!is.na(groups_11$nrow)]) + 1
groups_11 = groups_11[!is.na(groups_11$nrow),]
names(groups_11) = c("id_new","group","id_old")
groups_11 = groups_11[,c(3,1,2)]
saveRDS(groups_11,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Update Groups_all
groups_all$id_old = as.character(groups_all$id_old )
groups_11$id_old = as.character(groups_11$id_old )

for(i in 1:nrow(groups_11)){
  groups_all$group[groups_all$id_old == groups_11$id_old[i]] <- groups_11$group[i]
}

saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

length(unique(groups_all$group))
hist(groups_summary$Freq, breaks = 1:900)
nrow(groups_summary[groups_summary$Freq < 20,])
sum(groups_summary$Freq[groups_summary$Freq < 20])


